module sbylib.math.quaternion;

import sbylib.math.vector;
import sbylib.math.matrix;
import sbylib.math.angle;
import std.conv : to;
import std.format : format;

alias quat = Quaternion!float;

/**
Quaternion type
*/
struct Quaternion(T)
if (__traits(isArithmetic, T)) 
{
    /**
    Components of quaternion.

    q = ix + jy + kz + w
    */
    T x, y, z, w;

    /**
    Constructor from scalar components
    */
    this(T x, T y, T z, T w) {
        this.x = x;
        this.y = y;
        this.z = z;
        this.w = w;
    }

    /**
    Constructor from imaginary vector and real part
    */
    this(Vector!(T,3) v, T w) {
        this(v[0],v[1],v[2],w);
    }

    /**
    Get the x-axis basis vector of the transformation of this quaternion

    Returns: x basis vector
    */
    Vector!(T,3) baseX() const {
        return Vector!(T,3)
            (1-2*(y*y+z*z),
             2*(x*y+w*z),
             2*(x*z-w*y));
    }

    /**
    Get the y-axis basis vector of the transformation of this quaternion

    Returns: y basis vector
    */
    Vector!(T,3) baseY() const {
        return Vector!(T,3)
            (2*(x*y-z*w),
             1-2*(x*x+z*z),
             2*(y*z+w*x));
    }

    /**
    Get the z-axis basis vector of the transformation of this quaternion

    Returns: z basis vector
    */
    Vector!(T,3) baseZ() const {
        return Vector!(T,3)
            (2*(x*z+w*y),
             2*(y*z-w*x),
             1-2*(x*x+y*y));
    }

    /**
    Unary operation for "+".

    this is identity function

    Returns: a quaternionly which is equal to this quaternion
    */
    Quaternion!T opUnary(string op)() const 
    if (op == "+")
    {
        return this;
    }

    /**
    Unary operation for "-".

    Returns: a quaternion whose sign is inverted
    */
    Quaternion!T opUnary(string op)() const 
    if (op == "-")
    {
        return Quaternion!T(-x,-y,-z,-w);
    }

    /**
    Unary operation for "~".
    This means conjugation.

    Returns: a quaternion conjugate with this quaternion
    */
    Quaternion!T opUnary(string op)() const
    if (op == "~")
    {
        return Quaternion!T(-x,-y,-z,+w);
    }

    /**
    Binary operation between quaternion for "+" or "-"

    Params:
        q = target quaternion

    Returns: calculated quaternion
    */
    Quaternion!T opBinary(string op)(Quaternion!T q) const 
    if (op == "+" || op == "-")
    {
        Quaternion!T result;
        static foreach (i; 0..4) {
            result[i] = mixin(format!q{
                this[i] %s q
            }(op));
        }
    }

    /**
    Binary operation between quaternion for "*"

    Params:
        q = target quaternion

    Returns: calculated quaternion
    */
    Quaternion!T opBinary(string op)(Quaternion!T q) const 
    if (op == "*")
    {
        Quaternion!T result;
        result.w = w * q.w - x * q.x - y * q.y - z * q.z;
        result.x = w * q.x + x * q.w + y * q.z - z * q.y;
        result.y = w * q.y - x * q.z + y * q.w + z * q.x;
        result.z = w * q.z + x * q.y - y * q.x + z * q.w;
        return result;
    }

    /**
    Binary operation between scalar type for "+", "-", "*" or "/".

    Params:
        e = target scalar value

    Returns: calculated quaternion
    */
    Quaternion!T opBinary(string op)(T e) const
    if (op == "+" || op == "-" || op == "*" || op == "/")
    {
        Quaternion!T result;
        static foreach (i; 0..4) {
            result[i] = mixin(format!q{
                this[i] %s e
            }(op));
        }
        return result;
    }

    /**
    Operator assign for quaternion.
    This function supports "+" or "-" operation.

    Params:
        q = target quaternion
    */
    void opOpAssign(string op)(Quaternion q) 
    if (op == "+" || op == "-")
    {
        static foreach (i; 0..4) {
            mixin(format!q{
                this[i] %s= q[i];
            }(op));
        }
    }

    /**
    Operator assign for quaternion.
    This function supports "*" operation.

    Params:
        q = target quaternion
    */
    void opOpAssign(string op)(Quaternion q) 
    if (op == "*")
    {
        Quaternion result = this * q;
        static foreach (i; 0..4) {
            this[i] = result[i];
        }
    }

    /**
    Operator assign for scalar type.
    This function supports "+", "-", "*" or "/" operation.

    Params:
        q = target quaternion
    */
    void opOpAssign(string op)(T e) 
    if (op == "+" || op == "-" || op == "*" || op == "/")
    {
        static foreach (i; 0..4) {
            mixin(format!q{
                this[i] %s= e;
            });
        }
    }

    /**
    Indexing operation.

    Returns: i-th element of this quaternion
    */
    ref T opIndex(size_t i) 
    in (i < 4)
    {
        final switch (i) {
        case 0: return x;
        case 1: return y;
        case 2: return z;
        case 3: return w;
        }
    }

    string toString() const {
        return format!"q(%s, %s, %s, %s)"(x, y, z, w);
    }

    Vector!(T,3) axis() @property const {
        return Vector!(T,3)(x,y,z);
    }

    void axis(Vector!(T,3) axis) @property {
        this.x = axis[0];
        this.y = axis[1];
        this.z = axis[2];
    }

    /**
    Returns rotation quaternion decided by its rotation axis and angle.

    Params:
        axis = rotation axis vector, which must be normalized
        angle = rotation angle

    Returns: rotation quaternion
    */
    static Quaternion!T axisAngle(Vector!(T,3) axis, Angle rad) {
        return Quaternion(axis * sin(rad/2), cos(rad/2));
    }

    /**
    Returns rotation quaternion which converts a vector to another vector.

    Params:
        from = before vector, which must be normalized
        to = after vector, which must be normalized

    Returns: rotation quaternion
    */
    static Quaternion!T rotFromTo(Vector!(T,3) from, Vector!(T,3) to) {
        import sbylib.math.vector : vnormalize = normalize;
        import std.math : sgn;

        const c = dot(from, to);
        const v = vnormalize(cross(from, to));
        if (v.hasNaN) {
            return Quaternion!T(0,0,0,sgn(dot(from,to)));
        }
        return axisAngle(v, c.acos);
    }
}

/**
Returns length of a quaternion.

Params:
    q = target quaternion

Returns: length of the target quaternion
*/
T length(T)(Quaternion!T q) {
    import std.math : sqrt;
    T sum = 0;
    static foreach (i; 0..4) {
        sum += q[i] * q[i];
    }
    return sqrt(sum);
}

/**
Returns a quaternion whose length 1 from target quaternion.

Params:
    q = target quaternion

Returns: normalized target quaternion
*/
Quaternion!T normalize(T)(Quaternion!T q) {
    return q / length(q);
}

/**
Rotate a vector by a quaternion.

Params:
    vec = rotation target vector
    q = rotation quaternion

Returns: rotated vector
*/
Vector!(T,3) rotate(T)(Vector!(T,3) vec, Quaternion!(T) q) {
    return (q * Quaternion!T(vec, 0) * ~q).Axis;
}

/**
Converts a quaternion into matrix 3x3.

Params:
    q = target quaternion

Returns: converted matrix
*/
Matrix!(T,3) toMatrix3(T)(Quaternion!T q) {
    with (q) {
        return Matrix!(T,3)(
                1-2*(y*y+z*z), 2*(x*y-z*w), 2*(x*z+w*y),
                2*(x*y+w*z), 1-2*(x*x+z*z), 2*(y*z-w*x),
                2*(x*z-w*y), 2*(y*z+w*x), 1-2*(x*x+y*y));
    }
}

/**
Converts a quaternion into matrix 4x4.

Params:
    q = target quaternion

Returns: converted matrix
*/
Matrix!(T,4) toMatrix4(T)(Quaternion!T q) {
    return q.toMatrix3().toMatrix4();
}
