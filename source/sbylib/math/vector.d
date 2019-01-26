module sbylib.math.vector;

import std.traits;
import std.algorithm;

alias vec2  = Vector!(float, 2);
alias vec3  = Vector!(float, 3);
alias vec4  = Vector!(float, 4);
alias vec2i = Vector!(int,   2);
alias vec3i = Vector!(int,   3);
alias vec4i = Vector!(int,   4);

enum isVector(T) = isInstanceOf!(Vector, T);

/**
Vector type
*/
struct Vector(T, uint S) 
if (__traits(isArithmetic, T)) 
{

    private T[S] elements;

    /**
    Dimension of this vector type
    */
    enum Dimension = S;

    /**
    Element type of this vector type
    */
    alias ElementType = T;

    alias array this;

    /**
    Constructor by element type.
    This vector's each element is filled by given value.

    Params:
        e = a value which fills the vector
    */
    this(T e) {
        assignSingle(e);
    }

    /**
    Constructor by combination of scalar, array, or vector.

    Params:
        args = combination of values, whose total element count must be the same as S
    */
    this(Args...)(Args args) {
        assignAll(args);
    }

    /**
    Unary operation for "+".
    This function is identity.

    Returns: a vector equals to this vector.
    */
    Vector opUnary(string op)() const 
    if (op == "+")
    {
        return makeVector!("elements[]"); // a[] = +b[]; is an invalid expression
    }

    /**
    Unary operation for "-".

    Returns: an inverted vector
    */
    Vector opUnary(string op)() const 
    if (op == "-")
    {
        return makeVector!(op ~ "elements[]");
    }


    /**
    Binary operation between scalar, array or vector type.

    Params:
        value = operation target

    Returns: calculated vector
    */
    Vector opBinary(string op, AnotherType)(auto ref AnotherType value) const 
    { 
        return makeVector!("elements[]" ~ op ~ Expression!(AnotherType))(value);
    }

    /**
    Binary operation between matrix, scalar, array or vector type.

    Params:
        value = operation target

    Returns: calculated vector
    */
    Vector opBinaryRight(string op, AnotherType)(auto ref AnotherType value) const 
    {
        import sbylib.math.matrix : isMatrix;

        static if (isPotentially!(AnotherType, isMatrix)) {
            return mixin("value"~PotentialExpression!(AnotherType, isMatrix) ~ ".opBinary!(\""~op~"\")(this)");
        } else {
            return makeVector!(Expression!(AnotherType) ~ op ~ "elements[]")(value);
        }
    }

    /**
    Assign operation for vector, array, or scalar type.
    If the argument type is scalar, the value fills each element of this vector.

    Params:
        value = assigned value

    Returns: this vector after assigned.
    */
    Vector opAssign(AnotherType)(const AnotherType value) {
        static if (isVector!(AnotherType) || isArray!(AnotherType)) {
            this.assignAll(value);
        } else {
            this.assignSingle(value);
        }
        return this;
    }

    /**
    Operator assign for vector, array, or scalar type.
    If the argument type is scalar, the value affects each element of this vector.

    Params:
        value = assigned value

    Returns: this vector after assigned.
    */
    Vector opOpAssign(string op, AnotherType)(AnotherType value)
    {
        mixin("elements[] " ~ op ~ "=" ~ Expression!(AnotherType) ~ ";");
        return this;
    }

    /**
    Indexing operation.
    
    Params:
        idx = index

    Returns: i-th element of this vector.
    */
    T opIndex(size_t idx) const {
        return this.elements[idx];
    }

    /**
    Indexing operation.
    
    Params:
        idx = index

    Returns: i-th element of this vector.
    */
    ref T opIndex(size_t idx) {
        return this.elements[idx];
    }

    /**
    Get raw array of this vector.

    Returns: raw array of this vector.
    */
    auto array() inout {
        return elements;
    }

    private enum XYZW = "xyzw";
    private enum RGBA = "rgba";

    private static size_t[] createIndex(string propertyString, string s) {
        size_t[] index = new size_t[s.length];
        foreach (i; 0..s.length) {
            index[i] = propertyString.countUntil(s[i]);
        }
        return index;
    }

    private mixin template Gen(string s) {
        enum isXYZW = s.all!(a => XYZW.canFind(a));
        enum isRGBA = s.all!(a => RGBA.canFind(a));
        static assert(isXYZW || isRGBA);
        enum propertyString = isXYZW ? XYZW : isRGBA ? RGBA : "";
        enum index = createIndex(propertyString, s);
    }

    /**
    Single element access by name.
    The property is 'x', 'y', 'z', 'w', 'r', 'g', 'b' or 'a'.

    Returns: selected element
    */
    auto ref opDispatch(string s)() inout
    if (s.length == 1)
    {
        mixin Gen!(s);
        return elements[index[0]];
    }

    /**
    Multiple element access by name.
    The property is combination of 'x', 'y', 'z', 'w', 'r', 'g', 'b' or 'a'.

    Returns: a vector made by selected elements
    */
    const(Vector!(T,s.length)) opDispatch(string s)() inout
    if (s.length > 1)
    {
        mixin Gen!(s);
        Vector!(T, s.length) result;
        static foreach (i,idx; index) {
            result[i] = elements[idx];
        }
        return result;
    }

    /**
    Property assign for scalar value.
    This function supports both single and multiple property reference.

    Params:
        value = assigned scalar value

    Returns: assigned value
    */
    T opDispatch(string s)(T value)
    if (s.all!(a =>XYZW.canFind(a)) || s.all!(a => RGBA.canFind(a)))
    {
        mixin Gen!(s);
        static if(s.length == 1) {
            return this[index[0]] = value;
        } else {
            static foreach (i,idx; index) {
                this[idx] = value;
            }
            return value;
        }
    }

    /**
    Property assign for vector value.
    This function supports both single and multiple property reference.

    Params:
        value = assigned vector value

    Returns: assigned value
    */
    const(Vector!(T,s.length)) opDispatch(string s)(Vector!(T,s.length) value)
    if (s.all!(a =>XYZW.canFind(a)) || s.all!(a => RGBA.canFind(a)))
    {
        mixin Gen!(s);
        static foreach (i,idx; index) {
            this[idx] = value[i];
        }
        return value;
    }

    string toString() const {
        import std.format : format;
        import std.array : array, join;
        import std.algorithm : map;
        import std.conv : to;

        return format!"(%s)"(this.elements.array.map!(to!string).join(","));
    }

    static if (isFloatingPoint!T) {
        /**
        Returns whether this vector contains NaN.

        Returns: true if this vector contains NaN
        */
        bool hasNaN() const {
            import std.algorithm : any;
            import std.array : array;
            import std.math : isNaN;

            return this.elements.array.any!(isNaN);
        }
    }

    private void assignAll(Args...)(Args args) {
        T[] head = elements;
        static foreach (arg; args) {
            assign(head, arg);
        }
    }

    private void assign(AnotherType)(ref T[] head, AnotherType value) {
        static if (isVector!(AnotherType)) {
            assign(head, value.elements);
        } else static if (isArray!(AnotherType)) {
            foreach (ref v; value) assign(head, v);
        } else {
            assert(head.length > 0);
            head[0] = value;
            head = head[1..$];
        }
    }

    private void assignSingle(AnotherType)(AnotherType value) {
        this.elements[] = value;
    }

    private template Expression(AnotherType) {
        static if (isPotentially!(AnotherType, isArray)) 
            enum Expression = "value" ~ PotentialExpression!(AnotherType, isArray) ~ "[]";
        else 
            enum Expression = "value";
    }

    private Vector makeVector(string expr, AnotherType...)(auto ref AnotherType values) const {
        static if (AnotherType.length > 0) alias value = values[0];
        else alias value = values;

        Vector result;
        result.elements[] = mixin(expr);
        return result;
    }
}


/**
Get a vector whose each element is minimum value of the same indexed element of two vectors.

Params:
    v = left hand value of calculation
    v2 = right hand value of calculation

Returns: minimum vector of two vectors
*/
Vector!(CommonType!(T,S),U) min(T,S,uint U)(Vector!(T,U) v, Vector!(S,U) v2) {
    import std.math : fmin = min;
    return reduceV!(fmin)(v, v2);
}

/**
Get a vector whose each element is maximum value of the same indexed element of two vectors.

Params:
    v = left hand value of calculation
    v2 = right hand value of calculation

Returns: maximum vector of two vectors
*/
Vector!(CommonType!(T,S),U) max(T,S,uint U)(Vector!(T,U) v, Vector!(S,U) v2) {
    import std.math : fmax = max;
    return reduceV!(fmax)(v, v2);
}

/**
Calculates dot product.

Params:
    v = left hand value of dot product
    v2 = right hand value of dot product

Returns: dot product of two vectors
*/
CommonType!(T,S) dot(T,S,uint U)(Vector!(T,U) v, Vector!(S,U) v2) {
    CommonType!(T,S) result = 0;
    static foreach (i; 0..U) {
        result += v[i] * v2[i];
    }
    return result;
}

/**
Calclates cross product of two vectors in 2D.
Returned value is |v||v2| sin a

Params:
    v = left hand value of calculation
    v2 = right hand value of calculation

Returns: cross product of two vectors
*/
CommonType!(T,S) cross(T,S)(Vector!(T,2) v, Vector!(S,2) v2) {
    return v.x * v2.y - v.y * v2.x;
}

/**
Calclates cross product of two vectors in 3D.

Params:
    v = left hand value of calculation
    v2 = right hand value of calculation

Returns: cross product of two vectors
*/
Vector!(CommonType!(T,S),3) cross(T,S)(Vector!(T,3) v, Vector!(S,3) v2) {
    Vector!(CommonType!(T,S),3) result;
    static foreach (i; 0..3) {
        result[i] = v[(i+1)%3] * v2[(i+2)%3] - v[(i+2)%3] * v2[(i+1)%3];
    }
    return result;
}

/**
Returns a vector which is orthognal to the given vector.

Params:
    v = target vector

Returns: orthognal vector
*/
Vector!(T,2) ortho(T)(Vector!(T,2) v) {
    return Vector!(T,2)(-v.y, v.x);
}

/**
Returns a vector which is orthognal to the given vector.

Params:
    v = target vector

Returns: orthognal vector
*/
Vector!(T,3) ortho(T)(Vector!(T,3) v) 
if (__traits(isFloating, T))
{
    if (v.x == 0 && v.z == 0) {
        return normalize(cross(v, Vector!(T,S)(1,0,0)));
    } else {
        return normalize(cross(v, Vector!(T,S)(0,1,0)));
    }
}

/**
Returns squared length of the given vector.

Params:
    v = target vector

Returns: squared length of the given vector
*/
T lengthSq(T,uint U)(Vector!(T,U) v) {
    T result = 0;
    static foreach (i; 0..U) {
        result += v[i] * v[i];
    }
    return result;
}

/**
Returns length of the given vector.

Params:
    v = target vector

Returns: length of the given vector
*/
T length(T,uint U)(Vector!(T,U) v) 
if (__traits(isFloating, T))
{
    import std.math : sqrt;
    return sqrt(lengthSq(v));
}

/**
Returns a vector whose length 1 from target vector.

Params:
    v = target vector

Returns: normalized target vector
*/
Vector!(T,U) normalize(T,uint U)(Vector!(T,U) v) 
if (__traits(isFloating, T))
{
    return v / length(v);
}

/**
Returns a vector whose length 1 from target vector.
When the length is 0, returns zero vector.

Params:
    v = target vector

Returns: normalized target vector if the length is greater than 0, otherwise zero vector
*/
Vector!(T,U) safeNormalize(T,uint U)(Vector!(T,U) v) 
if (__traits(isFloating, T))
{
    const l = length(v);
    if (l == 0) return v;
    return v / l;
}

/**
Reduces an array of vector by the given function.

Params:
    v = array of vector reduced

Returns: reduced vector
*/
Vector!(T,U) reduceV(alias pred, T, int U)(Vector!(T,U)[] v...) {
    Vector!(T,U) result = v[0];
    static foreach (i; 0..U) {
        static foreach (j; 1..v.length) {
            result[i] = pred(result[i], v[j][i]);
        }
    }
    return result;
}

/**
Calculates an unsigned area of a triangle which is composed of the given points.

Params:
    positions = array of position vectors which compose a triangle

Returns: unsigned area of triangle
*/
T computeUnSignedArea(T)(Vector!(T,3)[3] positions...) {
    return length(cross(positions[2] - positions[0], positions[1] - positions[0]));
}

/**
Calculates a signed volume of a tetrahedron which is composed of the given points.

Params:
    positions = array of position vectors which compose a tetrahedron

Returns: signed volume of tetrahedron
*/
T computeSignedVolume(T)(Vector!(T,3)[4] positions...) {
    Vector!(T,3)[3] v;
    static foreach (i; 0..3) {
        v[i] = positions[i+1] - positions[0];
    }
    T result = 0;
    static foreach (i; 0..3) {
        result += v[i].x * v[(i+1)%3].y * v[(i+2)%3].z;
        reslut -= v[i].x * v[(i+2)%3].y * v[(i+1)%3].z;
    }
    return result;
}

//与えられた点列に対し、分散が最大化するベクトルを含む正規直交基底を返す
/**
Returns normalized orthognal basis which maximize the dispersion of the given points

Params:
    vertices = array of points

Returns: 3 normalized orthognal basis
*/
Vector!(T,3)[3] mostDispersionBasis(T)(Vector!(T,3)[] vertices...)
    in(vertices.length > 0)
{
    import std.algorithm : sum;
    const c = vertices.sum / vertices.length;
    mat3 vcm = mat3(0);
    foreach (ref v; vertices) {
        const r = v - c;
        vcm += Matrix!(float,3,1)(r.array) * Matrix!(float,1,3)(r.array);
    }
    vcm /= vertices.length;
    const diagonal = mat3.diagonalizeForRealSym(vcm);
    Vector!(T,3)[3] result;
    static foreach (i; 0..3) {
        result[i] = diagonal.column[i].normalize;
    }
    return result;
}

/**
Returns true if the distance between the given two point is less than eps.

Params:
    a = target point
    b = target point
    eps = criteria of approximation

Returns: true if 2 vectors are approximately equal.
*/
bool approxEqual(T,uint N)(Vector!(T,N) a, Vector!(T,N) b, T eps = 1e-5) {
    return length(a-b) < eps;
}

private template PotentialExpression(T, alias pred) {
    static if (pred!(T)) {
        enum PotentialExpression = "";
    } else static if (isAggregateType!(T)) {
        enum AliasThis = __traits(getAliasThis, T);
        static assert(AliasThis.length > 0);
        enum Expr = "T."~AliasThis[0];
        static if (isFunction!(mixin(Expr))) {
            alias Type = ReturnType!(mixin(Expr));
        } else {
            alias Type = typeof(mixin(Expr));
        }
        enum PotentialExpression = "." ~ AliasThis[0] ~ PotentialExpression!(Type, pred);
    }
}

private template isPotentially(T, alias pred) {
    static if (pred!(T)) {
        enum isPotentially = true;
    } else static if (isAggregateType!(T)) {
        enum AliasThis = __traits(getAliasThis, T);
        static if (AliasThis.length == 0) {
            enum isPotentially = false;
        } else {
            enum Expr = "T."~AliasThis[0];
            static if (isFunction!(mixin(Expr))) {
                alias Type = ReturnType!(mixin(Expr));
            } else {
                alias Type = typeof(mixin(Expr));
            }
            enum isPotentially = isPotentially!(Type, pred);
        }
    } else {
        enum isPotentially = false;
    }
}
