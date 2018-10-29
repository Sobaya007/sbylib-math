module sbylib.math.Angle;

enum isAngle(Type) = is(Type == Degree) || is(Type == Radian);

struct Angle {
    private Degree deg;

    this(Degree deg) {
        this.deg = deg;
    }

    this(Radian rad) {
        this.deg = rad.toDeg;
    }

    Radian asRad() {
        return deg.toRad;
    }

    Degree asDeg() {
        return deg;
    }

    Angle opUnary(string op)() {
        return Degree(mixin(op ~ "this.asFloat()"));
    }

    Angle opBinary(string op)(float v) if (op == "*" || op == "/") {
        return Degree(mixin("this.asFloat() " ~ op ~ " v"));
    }

    Angle opBinary(string op)(Angle a) {
        return Degree(mixin("this.asFloat() " ~ op ~ " a.asFloat()"));
    }

    Angle opBinaryRight(string op)(float v) if (op == "*" || op == "/") {
        return Degree(mixin("v" ~ op ~ "this.asFloat()"));
    }

    void opOpAssign(string op)(Angle a) {
        mixin("this.asFloat()" ~ op ~ "= a.asFloat();");
    }

    void opOpAssign(string op)(float v) if (op == "*" || op == "/") {
        mixin("this.asFloat()" ~ op ~ "= v;");
    }

    int opCmp(Angle a) {
        import std.math : sgn;
        return cast(int)sgn(this.asFloat() - a.asFloat());
    }

    bool opEquals(Angle a) {
        return this.asFloat() == a.asFloat();
    }

    string toString() {
        return deg.toString;
    }

    ref float asFloat() {
        return deg.deg;
    }
}


struct Degree {
    private float deg;
    alias asAngle this;

    private this(float deg) {
        this.deg = deg;
    }

    void opAssign(Radian rad) {
        this.deg = rad.toDeg.deg;
    }
const:

    Angle asAngle() {
        return Angle(this);
    }

    Radian toRad() {
        import std.math : PI;
        return Radian(deg * PI / 180);
    }

    string toString() {
        import std.format;
        return format!"%f [deg.]"(deg);
    }
}

struct Radian {
    private float rad;

    alias asAngle this;

    private this(float rad) {
        this.rad = rad;
    }

    void opAssign(Degree deg) {
        this.rad = deg.toRad.rad;
    }

const:

    Angle asAngle() {
        return Angle(this);
    }

    Degree toDeg() {
        import std.math : PI;
        return Degree(rad * 180 / PI);
    }

    string toString() {
        import std.format;
        return format!"%f [rad.]"(rad);
    }
}

Degree deg(float d) {
    return Degree(d);
}

Radian rad(float r) {
    return Radian(r);
}

mixin template DefineRadInputFunc(string func) {
    import std.format;
    mixin(format!q{

auto %s(Angle angle) {
    import std.math : %s;
    return %s(angle.asRad.rad);
}

    }(func, func, func));
}

mixin template DefineRadOutputFunc(string func) {
    import std.format;
    mixin(format!q{

auto %s(float angle) {
    import std.math : %s;
    return %s(angle).rad;
}

    }(func, func, func));
}

mixin template DefineAngleInoutFunc(string func) {
    import std.format;
    mixin(format!q{

auto %s(Angle angle) {
    import std.math : %s;
    return %s(angle.asFloat()).deg;
}

    }(func, func, func));
}

mixin DefineRadInputFunc!("sin");
mixin DefineRadInputFunc!("cos");
mixin DefineRadInputFunc!("tan");
mixin DefineRadOutputFunc!("asin");
mixin DefineRadOutputFunc!("acos");
mixin DefineRadOutputFunc!("atan");
mixin DefineAngleInoutFunc!("abs");
