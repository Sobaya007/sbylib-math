module sbylib.math.Angle;

enum isAngle(Type) = is(Type == Degree) || is(Type == Radian);

struct Angle {
    import std.variant;
    alias AngleImpl = Algebraic!(Degree*, Radian*);
    private AngleImpl value;

    this(ref Degree deg) {
        this.value = &deg;
    }

    this(ref Radian rad) {
        this.value = &rad;
    }

    Radian asRad() {
        return value.visit!(
            (Degree* deg) => deg.toRad,
            (Radian* rad) => *rad,
        );
    }

    Degree asDeg() {
        return value.visit!(
            (Degree* deg) => *deg,
            (Radian* rad) => rad.toDeg,
        );
    }

    float asFloat() {
        return value.visit!(
            (Degree* deg) => deg.deg,
            (Radian* rad) => rad.rad,
        );
    }
}


struct Degree {
    private float deg;
    Angle angle;
    alias angle this;

    private this(float deg) {
        this.deg = deg;
        this.angle = Angle(this);
    }

    void opAssign(Radian rad) {
        this.deg = rad.toDeg.deg;
    }

const:

    Degree opUnary(string op)() {
        return Degree(mixin(op ~ "deg"));
    }

    Degree opBinary(string op)(float v) {
        return Degree(mixin("deg " ~ op ~ " v"));
    }

    Degree opBinary(string op)(Degree v) {
        return Degree(mixin("deg " ~ op ~ " v.deg"));
    }

    Radian toRad() {
        import std.math : PI;
        return Radian(deg * PI / 180);
    }

    bool opEquals(Degree deg) {
        return this.deg == deg.deg;
    }

    bool opEquals(Radian rad) {
        return this.deg == rad.toDeg.deg;
    }

    string toString() {
        import std.format;
        return format!"%f [deg.]"(deg);
    }
}

struct Radian {
    private float rad;
    Angle angle;
    alias angle this;

    private this(float rad) {
        this.rad = rad;
        this.angle = Angle(this);
    }

    void opAssign(Degree deg) {
        this.rad = deg.toRad.rad;
    }

const:

    Radian opUnary(string op)() {
        return Radian(mixin(op ~ "rad"));
    }

    Radian opBinary(string op)(float v) {
        return Radian(mixin("rad " ~ op ~ " v"));
    }

    Radian opBinary(string op)(Radian v) {
        return Radian(mixin("rad " ~ op ~ " v.rad"));
    }

    Degree toDeg() {
        import std.math : PI;
        return Degree(rad * 180 / PI);
    }

    bool opEquals(Radian rad) {
        return this.rad == rad.rad;
    }

    bool opEquals(Degree deg) {
        return this.rad == deg.toRad.rad;
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

mixin DefineRadInputFunc!("sin");
mixin DefineRadInputFunc!("cos");
mixin DefineRadInputFunc!("tan");
mixin DefineRadOutputFunc!("asin");
mixin DefineRadOutputFunc!("acos");
mixin DefineRadOutputFunc!("atan");
