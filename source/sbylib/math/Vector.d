module sbylib.math.Vector;

import std.algorithm;
import std.range;
import std.conv;
import std.math;
import std.string;
import std.stdio;
import std.format;
import std.traits;

alias Vector!(float, 2) vec2; //========================================適当にaliasしとく
alias Vector!(float, 3) vec3;
alias Vector!(float, 4) vec4;
alias Vector!(int,   2) vec2i;
alias Vector!(int,   3) vec3i;
alias Vector!(int,   4) vec4i;


enum isVector(T) = isInstanceOf!(Vector, T);

//T型のS個のベクトル
struct Vector(T, uint S) 
{

    enum Dimension = S;
    alias ElementType = T;

    T[S] elements;
    alias elements this;

    this(T e) {
        assignSingle(e);
    }

    //数字、配列、Vectorいずれも使える
    this(Args...)(Args args) {
        assignAll(args);
    }

    Vector opBinary(string op, AnotherType)(auto ref AnotherType value) const 
    { 
        return makeVector!("elements[]" ~ op ~ Expression!(AnotherType))(value);
    }

    Vector opBinaryRight(string op, AnotherType)(auto ref AnotherType value) const 
    {
        import sbylib.math.Matrix;
        static if (isPotentially!(AnotherType, isMatrix)) {
            return mixin("value"~PotentialExpression!(AnotherType, isMatrix) ~ ".opBinary!(\""~op~"\")(this)");
        } else {
            return makeVector!(Expression!(AnotherType) ~ op ~ "elements[]")(value);
        }
    }

    Vector opUnary(string op)() const {
        static if (op == "+")
            return makeVector!("elements[]"); // a[] = +b[]; はダメらしい
        else
            return makeVector!(op ~ "elements[]");
    }

    Vector opAssign(AnotherType)(AnotherType a) {
        static if (isVector!(AnotherType) || isArray!(AnotherType)) {
            this.assignAll(a);
        } else {
            this.assignSingle(a);
        }
        return this;
    }

    Vector opOpAssign(string op, AnotherType)(AnotherType value)
    {
        mixin("elements[] " ~ op ~ "=" ~ Expression!(AnotherType) ~ ";");
        return this;
    }

    T opIndex(size_t idx) const {
        return this.elements[idx];
    }

    T opIndexAssign(ElementType value, size_t idx) {
        return this.elements[idx] = value;
    }

    T opIndexOpAssign(string op, AnotherType)(AnotherType value, size_t idx) {
        mixin("return this.elements[idx] " ~ op ~ "= value;");
    }

    auto array() inout {
        return elements;
    }

    enum XYZW = "xyzw";
    enum RGBA = "rgba";

    mixin template Gen(string s) {
        enum isXYZW = s.all!(a => XYZW.canFind(a));
        enum isRGBA = s.all!(a => RGBA.canFind(a));
        static assert(isXYZW || isRGBA);
        enum propertyString = isXYZW ? XYZW : isRGBA ? RGBA : "";
        enum index = s.map!(a => countUntil(propertyString, a)).array;
    }

    auto ref opDispatch(string s)() inout
        if (s.all!(a =>XYZW.canFind(a)) || s.all!(a => RGBA.canFind(a)))
    {
        mixin Gen!(s);
        static if(s.length == 1) {
            return elements[index[0]];
        } else {
            Vector!(T, s.length) result;
            static foreach (i,idx; index) {
                result[i] = elements[idx];
            }
            return result;
        }
    }

    ElementType opDispatch(string s)(ElementType val)
        if (s.all!(a =>XYZW.canFind(a)) || s.all!(a => RGBA.canFind(a)))
    {
        mixin Gen!(s);
        static if(s.length == 1) {
            return this[index[0]] = val;
        } else {
            static foreach (i,idx; index) {
                this[idx] = val;
            }
            return val;
        }
    }

    Vector!(ElementType,s.length) opDispatch(string s)(Vector!(ElementType,s.length) val)
        if (s.all!(a =>XYZW.canFind(a)) || s.all!(a => RGBA.canFind(a)))
    {
        mixin Gen!(s);
        static foreach (i,idx; index) {
            this[idx] = val[i];
        }
        return val;
    }

    string toString() const {
        import std.format;
        return format!"(%s)"(this.elements.array.map!(to!string).join(","));
    }

    static if (isFloatingPoint!T) {
        bool hasNaN() const {
            return this.elements.array.any!(isNaN);
        }
    }
    static if (isBasicType!(T)) {
        static Vector fromString(string str) {
            Vector r;
            auto strs = str.split[2].split(",");
            foreach (int c, s; strs) {
                r[c] = to!T(s);
            }
            return r;
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
        static if (isPotentially!(AnotherType, isArray)) enum Expression = "value" ~ PotentialExpression!(AnotherType, isArray) ~ "[]";
        else enum Expression = "value";
    }

    private Vector makeVector(string expr, AnotherType...)(auto ref AnotherType values) const {
        static if (AnotherType.length > 0) alias value = values[0];
        else alias value = values;

        Vector result;
        result.elements[] = mixin(expr);
        return result;
    }
}

//======================================================================以下ベクトル計算系の関数達

template dot(T, S, uint U) {

    alias Result = Largest!(T,S);

    Result dot(Vector!(T, U) v, Vector!(S, U) v2) {
        Result result;
        mixin({
            string code = "result = ";
            foreach (i; 0..U) {
                code ~= format!"+v.elements[%d] * v2.elements[%d]"(i,i);
            }
                code ~= ";";
                return code;
        }());
        return result;
    }
}

template minVector(T, S, uint U) {

    alias Result = Largest!(T,S);
    Vector!(Result, U) minVector(Vector!(T,U) v, Vector!(S,U) v2) {
        mixin({
            string str = "return Vector!(Result,U)(";
            foreach (i; 0..U) {
                str ~= format!"min(v[%d], v2[%d])"(i,i);
                if (i < U-1) str ~= ",";
            }
                return str ~ ");";
        }());
    }
}

template maxVector(T, S, uint U) {

    alias Result = Largest!(T,S);
    Vector!(Result, U) maxVector(Vector!(T,U) v, Vector!(S,U) v2) {
        mixin({
            string str = "return Vector!(Result,U)(";
            foreach (i; 0..U) {
                str ~= format!"max(v[%d], v2[%d])"(i,i);
                if (i < U-1) str ~= ",";
            }
                return str ~ ");";
        }());
    }
}

template cross(T, S, uint U) if (U == 2) {

    alias Result = Largest!(T,S);

    Result cross(Vector!(T, U) v, Vector!(S, U) v2){
        return v.x * v2.y - v.y * v2.x;
    }
}

template cross(T, S, uint U) if (U == 3) {

    alias Result = Largest!(T,S);

    Vector!(Result, U) cross(Vector!(T, U) v, Vector!(S, U) v2) {
        Vector!(Result, U) result;
        mixin({
            string code;
            foreach (i; 0..U) {
                code ~= format!"result[%d] = v[%d] * v2[%d] - v[%d] * v2[%d];"(i,(i+1)%3,(i+2)%3,(i+2)%3,(i+1)%3);
            }
                return code;
        }());
        return result;
    }
}


Vector!(T, S) getOrtho(T, uint S)(Vector!(T,S) v) {
    static if (S == 2) {
        return Vector!(T, S)(-v.y, v.x);
    } else static if (S == 3) {
        if (v.x == 0 && v.z == 0) {
            return normalize(cross(v, Vector!(T,S)(1,0,0)));
        } else {
            return normalize(cross(v, Vector!(T,S)(0,1,0)));
        }
    }
}


T length(T, int S)(Vector!(T, S) v) {
    mixin({
        string code = "T result = sqrt(";
        foreach (i; 0..S) {
            code ~= format!"+v[%d] * v[%d]"(i,i);
        }
            code ~= ");";
            return code;
    }());
    return result;
}

T lengthSq(T, int S)(Vector!(T, S) v) {
    mixin({
        string code = "T result = ";
        foreach (i; 0..S) {
            code ~= format!"+v[%d] * v[%d]"(i,i);
        }
            code ~= ";";
            return code;
    }());
    return result;
}

Vector!(T, S) normalize(T, int S)(Vector!(T, S) v) {
    mixin({
        string code = "T length = sqrt(";
        foreach (i; 0..S) {
            code ~= format!"+v[%d] * v[%d]"(i,i);
        }
            code ~= ");";
            code ~= "Vector!(T, S) result;";
            foreach (i; 0..S) {
                code ~= format!"result[%d] = v[%d] / length;"(i,i);
            }
                return code;
    }());
    return result;
}

Vector!(T,S) safeNormalize(T, int S)(Vector!(T,S) v) {
    if (mixin({
            string code;
            foreach (i; 0..S) {
                code ~= format!"v.elements[%d] == 0"(i);
                if (i < S-1) code ~= "&&";
            }
                return code;
        }())) return Vector!(T,S)(0);
    return normalize(v);
}

Vector!(T,S) reduceV(alias pred, T, int S)(Vector!(T,S)[] v...) {
    Vector!(T,S) result = v[0];
    foreach (i; 0..S) {
        foreach (j; 1..v.length) {
            result.elements[i] = pred(result.elements[i], v[j].elements[i]);
        }
    }
    return result;
}

T computeUnSignedArea(T)(Vector!(T,3)[3] positions...) {
    return length(cross(positions[2] - positions[0], positions[1] - positions[0]));
}

T computeSignedVolume(T)(Vector!(T, 3)[4] positions...) {
    mixin({
            string code;
            foreach (i; 0..3) {
            code ~= "Vector!(T,3) v" ~ to!string(i) ~ " = positions[" ~to!string(i+1) ~ "] - positions[0];\n";
            }
            code ~= "return ";
            foreach (i; 0..3) {
            code ~= "+v" ~ to!string(i) ~ ".x * v" ~ to!string((i+1)%3) ~ ".y * v" ~ to!string((i+2)%3) ~ ".z\n";
            }
            foreach (i; 0..3) {
            code ~= "-v" ~ to!string(i) ~ ".x * v" ~ to!string((i+2)%3) ~ ".y * v" ~ to!string((i+1)%3) ~ ".z\n";
            }
            code ~= ";";
            return code;
            }());
}

//与えられた点列に対し、分散が最大化するベクトルを含む正規直交基底を返す
Vector!(T,3)[] mostDispersionBasis(T)(Vector!(T,3)[] vertices...)
    in(vertices.length > 0)
{
    import std.algorithm, std.array, std.range;
    import sbylib.math.Matrix;
    auto c = vertices.sum / vertices.length;
    mat3 vcm = mat3(0);
    foreach (ref v; vertices) {
        auto r = v.xyz - c;
        vcm += Matrix!(float,3,1)(r.array) * Matrix!(float,1,3)(r.array);
    }
    vcm /= vertices.length;
    auto diagonal = mat3.diagonalizeForRealSym(vcm);
    auto base = 3.iota.map!(a => diagonal.column[a].normalize).array;
    return base;
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

