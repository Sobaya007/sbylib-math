module sbylib.math.NumLimited;

import std.conv;

struct NumLimited(T,
        alias limit = (T value, T lower, T upper) => value < lower ? lower : (value > upper ? upper : value)) 
if (__traits(isArithmetic, T)) {

    T element;

    T upper, lower;

    this(T e) {
        upper = T.max;
        static if (__traits(isFloating, T)) 
            lower = T.min_normal;
        else 
            lower = T.min;
        element = limit(e, lower, upper);
    }

    this(T e, T lower, T upper) {
        element = e;
        this.upper = upper;
        this.lower = lower;
        element = limit(e, lower, upper);
    }

    T opBinary(string op)(T t) {
        T result;
        mixin("result = element " ~ op ~ "t;");
        return result;
    }

    void opOpAssign(string op)(T t) {
        mixin("element " ~ op ~ "= t;");
        element = limit(element, lower, upper);
    }

    string toString() {
        return to!string(element);
    }

    alias element this;
}

alias NumLimited!float flim;
alias NumLimited!double dlim;
//alias NumLimited!int ilim;
