module sbylib.math.test;

import sbylib.math;

unittest {
    vec2 a = vec2(1,2);
    vec2 b = vec2(2,1);
    assert(a.xy == vec2(1,2));
    assert(a.xy == b.yx);
    vec3 c;
    c.yxz = vec3(3,2,1);
    assert(c.zx == a);

    c.zx = 1;
    assert(c == vec3(1,3,1));

    assert(vec4(vec2(1), vec2(2)) == vec4(1,1,2,2));

    assert(vec3(1) == vec3(1,1,1));

    vec2 d = vec2(2,1);
    d.xy = d.yx;
    assert(d.xy == vec2(1,2));

    d.x++;
    assert(d == vec2(2));
}

unittest {
    struct S {
        auto get() { return vec3(0); }

        alias get this;
    }

    S s;
    vec3 b = s;
    b += s;
}

unittest {
    static assert(__traits(hasMember, vec3, "undefinedMember") == false);
    static assert(__traits(hasMember, vec3, "x") == true);
    static assert(__traits(hasMember, vec3, "yxz") == true);
    //static assert(isCallable!(vec3.x) == false);
}

unittest {
    auto m1 = mat3(
            1,4,7,
            2,5,8,
            3,6,9);
    auto m2 = mat3(vec3(1,2,3),vec3(4,5,6),vec3(7,8,9));

    assert(m1 == m2);

    auto m3 = mat3(
            1,0,1,
            0,1,1,
            0,0,1);
    auto m4 = mat3(
            1,2,3,
            4,5,6,
            1,1,1);

    auto m5 = mat3(
            2,2,4,
            4,6,7,
            1,1,2);
    auto m6 = mat3(
            2,3,4,
            5,6,7,
            1,1,1);

    assert(m3 + m4 == m5);
    assert(m3 * m4 == m6);

    auto p = vec4(0,1,2,1);
    auto m = mat4(
            1,0,0,100,
            0,1,0,200,
            0,0,1,300,
            0,0,0,1);
    assert(m * p == vec4(100,201,302,1));
    auto t = mat4.translate(vec3(100,200,300));
    assert(m == t);
}

unittest {
    struct S {
        auto get() { return mat2(0); }
        alias get this;
    }

    S s;
    vec2 a = s * vec2(0);

}
