module sbylib.math.Matrix;

import sbylib.math;
import std.conv;
import std.string;
import std.stdio;
import std.traits;
import std.format;


//T型のUxV行列
/*
   1 1 1 1
   1 1 1 1
   は
   Matrix!(float, 2, 4)
   */

alias Matrix!(float, 2, 2) mat2;
alias Matrix!(float, 3, 3) mat3;
alias Matrix!(float, 4, 4) mat4;
alias Matrix!(float, 2, 3) mat2x3;
alias Matrix!(float, 3, 2) mat3x2;
alias Matrix!(float, 2, 4) mat2x4;
alias Matrix!(float, 4, 2) mat4x2;
alias Matrix!(float, 3, 4) mat3x4;
alias Matrix!(float, 4, 3) mat4x3;
alias Matrix!(double, 2, 2) mat2d;
alias Matrix!(double, 3, 3) mat3d;
alias Matrix!(double, 4, 4) mat4d;

struct Matrix(T, uint U, uint V) if (__traits(isArithmetic, T) && 1 <= U && U <= 4 && 1 <= V && V <= 4){
private:
    T[U*V] element;
public:
    enum Row = U;
    enum Column = V;
    alias ElementType = T;

    private alias vec3 = Vector!(T, 3);
    private alias vec4 = Vector!(T, 4);

    this(T e) {
        foreach (ref el; this.element) el = e;
    }

    this(T[] elements)
        in(elements.length == U * V)
    {
        foreach(i, e; elements) {
            this[i/V,i%V] = e;
        }
    }

    this(T[U*V] elements...) {
        foreach(i, e; elements) {
            this[i/V,i%V] = e;
        }
    }

    this(T[U][V] vectors...) {
        foreach (i; 0..U) {
            foreach (j; 0..V) {
                this[i,j] = vectors[j][i];
            }
        }
    }

    this(Vector!(T, U)[V] vectors...) {
        foreach (i; 0..U) {
            foreach (j; 0..V) {
                this[i,j] = vectors[j][i];
            }
        }
    }

    Matrix opBinary(string op, S, uint P, uint Q)(const Matrix!(S, P,Q)  m) const if ((op == "+" || op == "-") && U == P && V == Q) {
        static if (isImplicitlyConvertible!(T,S)) {
            alias Type = S;
        } else {
            alias Type = T;
        }

        Matrix!(Type,U,V) result;
        foreach (i; 0..U) {
            foreach (j; 0..V) {
                mixin(format!q{result[i,j] = this[i,j] %s m[i,j];}(op));
            }
        }
        return result;
    }

    Matrix!(S,U,Q) opBinary(string op, S, uint P, uint Q)(const Matrix!(S,P,Q) m) if (op == "*" && V == P) {
        static if (isImplicitlyConvertible!(T,S)) {
            alias Type = S;
        } else {
            alias Type = T;
        }
        Matrix!(Type,U,Q) result;
        foreach (i; 0..U) {
            foreach (j; 0..Q) {
                result[i,j] = 0;
                foreach (k; 0..V) {
                    result[i,j] += this[i,k] * m[k,j];
                }
            }
        }
        return result;
    }

    Matrix!(S,U,Q) opBinaryRight(string op, S, uint P, uint Q)(const Matrix!(S,P,Q) m) if (op == "*" && V == P) {
        static if (isImplicitlyConvertible!(T,S)) {
            alias Type = S;
        } else {
            alias Type = T;
        }
        Matrix!(Type,U,Q) result;
        mixin(multMMRightCode(U,V,P,Q));
        return result;
    }

    Matrix opBinary(string op)(T s) const {
        Matrix result;
        static if (op == "*" || op == "/") {
            mixin(getOpBinaryMSCode(op, U,V));
            return result;
        } else {
            static assert(false);
        }
    }

    Vector!(S,U) opBinary(string op, S)(Vector!(S,V) v) const {
        Vector!(S,U) result;
        static if (op == "*") {
            mixin(multMVCode(U, V));
            return result;
        } else {
            static assert(false);
        }
    }

    Matrix!(T,U,V) opOpAssign(string op)(T s) {
        static if (op == "*" || op == "/") {
            mixin(getOpAssignMSCode(op, U, V));
        } else {
            static assert(false);
        }
        return this;
    }

    Matrix!(T,U,V) opOpAssign(string op, S, uint P, uint Q)(Matrix!(S, P, Q) m) {
        static if (op == "+" || op == "-") {
            static assert(U == P && V == Q);
            mixin(getOpAssignMMCode(op, U, V));
        } else static if (op == "*") {
            Matrix result = this * m;
            mixin(getCopyCode("result", U, V));
        } else {
            static assert(false);
        }
        return this;
    }

    T opIndex(size_t i, size_t j) const { //i = tate, y = yoko
        return element[j+i*V];
    }

    Matrix!(T,U,V) opIndexAssign(Matrix!(T,U,V) m) {
        this.element[] = m.element[];
        return this;
    }

    T opIndexAssign(T value, size_t i, size_t j) {
        return element[j+i*V] = value;
    }

    T opIndexOpAssign(string op)(T value, size_t i, size_t j) {
        mixin("return element[j+i*V] " ~ op ~ "= value;");
    }

    T[U*V] array() inout {
        return element;
    }

    Vector!(T,U)[V] column() const {
        Vector!(T,U)[V] result;
        mixin(getColumnCode(U,V));
        return result;
    }

    Vector!(T,V)[U] row() {
        Vector!(T,V)[U] result;
        mixin(getRowCode(U,V));
        return result;
    }

    static Matrix!(T,V,U) transpose(Matrix m) {

        mixin({
            string result = "Matrix!(T,V,U) r;";
            foreach (i;0..V) {
                foreach (j;0..U) {
                    result ~= "r[" ~ to!string(i) ~ "," ~ to!string(j) ~ "] = ";
                    result ~= "m[" ~ to!string(j) ~ "," ~ to!string(i) ~ "];";
                }
                }
                result ~= "return r;";
                return result;
        }());
    }

    string toString(T epsilon = 0) inout {
        string res;
        foreach (i; 0..U) {
            res ~= "\n\t";
            foreach (j; 0..V) {
                res ~= format!"%10f,"(this[i,j]);
            }
        }
        return res;
    }

    static if (U == V) {
        static Matrix identity() {
            Matrix result;
            mixin(getidentityCode(U));
            return result;
        }

        static auto translate(int S)(Vector!(T, S) vec) if (U == 4 && (S == 3 || S == 4) || U == S) {
            Matrix result;
            mixin(getTranslationCode(U));
            return result;
        }

        static Matrix scale(int S)(Vector!(T, S) vec) if (U == 4 && (S == 3 || S == 4) || U == S) {
            Matrix result;
            mixin(getScaleCode(U));
            return result;
        }

        static if (U == 3) {
            static Matrix axisAngle(Vector!(T,3) v, Angle angle) {
                auto c = cos(angle);
                auto s = sin(angle);
                Matrix result;
                mixin(getRotAxisCode(U));
                return result;
            }
            static Matrix rotFromTo(Vector!(T,3) from, Vector!(T,3) to) {
                auto v = cross(from, to);
                auto s = v.length;
                if (s < 1e-5) return identity();
                auto l = v.length;
                auto rad = asin(l < -1 ? -1 : l > 1 ? 1 : l);
                auto c = cos(rad);
                v = -normalize(v);
                Matrix result;
                mixin(getRotAxisCode(3));
                return result;
            }

            Quaternion!T toQuaternion() {
                import std.math : sqrt, sgn;
                auto q0 = ( this[0,0] + this[1,1] + this[2,2] + 1.0f) / 4.0f;
                auto q1 = ( this[0,0] - this[1,1] - this[2,2] + 1.0f) / 4.0f;
                auto q2 = (-this[0,0] + this[1,1] - this[2,2] + 1.0f) / 4.0f;
                auto q3 = (-this[0,0] - this[1,1] + this[2,2] + 1.0f) / 4.0f;
                if(q0 < 0.0f) q0 = 0.0f;
                if(q1 < 0.0f) q1 = 0.0f;
                if(q2 < 0.0f) q2 = 0.0f;
                if(q3 < 0.0f) q3 = 0.0f;
                q0 = sqrt(q0);
                q1 = sqrt(q1);
                q2 = sqrt(q2);
                q3 = sqrt(q3);
                if(q0 >= q1 && q0 >= q2 && q0 >= q3) {
                    q1 *= sgn(this[2,1] - this[1,2]);
                    q2 *= sgn(this[0,2] - this[2,0]);
                    q3 *= sgn(this[1,0] - this[0,1]);
                } else if(q1 >= q0 && q1 >= q2 && q1 >= q3) {
                    q0 *= sgn(this[2,1] - this[1,2]);
                    q2 *= sgn(this[1,0] + this[0,1]);
                    q3 *= sgn(this[0,2] + this[2,0]);
                } else if(q2 >= q0 && q2 >= q1 && q2 >= q3) {
                    q0 *= sgn(this[0,2] - this[2,0]);
                    q1 *= sgn(this[1,0] + this[0,1]);
                    q3 *= sgn(this[2,1] + this[1,2]);
                } else if(q3 >= q0 && q3 >= q1 && q3 >= q2) {
                    q0 *= sgn(this[1,0] - this[0,1]);
                    q1 *= sgn(this[2,0] + this[0,2]);
                    q2 *= sgn(this[2,1] + this[1,2]);
                } else {
                    assert(false);
                }

                auto result = Quaternion!T(q1, q2, q3, q0);
                return result.normalize;
            }

            vec3 getScale() {
                auto rows = this.row();
                return vec3(rows[0].length,
                        rows[1].length,
                        rows[2].length);
            }
        } else static if (U == 4) {

            static Matrix axisAngle(Vector!(T,3) v, Radian angle) {
                auto c = cos(angle);
                auto s = sin(angle);
                Matrix result;
                mixin(getRotAxisCode(4));
                return result;
            }
            static Matrix rotFromTo(Vector!(T,3) from, Vector!(T,3) to) {
                auto v = cross(from, to);
                auto s = v.length * (dot(from, to)<0 ? 1: -1);
                if (s == 0) return identity();
                auto rad = asin(v.length);
                auto c = cos(rad);
                v = normalize(v);
                Matrix result;
                mixin(getRotAxisCode(4));
                return result;
            }

            static Matrix lookAt(Vector!(T,3) eye, Vector!(T,3) vec, Vector!(T,3) up) {
                Vector!(T,3) side = normalize(cross(up, vec));
                up = normalize(cross(vec, side));
                alias vec4 = Vector!(T, 4);
                return Matrix(vec4(side, 0), vec4(up, 0), vec4(vec, 0), vec4(0,0,0,1));
            }

            static Matrix ortho(T width, T height, T nearZ, T farZ) {
                return Matrix(
                        2 / width, 0,          0,                  0,
                        0,         2 / height, 0,                  0,
                        0,         0,          1 / (nearZ - farZ), nearZ / (nearZ - farZ),
                        0,         0,          0,                  1
                        );
            }

            static Matrix perspective(T aspectWperH, Radian fovy, T nearZ, T farZ) {
                return Matrix(
                        1 / (aspectWperH * tan(fovy/2)), 0,                 0,                         0,
                        0,                               1 / (tan(fovy/2)), 0,                         0,
                        0,                               0,                 (nearZ+farZ)/(nearZ-farZ), 2 * nearZ * farZ / (nearZ - farZ),
                        0,                               0,                 -1,                        0
                        );
            }

            static Matrix makeTRS(Vector!(T,3) pos, Matrix!(T,3,3) rot, Vector!(T, 3) scale) {
                return Matrix(
                        scale.x * rot[0,0], scale.y * rot[0,1], scale.z * rot[0,2], pos.x,
                        scale.x * rot[1,0], scale.y * rot[1,1], scale.z * rot[1,2], pos.y,
                        scale.x * rot[2,0], scale.y * rot[2,1], scale.z * rot[2,2], pos.z,
                        0,0,0, 1);
            }
            static Matrix makeInvertTRS(Vector!(T,3) pos, Matrix!(T,3,3) rot, Vector!(T, 3) scale) {
                auto column = rot.column;
                return Matrix(
                        rot[0,0] / scale.x, rot[1,0] / scale.x, rot[2,0] / scale.x, -dot(column[0], pos) / scale.x,
                        rot[0,1] / scale.y, rot[1,1] / scale.y, rot[2,1] / scale.y, -dot(column[1], pos) / scale.y,
                        rot[0,2] / scale.z, rot[1,2] / scale.z, rot[2,2] / scale.z, -dot(column[2], pos) / scale.z,
                        0,0,0, 1);
            }

            Matrix!(T,3,3) toMatrix3() const {
                return Matrix!(T,3,3)(element[0..3],element[4..7],element[8..11]);
            }

            vec3 getScale() {
                return toMatrix3.getScale;
            }

            vec3 getTranslation() {
                return vec3(this[0,3], this[1,3], this[2,3]);
            }

        }

        static T determinant(Matrix m) {
            static if (U == 4 && V  == 4) {
                auto e2233_2332 = m[2,2] * m[3,3] - m[2,3] * m[3,2];
                auto e2133_2331 = m[2,1] * m[3,3] - m[2,3] * m[3,1];
                auto e2132_2231 = m[2,1] * m[3,2] - m[2,2] * m[3,1];
                auto e1233_1332 = m[1,2] * m[3,3] - m[1,3] * m[3,2];
                auto e1133_1331 = m[1,1] * m[3,3] - m[1,3] * m[3,1];
                auto e1132_1231 = m[1,1] * m[3,2] - m[1,2] * m[3,1];
                auto e1322_1223 = m[1,3] * m[2,2] - m[1,2] * m[2,3];
                auto e1123_1321 = m[1,1] * m[2,3] - m[1,3] * m[2,1];
                auto e1122_1221 = m[1,1] * m[2,2] - m[1,2] * m[2,1];
                auto e2033_2330 = m[2,0] * m[3,3] - m[2,3] * m[3,0];
                auto e2032_2230 = m[2,0] * m[3,2] - m[2,2] * m[3,0];
                auto e1033_1330 = m[1,0] * m[3,3] - m[1,3] * m[3,0];
                auto e1032_1230 = m[1,0] * m[3,2] - m[1,2] * m[3,0];
                auto e1023_1320 = m[1,0] * m[2,3] - m[1,3] * m[2,0];
                auto e1022_1220 = m[1,0] * m[2,2] - m[1,2] * m[2,0];
                auto e2031_2130 = m[2,0] * m[3,1] - m[2,1] * m[3,0];
                auto e1031_1130 = m[1,0] * m[3,1] - m[1,1] * m[3,0];
                auto e1021_1120 = m[1,0] * m[2,1] - m[1,1] * m[2,0];
                return
                    m[0,0] * (m[1,1] * e2233_2332 - m[1,2] * e2133_2331 + m[1,3] * e2132_2231) -
                    m[0,1] * (m[1,0] * e2233_2332 - m[1,2] * e2033_2330 + m[1,3] * e2032_2230) +
                    m[0,2] * (m[1,0] * e2133_2331 - m[1,1] * e2033_2330 + m[1,3] * e2031_2130) -
                    m[0,3] * (m[1,0] * e2132_2231 - m[1,1] * e2032_2230 + m[1,2] * e2031_2130)
                ;
            } else static if (U == 3 && V == 3) {
                return
                 + m[0,0]*m[1,1]*m[2,2]
                 + m[0,1]*m[1,2]*m[2,0]
                 + m[0,2]*m[1,0]*m[2,1]
                 - m[0,0]*m[1,2]*m[2,1]
                 - m[0,1]*m[1,0]*m[2,2]
                 - m[0,2]*m[1,1]*m[2,0];
            } else static if (U == 2 && V == 2) {
                return m[0,0]*m[1,1] - m[0,1]*m[1,0];
            }
        }

        static Matrix invert(Matrix m) {
            static if (U == 4 && V == 4) {
                auto e2233_2332 = m[2,2] * m[3,3] - m[2,3] * m[3,2];
                auto e2133_2331 = m[2,1] * m[3,3] - m[2,3] * m[3,1];
                auto e2132_2231 = m[2,1] * m[3,2] - m[2,2] * m[3,1];
                auto e1233_1332 = m[1,2] * m[3,3] - m[1,3] * m[3,2];
                auto e1133_1331 = m[1,1] * m[3,3] - m[1,3] * m[3,1];
                auto e1132_1231 = m[1,1] * m[3,2] - m[1,2] * m[3,1];
                auto e1322_1223 = m[1,3] * m[2,2] - m[1,2] * m[2,3];
                auto e1123_1321 = m[1,1] * m[2,3] - m[1,3] * m[2,1];
                auto e1122_1221 = m[1,1] * m[2,2] - m[1,2] * m[2,1];
                auto e2033_2330 = m[2,0] * m[3,3] - m[2,3] * m[3,0];
                auto e2032_2230 = m[2,0] * m[3,2] - m[2,2] * m[3,0];
                auto e1033_1330 = m[1,0] * m[3,3] - m[1,3] * m[3,0];
                auto e1032_1230 = m[1,0] * m[3,2] - m[1,2] * m[3,0];
                auto e1023_1320 = m[1,0] * m[2,3] - m[1,3] * m[2,0];
                auto e1022_1220 = m[1,0] * m[2,2] - m[1,2] * m[2,0];
                auto e2031_2130 = m[2,0] * m[3,1] - m[2,1] * m[3,0];
                auto e1031_1130 = m[1,0] * m[3,1] - m[1,1] * m[3,0];
                auto e1021_1120 = m[1,0] * m[2,1] - m[1,1] * m[2,0];
                auto det =
                    m[0,0] * (m[1,1] * e2233_2332 - m[1,2] * e2133_2331 + m[1,3] * e2132_2231) -
                    m[0,1] * (m[1,0] * e2233_2332 - m[1,2] * e2033_2330 + m[1,3] * e2032_2230) +
                    m[0,2] * (m[1,0] * e2133_2331 - m[1,1] * e2033_2330 + m[1,3] * e2031_2130) -
                    m[0,3] * (m[1,0] * e2132_2231 - m[1,1] * e2032_2230 + m[1,2] * e2031_2130)
                ;
                if (det != 0) det = 1 / det;
                auto t00 =  m[1,1] * e2233_2332 - m[1,2] * e2133_2331 + m[1,3] * e2132_2231;
                auto t01 = -m[0,1] * e2233_2332 + m[0,2] * e2133_2331 - m[0,3] * e2132_2231;
                auto t02 =  m[0,1] * e1233_1332 - m[0,2] * e1133_1331 + m[0,3] * e1132_1231;
                auto t03 =  m[0,1] * e1322_1223 + m[0,2] * e1123_1321 - m[0,3] * e1122_1221;
                auto t10 = -m[1,0] * e2233_2332 + m[1,2] * e2033_2330 - m[1,3] * e2032_2230;
                auto t11 =  m[0,0] * e2233_2332 - m[0,2] * e2033_2330 + m[0,3] * e2032_2230;
                auto t12 = -m[0,0] * e1233_1332 + m[0,2] * e1033_1330 - m[0,3] * e1032_1230;
                auto t13 = -m[0,0] * e1322_1223 - m[0,2] * e1023_1320 + m[0,3] * e1022_1220;
                auto t20 =  m[1,0] * e2133_2331 - m[1,1] * e2033_2330 + m[1,3] * e2031_2130;
                auto t21 = -m[0,0] * e2133_2331 + m[0,1] * e2033_2330 - m[0,3] * e2031_2130;
                auto t22 =  m[0,0] * e1133_1331 - m[0,1] * e1033_1330 + m[0,3] * e1031_1130;
                auto t23 = -m[0,0] * e1123_1321 + m[0,1] * e1023_1320 - m[0,3] * e1021_1120;
                auto t30 = -m[1,0] * e2132_2231 + m[1,1] * e2032_2230 - m[1,2] * e2031_2130;
                auto t31 =  m[0,0] * e2132_2231 - m[0,1] * e2032_2230 + m[0,2] * e2031_2130;
                auto t32 = -m[0,0] * e1132_1231 + m[0,1] * e1032_1230 - m[0,2] * e1031_1130;
                auto t33 =  m[0,0] * e1122_1221 - m[0,1] * e1022_1220 + m[0,2] * e1021_1120;
                Matrix r;
                r[0,0] =  det * t00;
                r[0,1] =  det * t01;
                r[0,2] =  det * t02;
                r[0,3] =  det * t03;
                r[1,0] =  det * t10;
                r[1,1] =  det * t11;
                r[1,2] =  det * t12;
                r[1,3] =  det * t13;
                r[2,0] =  det * t20;
                r[2,1] =  det * t21;
                r[2,2] =  det * t22;
                r[2,3] =  det * t23;
                r[3,0] =  det * t30;
                r[3,1] =  det * t31;
                r[3,2] =  det * t32;
                r[3,3] =  det * t33;
                return r;
            }
            static if (U == 3 && V == 3) {
                auto det =
                     + m[0,0]*m[1,1]*m[2,2]
                 + m[0,1]*m[1,2]*m[2,0]
                 + m[0,2]*m[1,0]*m[2,1]
                 - m[0,0]*m[1,2]*m[2,1]
                 - m[0,1]*m[1,0]*m[2,2]
                 - m[0,2]*m[1,1]*m[2,0];
                if (det != 0) det = 1 / det;
                Matrix r;
                r[0,0] = (m[1,1]*m[2,2] - m[1,2]*m[2,1]) * det;
                r[0,1] = (m[0,2]*m[2,1] - m[0,1]*m[2,2]) * det;
                r[0,2] = (m[0,1]*m[1,2] - m[0,2]*m[1,1]) * det;
                r[1,0] = (m[1,2]*m[2,0] - m[1,0]*m[2,2]) * det;
                r[1,1] = (m[0,0]*m[2,2] - m[0,2]*m[2,0]) * det;
                r[1,2] = (m[0,2]*m[1,0] - m[0,0]*m[1,2]) * det;
                r[2,0] = (m[1,0]*m[2,1] - m[1,1]*m[2,0]) * det;
                r[2,1] = (m[0,1]*m[2,0] - m[0,0]*m[2,1]) * det;
                r[2,2] = (m[0,0]*m[1,1] - m[0,1]*m[1,0]) * det;
                return r;
            }
            static if (U == 2 && V == 2) {
                auto det = m[0,0]*m[1,1] - m[0,1]*m[1,0];
                if (det != 0) det = 1 / det;
                Matrix r;
                r[0,0] = +m[1,1] * det;
                r[0,1] = -m[0,1] * det;
                r[1,0] = -m[1,0] * det;
                r[1,1] = +m[0,0] * det;
                return r;
            }
            assert(false);
        }

        //static Matrix!(T, U, V) diagnalizate(Matrix m) {
        static Matrix!(T, U, V) diagonalize(Matrix m) {
            // すべて実数固有値で全部相違であると仮定しています
            // ＞ めっちゃ条件付き ＜

            // hessenberg
            import std.math : abs, sqrt;
            Matrix!(T,U,V) H;
            for(int i=0;i<U;++i)for(int j=0;j<V;++j)
                    H[i,j] = m[i,j];
            H.writeln;
            for(int k=1;k<=U-2;++k){
                T[U] u;
                u[] = 0;
                for(int i=k;i<U;++i)
                    u[i] = H[i,k-1];
                T ss = 0.0;
                for(int i=k+1;i<U;++i)
                    ss += u[i]*u[i];
                if(abs(ss)<=0.0) continue;
                T s = sqrt(ss+u[k]*u[k]);
                if(u[k]>0.0) s = -s;
                u[k] -= s;
                T uu = sqrt(ss+u[k]*u[k]);
                for(int i=k;i<U;++i)
                    u[i] /= uu;
                T[U] f,g;
                f[] = 0;
                g[] = 0;
                for(int i=0;i<U;++i)
                    for(int j=k;j<U;++j){
                        f[i] += H[i,j]*u[j];
                        g[i] += H[j,i]*u[j];
                    }
                T gamma = 0.0;
                for(int i=0;i<U;++i)
                    gamma += u[i]*g[i];
                for(int i=0;i<U;++i)
                    f[i] -= gamma*u[i], g[i] -= gamma*u[i];
                for(int i=0;i<U;++i)
                    for(int j=0;j<U;++j)
                        H[i,j] -= 2.0*u[i]*g[j] + 2.0*f[i]*u[j];
            }
            // QR method
            T[U] sn,cs;
            const T EPS = 1e-12;
            for(int n=U;n>=2;){
                if(abs(H[n-1,n-2])<EPS){
                    --n; continue;
                }
                T shift = H[n-1,n-1];
                for(int i=0;i<n;++i)
                    H[i,i] -= shift;
                for(int k=0;k<n-1;++k){
                    T a = H[k,k];
                    T b = H[k+1,k];
                    T r = sqrt(a*a+b*b);
                    sn[k] = (r==0.0 ? 0.0 : b/r);
                    cs[k] = (r==0.0 ? 0.0 : a/r);
                    for(int j=k;j<n;++j){
                        T x = H[k,j], y = H[k+1,j];
                        H[k,j] = cs[k]*x + sn[k]*y;
                        H[k+1,j] = -sn[k]*x + cs[k]*y;
                    }
                }
                for(int k=0;k<n-1;++k){
                    for(int i=0;i<=k+1;++i){
                        T x = H[i,k], y = H[i,k+1];
                        H[i,k] = cs[k]*x + sn[k]*y;
                        H[i,k+1] = -sn[k]*x + cs[k]*y;
                    }
                }
                for(int i=0;i<n;++i)
                    H[i,i] += shift;
            }
            T[U] lambda;
            for(int i=0;i<U;++i)
                lambda[i]=H[i,i];
            lambda.writeln;
            // 未完
            // TODO: 同じ固有値の検出、連立方程式の求解
            return identity;
        }
        // こっち使って :sobaya:
        static Matrix!(T,U,V) diagonalizeForRealSym(Matrix m){
            auto result = identity;

            T getMaxValue(ref Matrix m, out uint p, out uint q) {
                T max = 0;
                foreach (i; 0..U) {
                    foreach (j; 0..V) {
                        import std.math : abs;
                        if (i == j) continue;
                        if (m[i,j].abs > max) {
                            max = m[i,j].abs;
                            p = i;
                            q = j;
                        }
                    }
                }
                return max;
            }
            T max;
            uint p, q;
            uint bp = 114514, bq = 114514;
            while (true) {
                import std.math : abs, sqrt;
                if ((max = getMaxValue(m, p, q)) < 1e-3) break;
                if (p == bp && q == bq) break;
                T pp = m[p,p];
                T pq = m[p,q];
                T qq = m[q,q];
                T alpha = (pp - qq) / 2.0;
                T beta = -pq;
                T gamma = abs(alpha) / sqrt(alpha*alpha+beta*beta);
                T s = sqrt((1.0-gamma) / 2.0);
                T c = sqrt((1.0+gamma) / 2.0);
                if (alpha * beta < 0) s = -s;
                foreach (i; 0..U) {
                    T tmp = c * m[p, i] - s * m[q, i];
                    m[q, i] = s * m[p, i] + c * m[q, i];
                    m[p, i] = tmp;
                }
                foreach (i; 0..U) {
                    m[i,p] = m[p,i];
                    m[i,q] = m[q,i];
                }
                m[p,p] = c*c*pp + s*s*qq - 2*s*c*pq;
                m[p,q] = s*c*(pp-qq) + (c*c-s*s) * pq;
                m[q,p] = m[p,q];
                m[q,q] = s*s*pp + c*c*qq + 2*s*c*pq;
                foreach (i; 0..U) {
                    T tmp = c*result[i,p]-s*result[i,q];
                    result[i,q] = s*result[i,p] + c*result[i,q];
                    result[i,p] = tmp;
                }
                bp = p;
                bq = q;
            }
            return result;
        }
    }
}


//============================================================================以下mixin用の関数達

private static string multMMCode(uint U, uint V, uint P, uint Q) {
    string code;
    foreach (i; 0..U) {
        foreach (j; 0..V) {
            code ~= format!"result[%d,%d] = "(i,j);
            foreach (k; 0..V) {
                code ~= format!"+ this[%d,%d] * m[%d,%d]"(i,k,k,j);
            }
            code ~= ";";
        }
    }
    return code;
}

private static string multMMRightCode(uint U, uint V, uint P, uint Q) {
    string code;
    foreach (i; 0..U) {
        foreach (j; 0..V) {
            code ~= format!"result[%d,%d] = "(i,j);
            foreach (k; 0..V) {
                code ~= format!"+ m[%d,%d] * this[%d,%d]"(i,k,k,j);
            }
            code ~= ";";
        }
    }
    return code;
}

private static string multMVCode(uint U, uint V) {
    string code;
    foreach (i; 0..U) {
        code ~= format!"result[%d] = "(i);
        foreach (j; 0..V) {
            code ~= format!"+this[%d, %d] * v[%d]"(i,j,j);
        }
        code ~= ";";
    }
    return code;
}

private static string getidentityCode(uint U) {
    string code;
    foreach (i; 0..U) {
        foreach (j; 0..U) {
            code ~= format!"result[%d,%d] = "(i,j);
            if (i == j) code ~= "1;";
            else code ~= "0;";
        }
    }
    return code;
}

private static string getTranslationCode(uint U) {
    string code;
    foreach (i; 0..U) {
        foreach (j; 0..U) {
            code ~= format!"result[%d,%d] = "(i,j);
            if (i == j)
                code ~= "1;";
            else if (j == U-1)
                code ~= format!"vec[%d];"(i);
            else
                code ~= "0;";
        }
    }
    return code;
}

private static string addMat4(string code) {
    foreach (i; 0..3) {
        code ~= format!"result[%d,3] = 0;"(i);
        code ~= format!"result[3,%d] = 0;"(i);
    }
    code ~= "result[3,3] = 1;";
    return code;
}

private static string getScaleCode(uint U) {
    string code;
    foreach (i; 0..U) {
        foreach (j; 0..U) {
            code ~= format!"result[%d,%d] = "(i,j);
            if (i == j)
                code ~= format!"vec[%d];"(i);
            else
                code ~= "0;";
        }
    }
    if (U == 4) code = addMat4(code);
    return code;
}

private static string getRotAxisCode(uint U) {
    string code;
    foreach (i; 0..3) {
        foreach (j; 0..3) {
            code ~= format!"result[%d,%d] = "(i,j);
            if (i == j)
                code ~= format!"v[%d]*v[%d]*(1-c)+c;"(i,j);
            else if (j == (i+1)%3)
                code ~= format!"v[%d]*v[%d]*(1-c)+v[%d]*s;"(i,j,(i+2)%3);
            else
                code ~= format!"v[%d]*v[%d]*(1-c)-v[%d]*s;"(i,j,(i+1)%3);
        }
    }
    if (U == 4) code = addMat4(code);
    return code;
}

private static string getOpBinaryMSCode(string op, uint U, uint V) {
    string code;
    foreach (i; 0..U) {
        foreach (j; 0..V) {
            code ~= format!"result[%d,%d] = this[%d,%d] %s s;"(i,j,i,j,op);
        }
    }
    return code;
}

private static string getOpAssignMSCode(string op, uint U, uint V) {
    string code;
    foreach (i; 0..U) {
        foreach (j; 0..V) {
            code ~= format!"this[%d,%d] = this[%d,%d] %s s;"(i,j,i,j,op);
        }
    }
    return code;
}

private static string getOpBinaryMMCode(string op, uint U, uint V) {
    string code;
    foreach (i; 0..U) {
        foreach (j; 0..V) {
            code ~= format!"result[%d,%d] = this[%d,%d] %s m[%d,%d];"(i,j,i,j,op,i,j);
        }
    }
    return code;
}

private static string getOpAssignMMCode(string op, uint U, uint V) {
    string code;
    foreach (i; 0..U) {
        foreach (j; 0..V) {
            code ~= format!"this[%d,%d] = this[%d,%d] %s m[%d,%d];"(i,j,i,j,op,i,j);
        }
    }
    return code;
}

private static string getCopyCode(string identifier, uint U, uint V) {
    string code;
    foreach (x; 0..U) {
        foreach (y; 0..V) {
            code ~= "this[" ~ to!string(x) ~ "," ~ to!string(y) ~ "]
            = " ~ identifier ~ "[" ~ to!string(x) ~ "," ~ to!string(y) ~ "];";
        }
    }
    return code;
}

private static string getColumnCode(uint U, uint V) {
    string code;
    foreach (j; 0..V) {
        foreach (i; 0..U) {
            code ~= format!"result[%d][%d] = this[%d, %d];"(j, i, i, j);
        }
    }
    return code;
}

private static string getRowCode(uint U, uint V) {
    string code;
    foreach (i; 0..U) {
        foreach (j; 0..V) {
            code ~= format!"result[%d][%d] = this[%d, %d];"(i, j, i, j);
        }
    }
    return code;
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
