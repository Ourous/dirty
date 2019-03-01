definition module Dirty.Backend.Rational

from StdOverloaded import class +, class -, class *, class /, class ^, class ~,
                          class ==, class <, class one, class abs,
                          class sign, class mod, class gcd, class lcm,
                          class toInt, class toReal, class toString,
                          class fromInt, class fromReal, class fromString,
                          class ln, class log10, class exp, class sqrt,
                          class sin, class cos, class tan, class asin,
                          class acos, class atan, class zero

:: Rational

instance + Rational
instance - Rational
instance * Rational
instance / Rational
instance ^ Rational
instance abs Rational
instance sign Rational
instance ~ Rational
instance == Rational
instance < Rational
instance one Rational
instance zero Rational

instance toInt Rational
instance toReal Rational
instance toString Rational

instance fromInt Rational
instance fromReal Rational
instance fromString Rational

instance mod Rational
instance gcd Rational
instance lcm Rational
instance ln Rational
instance log10 Rational
instance exp Rational
instance sqrt Rational
instance sin Rational
instance cos Rational
instance tan Rational
instance asin Rational
instance acos Rational
instance atan Rational