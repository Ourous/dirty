definition module Dirty.Backend.Number

from Dirty.Backend.Rational import ::Rational
from Dirty.Backend import class repr, class eval, class disp

from StdOverloaded import class +, class -, class *, class /, class ^, class ~,
                          class ==, class <, class zero, class one, class abs,
                          class mod, class gcd, class lcm, class toInt,
                          class toReal, class toBool, class toString,
                          class fromInt, class fromReal, class fromBool, class fromString,
                          class ln, class log10, class exp, class sqrt,
                          class sin, class cos, class tan, class asin,
                          class acos, class atan


:: Number

one_i :: Number

instance + Number
instance - Number
instance zero Number
instance * Number
instance / Number
instance one Number
instance ^ Number
instance abs Number
instance ~ Number
instance == Number
instance < Number
instance mod Number
instance gcd Number
instance lcm Number

instance toInt Number
instance toReal Number
instance toBool Number
instance toString Number

instance fromInt Number
instance fromReal Number
instance fromBool Number
instance fromString Number

instance repr Number
instance eval Number
instance disp Number

instance ln Number
instance log10 Number
instance exp Number
instance sqrt Number
instance sin Number
instance cos Number
instance tan Number
instance asin Number
instance acos Number
instance atan Number

bitOR :: !Number !Number -> Number
bitAND :: !Number !Number -> Number
bitXOR :: !Number !Number -> Number
bitNOT :: !Number -> Number

bitLEFT :: !Number !Number -> Number
bitRIGHT :: !Number !Number -> Number

numFloor :: !Number -> Number
numCeiling :: !Number -> Number
numRound :: !Number -> Number

toRadians :: !Number -> Number
toDegrees :: !Number -> Number

class isInfinite a :: a -> Bool
class isComplex a :: a -> Bool
class isRational a :: a -> Bool
class isImaginary a :: a -> Bool
class isInvalid a :: a -> Bool

instance isInfinite Number
instance isComplex Number
instance isRational Number
instance isImaginary Number
instance isInvalid Number

class toNumber a :: a -> Number
instance toNumber Int
instance toNumber Real
instance toNumber Bool
instance toNumber String