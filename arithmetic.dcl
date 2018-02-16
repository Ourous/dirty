definition module arithmetic

import types
from StdOverloaded import class +, class -, class *, class /, class ^, class ~, class ==, class <, class zero, class one, class abs, class mod, class gcd, class lcm, class toInt, class toReal, class fromInt, class ln, class log10, class exp, class sqrt, class sin, class cos, class tan, class asin, class acos, class atan


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
instance fromInt Number

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

bitOR :: Number Number -> Number
bitAND :: Number Number -> Number
bitXOR :: Number Number -> Number
bitNOT :: Number Number -> Number
bitNOR :: Number Number -> Number
bitNAND :: Number Number -> Number
bitXNOR :: Number Number -> Number

numFLOOR :: Number -> Number
numCEILING :: Number -> Number
numROUND :: Number -> Number

toRadians :: Number -> Number
toDegrees :: Number -> Number