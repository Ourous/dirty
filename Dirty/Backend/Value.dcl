definition module Dirty.Backend.Value

from Dirty.Backend.Number import ::Number, class isInfinite, class isRational,
                                 class isImaginary, class isInvalid
from Dirty.Backend.Stack import ::Stack, class toStack
from Data.Maybe import ::Maybe
from StdOverloaded import class toBool

//:: Item :== Number
:: Value
	= Num Number
	| Stk Stack

tryParseValue :: String -> Maybe Value

instance toBool Value

//instance isInfinite Value
//instance isRational Value
//instance isImaginary Value
//instance isInvalid Value

class toValue a :: a -> Value
instance toValue Int
instance toValue Real
instance toValue Char
instance toValue Number
instance toValue Stack
//instance toValue a

isNumber val :== case val of (Num _) = True; _ = False
isStack val :== case val of (Stk _) = True; _ = False