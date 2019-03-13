definition module Dirty.Backend.Value

from Dirty.Backend.Number import ::Number, class isInfinite, class isRational,
                                 class isImaginary, class isInvalid
from Dirty.Backend.Stack import ::Stack, class toStack
from Dirty.Backend import class repr
from Data.Maybe import ::Maybe
from StdOverloaded import class toBool, class <, class ==, class toString

//:: Item :== Number
:: Value
	= Num Number
	| Stk Stack

tryParseValue :: String -> Maybe Value

instance toBool Value
instance < Value
instance == Value
//instance toString Value

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
instance toValue Value
//instance toValue a


instance repr Value

isNumber val :== case val of (Num _) = True; _ = False
isStack val :== case val of (Stk _) = True; _ = False

appN fn val :== case val of (Num v) = toValue (fn v); _ = val
appS fn val :== case val of (Stk v) = toValue (fn v); _ = val
appV fnN fnS val :== case val of (Num n) = (fnN n); (Stk s) = (fnS s)

vectorizeLeft :: (Number Value -> a) -> (Value Value -> Value) | toValue a
vectorizeRight :: (Value Number -> a) -> (Value Value -> Value) | toValue a
vectorizeFull :: (Number Number -> a) -> (Value Value -> Value) | toValue a
//vectorizeFull fn lhs rhs :== vectorizeRight (vectorizeLeft fn lhs) rhs