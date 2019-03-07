implementation module Dirty.Backend.Value

import Dirty.Backend.Number, Dirty.Backend.Stack
import Data.Maybe
import Text.GenParse
import StdClass, StdOverloaded, StdBool, StdInt, StdReal

instance fromInt Value
where fromInt val = Num (fromInt val)

instance fromReal Value
where fromReal val = Num (fromReal val)

instance fromBool Value
where fromBool val = Num (fromBool val)

tryParseValue :: String -> Maybe Value
tryParseValue str
	# val = mapMaybe fromInt (gParse{|*|} expr)
	| isJust val = val
	# val = mapMaybe fromReal (gParse{|*|} expr)
	| isJust val = val
	# val = mapMaybe fromBool (gParse{|*|} expr)
	= val
	// TODO: strings
	// TODO: nested stacks
where expr = preParseString str

instance toBool Value
where
	toBool (Num num) = toBool num
	toBool (Stk grp) = toBool grp
	
instance < Value
where
	(<) (Num lhs) (Num rhs) = lhs < rhs
	
instance == Value
where
	(==) (Num lhs) (Num rhs) = lhs == rhs
	
instance isInfinite Value
where
	isInfinite (Num num) = isInfinite num
	isInfinite _ = False
instance isRational Value
where
	isRational (Num num) = isRational num
	isRational _ = False
instance isImaginary Value
where
	isImaginary (Num num) = isImaginary num
	isImaginary _ = False
instance isInvalid Value
where
	isInvalid (Num num) = isInvalid num
	isInvalid _ = False
	
class toValue a :: a -> Value
instance toValue Int
where toValue val = Num (fromInt val)
instance toValue Real
where toValue val = Num (fromReal val)
instance toValue Char
where toValue val = Num (fromInt (toInt val))
instance toValue Number
where toValue val = Num val
instance toValue Stack
where toValue val = Stk val
//instance toValue a
//where toValue val = Group (toStack val)