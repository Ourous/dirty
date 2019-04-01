implementation module Dirty.Backend.Value

import Dirty.Backend
import Data.Maybe
import Text.GenParse
import StdClass, StdOverloaded, StdBool, StdInt, StdReal, StdEnv

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
	(<) (Num _) (Stk _) = False
	(<) (Stk _) (Num _) = True
	(<) (Stk lhs) (Stk rhs) = lhs < rhs
	
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
instance toValue Value
where toValue val = val
//instance toValue a
//where toValue val = Group (toStack val)

instance toCharList Value where
	toCharList (Num num) = undef//toCharList num
	toCharList (Stk stk) = undef//toCharList stk
	
instance repr Value where
	repr inf (Num num) = repr inf num
	repr inf (Stk stk) = ['['] ++ repr inf stk ++ [']']

instance eval Value where
	eval val = undef/*case val of
		['[':_]
			# try = eval val
			| isJust try = Just (Stk (fromJust try))
		_
			= case eval val of
				(Just num) = (Just (Num num))
				_ = Nothing*/
				
instance disp Value where
	disp (Num num) = disp num
	disp (Stk stk) = disp stk

vectorizeLeft :: (Number Value -> a) -> (Value Value -> Value) | toValue a
vectorizeLeft fn = op
where
	op (Num lhs) rhs = toValue (fn lhs rhs)
	op (Stk lhs) rhs = toValue (S_map (flip op rhs) lhs)

vectorizeRight :: (Value Number -> a) -> (Value Value -> Value) | toValue a
vectorizeRight fn = op
where
	op lhs (Num rhs) = toValue (fn lhs rhs)
	op lhs (Stk rhs) = toValue (S_map (op lhs) rhs)
	
vectorizeFull :: (Number Number -> a) -> (Value Value -> Value) | toValue a
vectorizeFull fn = op
where
	op (Num lhs) (Num rhs) = toValue (fn lhs rhs)
	op lhs=:(Num _) (Stk rhs) = toValue (S_map (op lhs) rhs)
	op (Stk lhs) rhs=:(Num _) = toValue (S_map (flip op rhs) lhs)
	op (Stk lhs) (Stk rhs) = toValue (S_zipWith op lhs rhs)



