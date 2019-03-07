implementation module Dirty.Backend.Stack

import Dirty.Backend.Number, Dirty.Backend.Value
import Data.Maybe
import StdEnv, StdOverloadedList, StdDebug

:: Stack
	= Head !Value Stack
	| Loop ![!Value!] Stack
	| Lazy ![!Value] Attrs Stack
	| Null

:: Attrs = {
	hasStk :: Maybe Bool,
	hasNum :: Maybe Bool,
	hasRe :: Maybe Bool,
	hasIm :: Maybe Bool,
	hasCx :: Maybe Bool,
	hasNaN :: Maybe Bool,
	minVal :: Maybe Number,
	maxVal :: Maybe Number,
	sorted :: Maybe Bool
	}
	
instance zero Stack
where zero = Null

instance +++ Stack
where
	(+++) Null Null = Null
	(+++) Null rhs = rhs
	(+++) lhs Null = lhs
	//(+++) lhs=:(Lazy _ _) _ = lhs // probably okay to ignore attributes here
	(+++) (Head val lhs) rhs = (Head val (lhs +++ rhs))

instance toBool Stack
where
	toBool Null = False
	toBool stack = toBool (peek stack)
	

instance fromString Stack
where
	fromString str = toStack [c \\ c <-: str]

instance zero Attrs
where zero = {
		hasStk=Nothing,
		hasNum=Nothing,
		hasRe=Nothing,
		hasIm=Nothing,
		hasCx=Nothing,
		hasNaN=Nothing,
		minVal=Nothing,
		maxVal=Nothing,
		sorted=Nothing
		}
		
class toStack a :: a -> Stack
instance toStack [Char]
where
	toStack [] = Null
	toStack [h:t] = (Head (toValue h) (toStack t))
	
instance toStack Value
where
	toStack v = (Head v Null)
/*	
instance toStack [!a] | toValue a
where
	toStack [!a]
instance toStack [!a!] | toValue a
		*/
fromValue :: Value -> Stack
fromValue val = (Head val Null)
	
push :: Value Stack -> Stack
push val stack = (Head val stack)
		
pop :: Stack -> (Value, Stack)
pop Null = abort "Fatal: cannot pop null"
pop (Head val stack) = (val, stack)

peek :: Stack -> Value
peek Null = abort "Fatal: cannot peek null"
peek (Head val _) = val

// misc builtins
S_sort :: Stack -> Stack
S_sort Null = Null
S_sort stack=:(Head _ Null) = stack
S_sort h=:(Head a t=:(Head b c))
	| a > b
		= (Head b (S_sort (Head a c)))
	| otherwise
		= (Head a (S_sort t))
	
//sort stack=:(More _ Null) = stack
//sort (Loop loop Null) = Foldr (\a b = (More a b)) Null loop

S_isSorted :: Stack -> Bool
S_isSorted _ = undef

S_reduce :: (Value -> Value -> Value) Value Stack -> Value
S_reduce _ st Null = st
S_reduce f st (Head h t) = S_reduce f (f st h) t
//reduce f st t=:(More h _) = S_reduce f (f st h) t
S_reduce f st t=:(Loop h _) = S_reduce f (Foldl f st h) t
S_reduce f st (Lazy h _ t) = S_reduce f (Foldl f st h) (trace_n "Error: access past end of infinite list" t)

S_map :: (Value -> Value) Stack -> Stack
S_map _ Null = Null
S_map f (Head h t) = (Head (f h) (S_map f t))
S_map f (Loop h t) = (Loop (Map f h) (S_map f t))
S_map f (Lazy h _ t) = (Lazy (Map f h) zero (trace_n "Error: access past end of infinite list" t))
//reverse :: Stack -> Stack
S_indexOf :: Stack Value -> Value
S_indexOf _ _ = undef
S_valueAt :: Stack Value -> Value
S_valueAt _ _ = undef

