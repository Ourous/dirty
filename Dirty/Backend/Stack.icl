implementation module Dirty.Backend.Stack

import Dirty.Backend.Number, Dirty.Backend.Value
import Data.Maybe
import StdEnv, StdOverloadedList

instance zero Stack
where zero = Null

instance +++ Stack
where
	(+++) Null Null = Null
	(+++) Null rhs = rhs
	(+++) lhs Null = lhs
	(+++) lhs=:(Lazy _ _) _ = lhs // probably okay to ignore attributes here
	(+++) (Head val lhs) rhs = (Head val (lhs +++ rhs))

instance toBool Stack
where
	toBool Null = False
	toBool stack = toBool (peek stack)

instance fromString Stack
where
	fromString str = toStack [c \\ c <-: str]

instance zero StackAttrs
where zero = {
		hasStacks=False,
		hasNumbers=False,
		hasRationals=False,
		hasImaginarys=False,
		hasComplexes=False
		}
		
class toStack a :: a -> Stack
instance toStack [Char]
where
	toStack [] = Null
	toStack [h:t] = (Head (toValue h) (toStack t))
		
fromValue :: Value -> Stack
fromValue val = (Head val Null)
	
push :: Value Stack -> Stack
push val stack = (Head val stack)
		
pop :: Stack -> (Value, Stack)
pop Null = abort "Error: cannot pop null"
pop (Head val stack) = (val, stack)
pop (Lazy [|val:tail] attrs) = (val, (Lazy tail attrs))

peek :: Stack -> Value
peek Null = abort "Error: cannot peek null"
peek (Head val _) = val
peek (Lazy [|val:_] _) = val