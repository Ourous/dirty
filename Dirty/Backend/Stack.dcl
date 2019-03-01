definition module Dirty.Backend.Stack

from Dirty.Backend.Number import ::Number
from Dirty.Backend.Value import ::Value, class toValue
from StdOverloaded import class zero, class +++, class toBool, class fromString
	
:: Stack = {
	length :: Number,
	attrs :: StackAttrs,
	list :: [!Value]
	}
	
:: StackAttrs = {
	hasStacks :: Bool,
	hasNumbers :: Bool,
	hasRationals :: Bool,
	hasImaginarys :: Bool,
	hasComplexes :: Bool
	}
	
instance zero Stack
instance +++ Stack
instance toBool Stack

instance zero StackAttrs

class toStack a :: a -> Stack
//instance toStack [a] | toValue a
instance toStack [Char]

fromValue :: Value -> Stack

prepend :: Value Stack -> Stack