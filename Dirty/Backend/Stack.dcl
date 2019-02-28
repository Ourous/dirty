definition module Dirty.Backend.Stack

from Dirty.Backend.Number import ::Number
from Dirty.Backend.Value import ::Value
from StdOverloaded import class zero, class +++
	
:: Stack = {
	length :: Number,
	attrs :: StackAttrs,
	list :: [!Value]
	}
	
:: StackAttrs = {
	hasStacks :: Bool,
	hasNumbers :: Bool,
	hasReals :: Bool,
	hasImaginarys :: Bool,
	hasComplexes :: Bool
	}
	
instance zero Stack
instance +++ Stack

instance zero StackAttrs

fromValue :: Value -> Stack