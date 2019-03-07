definition module Dirty.Backend.Stack

from Dirty.Backend.Number import ::Number
from Dirty.Backend.Value import ::Value, class toValue
from StdOverloaded import class zero, class +++, class toBool, class fromString
from Data.Maybe import ::Maybe
from _SystemStrictLists import class List
	
// TODO: make stack cons-list when not infinite, so element removal can update attributes
	
:: Stack

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
	
//:: Attrs
	
instance zero Stack
instance +++ Stack
instance toBool Stack

class toStack a :: a -> Stack
//instance toStack [a] | toValue a
instance toStack [Char]
instance toStack Value
//instance toStack [!a] | toValue a
//instance toStack [!a!] | toValue a

//toLazy :: (l a) Attrs -> Stack | List l a

fromValue :: Value -> Stack

push :: Value Stack -> Stack

pop :: Stack -> (Value, Stack)

peek :: Stack -> Value


// misc builtins
S_sort :: Stack -> Stack
S_isSorted :: Stack -> Bool

S_reduce :: (Value -> Value -> Value) Value Stack -> Value
S_map :: (Value -> Value) Stack -> Stack
//reverse :: Stack -> Stack
S_indexOf :: Stack Value -> Value
S_valueAt :: Stack Value -> Value