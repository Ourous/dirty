definition module Dirty.Backend.Value

from Dirty.Backend.Number import ::Number
from Dirty.Backend.Stack import ::Stack
from Data.Maybe import ::Maybe
from StdOverloaded import class fromInt, class fromReal, class fromBool

:: Value
	= Item Number
	| Group Stack

//instance fromInt Value
//instance fromReal Value
//instance fromBool Value

tryParseValue :: String -> Maybe Value