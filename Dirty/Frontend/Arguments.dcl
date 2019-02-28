definition module Dirty.Frontend.Arguments

from Dirty.Backend.Stack import ::Stack
from Data.Maybe import ::Maybe
from Data.Error import ::MaybeError
from StdOverloaded import class zero

:: Flags = {
	unicode :: Bool,
	warnings :: Bool,
	logging :: Bool,
	debug :: Bool
	}

instance zero Flags
	
parseArguments :: *World -> *(MaybeError [String] (Flags, String, Stack), *World)