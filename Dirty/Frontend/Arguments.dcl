definition module Dirty.Frontend.Arguments

from Dirty.Backend.Stack import ::Stack
from Data.Maybe import ::Maybe
from Data.Error import ::MaybeError

:: Flags = {
	frontend :: FrontendFlags,
	runtime :: RuntimeFlags
	}

:: FrontendFlags = {
	unicode :: Bool
	}
	
:: RuntimeFlags = {
	warnings :: Bool,
	logging :: Bool,
	debug :: Bool
	}
	
parseArguments :: *World -> *(MaybeError [String] (Flags, String, Stack), *World)