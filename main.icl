module main

import System.IO, System.Options, System.CommandLine
import Data.Error, Data.Maybe
import StdDebug
import Text.GenPrint

Start :: *World -> [String]
Start w
	# ([_:args], w) = getCommandLine w
	# result = parseOptions cliOptions args []
	| isOk result
		= fromOk result
	
	// apply with OptParser instead of Option
	
cliOptions = Flag "-h" (\opts | trace_tn (printToString opts) = Ok opts ) "Pass -h please"