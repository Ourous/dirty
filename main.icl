module main

import System.IO, System.File
import Data.Error, Data.Maybe, Data.Func
import StdEnv, StdDebug
import Dirty.Backend
import Dirty.Frontend.Arguments, Dirty.Frontend.Preprocessor, Dirty.Frontend.Parser
import Dirty.Runtime.Instruction
import _SystemEnumStrict
from StdOverloadedList import Sum
import Text
import Regex

//debug
//Start _ = Sum [(toNumber 0)..(toNumber 9999999)]//S_sort (toStack ['abcef0'])

Start world
	# (opts, world) = parseArguments world
	| isError opts
		= abort (join "\n" [usage:fromError opts])
	# (flags, file, stack) = fromOk opts
	# (file, world)
		= case (readFile file world) of
			(Ok file, world) = (if(flags.frontend.unicode) preprocessUTF8 id file, world)
			(Error err, _) = abort (usage + "\n" <+ err <+ file)
	# source = preprocessFile file
	= parseFile source
	
usage :== "usage: dirty [config] [options] <file> [<args>...]"