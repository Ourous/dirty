module main

import System.IO, System.File, System.Time
import Data.Error, Data.Maybe, Data.Func, Data.Matrix
import StdEnv, StdDebug
import Dirty.Backend, Dirty.Backend.Rational
import Dirty.Frontend.Arguments, Dirty.Frontend.Preprocessor, Dirty.Frontend.Parser
import Dirty.Runtime, Dirty.Types
import Dirty.Runtime.Instruction
import _SystemEnumStrict
from StdOverloadedList import Sum, Prod
import Text
import Regex

//debug
//Start _ = Sum [zero..toNumber 9999999]//S_sort (toStack ['abcef0'])

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
	# (instrs, starts) = parseFile source
	# (tm, world) = time world
	# seed = toInt tm
	# runtime = (initialize flags.runtime instrs starts stack seed)
	= runtime world
	// =from_position_east {{'a', 'b', 'c', 'd', 'e'}} {x=3,y=0}//
usage :== "usage: dirty [config] [options] <file> [<args>...]"


from_position_east m pos = [m.[pos.y,x] \\ x <- [pos.x..cols m-1] ++ [0..pos.x-1]]