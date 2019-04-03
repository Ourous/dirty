module main

import System.IO, System.File, System.Time
import Data.Error, Data.Maybe
import StdEnv, StdDebug
import Dirty.Backend
import Dirty.Frontend
import Dirty.Runtime
import Dirty.Types
import Text


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

usage :== "usage: dirty [config] [options] <file> [<args>...]"
