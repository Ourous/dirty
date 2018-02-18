module main
import types, atomics, runtime, parser, converter, arithmetic, System.CommandLine, System.File, Data.Error, StdEnv
Start world
	//# ([_:args], world)
	//	= getCommandLine world
	# args = ["-utf8", "helloworld.txt"]
	| isEmpty args
		= abort "Usage: dirty [-utf8] <file> [<stack> [<stack...>]]"
	# (parser, [file:args])
		= case args of
			["-utf8":args] = (parseUTF8, args)
			args = (parseNative, args)
	# (file, world)
		= case (readFile file world) of
			(Error _, _) = abort "Cannot open the file specified!"
			(Ok file, world) = (file, world)
	//= execute (parser file) (evaluate args) world
	= parser file