module main
import types, atomics, runtime, parser, converter, arithmetic, System.CommandLine, System.IO, System.File, Data.Error, StdEnv
Start world
	# ([_:args], world)
		= getCommandLine world
	| isEmpty args
		= abort "Usage: dirty [<config>] [-format] [--flags] <file> [<stack>]\n\tformat: utf8\n\tflags: none"
	# (parser, [file:args])
		= case args of
			["-utf8":args] = (parseUTF8, args)
			args = (parseNative, args)
	# (file, world)
		= case (readFile file world) of
			(Error _, _) = abort "Cannot open the file specified!"
			(Ok file, world) = (file, world)
	#! (memory, world) = execute (parser file) (evaluate args) world
	= world