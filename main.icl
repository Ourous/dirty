module main
import types, atomics, runtime, parser, converter, arithmetic, System.CommandLine, System.IO, System.File, Data.Error, StdEnv
Start world
	# ([_:args], world)
		= getCommandLine world
	| isEmpty args
		= abort "Usage: dirty [<config>] [-format] [--flags] <file> [<stack>]\n\tformat: utf8\n\tflags: none"
	# (parser, args)
		= case args of
			["-utf8":args] = (parseUTF8, args)
			args = (parseNative, args)
	# (flags, [file:args])
		= span (\e -> e%(0,1) == "-") args
	# parser
		= last [parseNative:[parser \\ (flag, parser) <- [("-utf8", parseUTF8)]]]
	# (file, world)
		= case (readFile file world) of
			(Error _, _) = abort "Cannot open the file specified!"
			(Ok file, world) = (file, world)
	#! (memory, world) = execute (parser file) (evaluate args) world
	= world
	//= map(map toInt)(parseUTF8 file).program