module main
import types, atomics, runtime, parser, converter, arithmetic, System.CommandLine, System.IO, System.File, Data.Error, StdEnv
Start world
	# ([_:args], world)
		= getCommandLine world
	# args = ["-utf8", "recursion_test.txt"]
	| isEmpty args
		= abort "Usage: dirty [<config>] [-format] [--flags] <file> [<stack>]\n\tformat: utf8\n\tflags: none"
	# (flags, [file:args])
		= span (\e -> e%(0,0) == "-") args
	# parser
		= last [parseNative:[parser \\ (flag, parser) <- [("-utf8", parseUTF8)]]]
	# (file, world)
		= case (readFile file world) of
			(Error _, _) = abort "Cannot open the file specified!"
			(Ok file, world) = (file, world)
	= execute (parser file) (evaluate args) world
	//= map(map toInt)(parseUTF8 file).program