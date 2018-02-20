module main
import types, atomics, runtime, parser, converter, arithmetic
import StdEnv, StdLib, System.CommandLine, System.IO, System.File, Data.Error
Start world
	//# ([_:args], world)
	//	= getCommandLine world
	# args = ["-utf8", "loop_test.txt"]
	| isEmpty args
		= abort "Usage: dirty [<config>] [-format] [--flags] <file> [<seed> [<stack>]]\n\tformat: utf8\n\tflags: none\n"
	# (flags, [file:args])
		= span (\e -> e%(0,0) == "-") args
	# (flags, format)
		= partition (\e -> e%(0,1) == "--") flags
	# parser
		= case format of
			[] = parseNative
			["-utf8"] = parseUTF8
			_ = abort "Cannot parse the given format!"
	# (file, world)
		= case (readFile file world) of
			(Ok file, world) = (file, world)
			_ = abort "Cannot open the file specified!"
	# (memory, world)
		= evaluate args world
	= execute (parser file) memory  world(toFlags flags)
	//= (parser file)
	
toFlags _
	= {debug = False, dump = False, ints = False}