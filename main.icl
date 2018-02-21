module main
import types, atomics, runtime, parser, converter, arithmetic
import StdEnv, StdLib, System.CommandLine, System.IO, System.File, Data.Error
Start world
	# ([_:args], world)
		= getCommandLine world
	//# args = ["-utf8", "--n", "--s", "loop_test.txt"]
	| isEmpty args
		= abort usage
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
	//# memory = {memory&main=[[[(Re (Fin (Int (2^19))))]]]}
	= execute (parser file) memory ((toFlags flags), world)
	
toFlags flags
	= {debug = False, dump = isMember "--s" flags, nums = isMember "--n" flags}
	
usage =: foldr ((+++)) ""
	["Usage: dirty [<config>] [-format] [--flags] <file> [<seed> [<stack>]]\n"
	,"\t-utf8: use the UTF-8 parser\n"
	,"\t--n: numeric output\n"
	,"\t--s: show the stacks stacks\n"
	//,"\t--g: take args greedily"
	]
