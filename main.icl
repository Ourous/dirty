module main
import types, stacks, runtime, parser, tests
import StdEnv, StdLib, System.CommandLine, System.IO, System.File, Data.Error
Start world
	# ([_:args], world)
		= getCommandLine world
	//# args = ["-utf8", "--dump-stacks", "tests/console_test.txt"]
	| isEmpty args
		= abort usage
	# (flags, [file:args])
		= span (\e -> e%(0,0) == "-") args
	# (longFlags, shortFlags)
		= partition (\e -> e%(0,1) == "--") flags
	| isMember "-test" shortFlags
		= runTests world
	# parser
		= case shortFlags of
			[] = parseNative
			["-utf8"] = parseUTF8
			_ = abort "Cannot parse the given format!"
	# (file, world)
		= case (readFile file world) of
			(Ok file, world) = (file, world)
			_ = abort "Cannot open the file specified!"
	= let program = parser file
	in (construct program (toFlags longFlags)) (initialize program args world)
	
toFlags flags
	= {debug = False, dump = isMember "--dump-stacks" flags, nums = isMember "--numeric-output" flags, strict = isMember "--strict-args" flags, native = isMember "--native-env" flags}
	
usage :== foldr ((+++)) ""
	["Usage: dirty [<config>] [-format] [--flags] <file> [<stack>]\n"
	,"\t-utf8: use the UTF-8 parser\n"
	,"\t-test: run all unit tests\n"
	,"\t--numeric-output: numeric output for write commands\n"
	,"\t--dump-stacks: show the stacks on termination\n"
	,"\t--strict-args: never take two arguments from the middle stack\n"
	,"\t--native-env: take input and give output without translating unicode\n"
	//,"\t--g: take args greedily"
	]