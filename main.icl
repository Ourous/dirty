module main
import types, atomics, runtime, parser, arithmetic, System.CommandLine, System.File, Data.Error

Start world
	= (Cx (Fin {re=(Int 7), im=(Int -3)})) + (Im (Fin (Int 3))) + (Im (Inf Positive))//(Initial)
/*
	# ([_:args], world)
		= getCommandLine world
	| isEmpty args
		= abort "Usage: dirty [-utf8] [<stack> [<stack...>]] <file>"
	# (file, world)
		= case (readFile (last args)) of
			(Error _, _) = abort "Cannot open the file specified!"
			(Ok file, world) = (file, world)
	= case (init args) of
		["-utf8", args] = execute (parseUTF8 file) (evaluate args) world
		args = execute (parseSBCS file) (evaluate args) world
*/