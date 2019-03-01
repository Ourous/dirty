implementation module Dirty.Frontend.Arguments

import Dirty.Backend.Stack, Dirty.Backend.Value
import StdEnv
import Data.Maybe, Data.Error
import System.IO, System.CommandLine, System.Options
import Text.GenParse

defaultFlags =: {
	frontend={
		unicode=False
		},
	runtime={
		warnings=False,
		logging=False,
		debug=False
		}
	}

parseArguments :: *World -> *(MaybeError [String] (Flags, String, Stack), *World)
parseArguments w
	# ([_:args], w) = getCommandLine w
	# options = case parseOptions (WithHelp False allOptions) args (defaultFlags, "", zero) of
		Ok (_, "", _) = Error ["No input file specified"]
		options = options
	= (options, w)
	
allOptions = Options [
	inputFile,
	stackArgs,
	unicodeShorthand,
	unicodeFlag,
	warningShorthand,
	warningFlag,
	loggingShorthand,
	loggingFlag,
	debugShorthand,
	debugFlag
	:compilerOptions]
	
unicodeFlag = Flag "--utf8" update "[options] use the utf8 pre-processor"
where update (flags, file, stack) = Ok ({flags&frontend.unicode=True}, file, stack)
unicodeShorthand = Shorthand "-u" "--utf8" unicodeFlag

warningFlag = Flag "--warn" update "[options] display warnings"
where update (flags, file, stack) = Ok ({flags&runtime.warnings=True}, file, stack)
warningShorthand = Shorthand "-w" "--warn" warningFlag

loggingFlag = Flag "--log" update "[options] enable state logging"
where update (flags, file, stack) = Ok ({flags&runtime.logging=True}, file, stack)
loggingShorthand = Shorthand "-l" "--log" loggingFlag

debugFlag = Flag "--debug" update "[options] display debugging information"
where update (flags, file, stack) = Ok ({flags&runtime.debug=True}, file, stack)
debugShorthand = Shorthand "-d" "--debug" debugFlag

inputFile = Operand True update "<file>" "file to read code from"
where
	update meta (flags, file, stack)
		| file <> ""
			= Nothing
		| otherwise
			= Just (Ok (flags, meta, stack))

stackArgs = Operand True update "<args>" "initial arguments on the left stack"
where
	update meta (flags, file, stack)
		| file == ""
			= Nothing
		| otherwise
			= Just case tryParseValue meta of
				(Just val) = Ok (flags, file, stack +++ fromValue val)
				_ = Error ["Cannot parse " +++ meta]

throwConfigOptError _ = throwConfigFlagError
throwConfigFlagError _ = Error ["Config must come before options"]

compilerOptions = [
	Option "-h" throwConfigOptError "<size>" "[config] set maximum heap size",
	Option "-s" throwConfigOptError "<size>" "[config] set maximum stack size",
	Flag "-b" throwConfigFlagError "[config] return world id",
	Flag "-sc" throwConfigFlagError "[config] return world id",
	Flag "-nr" throwConfigFlagError "[config] return nothing",
	Flag "-t" throwConfigFlagError "[config] display execution time",
	Flag "-nt" throwConfigFlagError "[config] don't display execution time",
	Flag "-gc" throwConfigFlagError "[config] display heap size after garbage collection",
	Flag "-ngc" throwConfigFlagError "[config] don't display heap size after garbage collection",
	Flag "-st" throwConfigFlagError "[config] display stack size before garbage collection",
	Flag "-nst" throwConfigFlagError "[config] don't display stack size before garbage collection",
	Flag "-gcm" throwConfigFlagError "[config] use marking/compacting garbage collection",
	Flag "-gcc" throwConfigFlagError "[config] use copy/compacting garbage collection",
	Option "-gcf" throwConfigOptError "<mult>" "[config] set heap size increase after garbage collection",
	Option "-gci" throwConfigOptError "<size>" "[config] set initial heap size"
	]







