implementation module parser

import types, converter, StdEnv, StdLib, Text

parseUTF8 :: !String -> Program
parseUTF8 string 
	= parseNative {#i \\ c <- utf8ToUnicode string, i <- ['\0'..] & u <- unicodeCharset | c == u}
		
parseNative :: !String -> Program
parseNative string = let
	tokens = map fromString (split "\n" string)
	commands = linkLoops (map (map toCommand) tokens)
	in {
		dimension = {x=last(sort(map length tokens)), y=length tokens},
		source = {{#el \\ el <- line} \\ line <- tokens},
		commands = {{el \\ el <- line} \\ line <- commands},
		wrapping = 0 < sum[1 \\ (Control Terminate) <- flatten commands]
		}
		
linkLoops :: ![[Command]] -> [[Command]]
linkLoops commands = (linkLoop Left o linkLoop Right o linkGoto Middle) commands

linkLoop _ val = val // TODO

linkGoto type commands
	= (map (linkGoto` East West) o transpose o map (linkGoto` North South) o transpose) commands
where
	linkGoto` lhs rhs
		= abort "That's a TODO!"
		
		
rotate _ [] = []
rotate n list
	| n > zero
		= rotate (dec n) ((tl list) ++ [hd list])
	| n < zero
		= rotate (inc n) [last list:init list]
	| otherwise
		= list
		