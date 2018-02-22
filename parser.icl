implementation module parser

import types, converter, StdEnv, StdLib, Text

parseUTF8 :: !String -> Program
parseUTF8 string 
	= parseNative {#i \\ c <- utf8ToUnicode string, i <- ['\0'..] & u <- unicodeCharset | c == u}
		
parseNative :: !String -> Program
parseNative string = let
	tokens = map fromString (split "\n" string)
	commands = map (map toCommand) tokens
	in {
		dimension = {x=last(sort(map length tokens)), y=length tokens},
		source = {{#el \\ el <- line} \\ line <- tokens},
		commands = {{el \\ el <- line} \\ line <- commands},
		wrapping = 0 < sum[1 \\ (Control Terminate) <- flatten commands]
		}