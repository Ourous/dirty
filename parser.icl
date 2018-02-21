implementation module parser

import types, converter, StdEnv, Text

parseUTF8 :: !String -> Program
parseUTF8 string 
	# string
		= {#i \\ c <- utf8ToUnicode string, i <- ['\0'..] & u <- unicodeCharset | c == u}
	= parseNative string
		
parseNative :: !String -> Program
parseNative string
	# tokens
		= map fromString (split "\n" string)
	# commands
		= map (map toCommand) tokens
	# dimensions
		= {x=last(sort(map length tokens)), y=length tokens}
	# (dir, x, y)
		= findStart commands
	# location
		= if((x, y) == (-1, -1)) {x=0, y=0} {x=x, y=y}
	# wrapping
		= isWrapping commands
	# direction = dir
	= {
		dimension = dimensions,
		location = location,
		direction = direction,
		source = {{#el \\ el <- line} \\ line <- tokens},
		commands = {{el \\ el <- line} \\ line <- commands},
		wrapping = wrapping
		}
where		
	findStart [[(Control (Start dir)):_]:_]
		= (dir, 0, 0)
	findStart [[_:line]:lines]
		# (dir, x, y) = findStart [line:lines]
		| (x, y) == (-1, -1)
			= (dir, x, y)
		= (dir, x+1, y)
	findStart [[]:lines]
		# (dir, x, y) = findStart lines
		| (x, y) == (-1, -1)
			= (dir, x, y)
		= (dir, x, y+1)
	findStart _ = (East, -1, -1)
	isWrapping commands
		= 0 < sum[1\\(Control Terminate) <- flatten commands]