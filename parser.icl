implementation module parser

import types, converter, StdEnv, Text

parseUTF8 :: String -> State
parseUTF8 string 
	# string
		= {#i \\ c <- utf8ToUnicode string, i <- ['\0'..] & u <- unicodeCharset | c == u}
	= parseNative string
		
parseNative :: String -> State
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
		= {x=x, y=y}
	# direction = dir
	= {
		dimension = dimensions,
		location = location,
		direction = direction,
		program = tokens,
		random = [],
		history = []
		}
where		
	findStart [[(Control (Start dir)):_]:_]
		= (dir, 0, 0)
	findStart [[_:line]:lines]
		# (dir, x, y) = findStart [line:lines]
		= (dir, x+1, y)
	findStart [[]:lines]
		# (dir, x, y) = findStart lines
		= (dir, x, y+1)
	findStart _ = (East, 0, 0)