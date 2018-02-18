implementation module parser

import types, converter, StdEnv, Text

parseUTF8 :: String -> State
parseUTF8 string 
	# string
		= {#i \\ i <- ['\0'..] & u <- unicodeCharset, c <- utf8ToUnicode string | c == u}
	= parseNative string
		
parseNative :: String -> State
parseNative string
	# lines
		= map tokenizeLine (split "\n" string)
	# dimensions
		= {x=last(sort(map length lines)), y=length lines}
	# (dir, x, y)
		= findStart lines
	# location
		= {x=x, y=y}
	# direction = dir
	= {
		dimension = dimensions,
		location = location,
		direction = direction,
		program = lines,
		random = [],
		history = [],
		memory = {
			left = [],
			right = [],
			bases = [0],
			main = [[]]
			}
		}
where
	tokenizeLine line
		= [commandMapping !! (toInt c) \\ c <-: line]
			
	findStart [[(Control (Start dir)):_]:_]
		= (dir, 0, 0)
	findStart [[_:line]:lines]
		# (dir, x, y) = findStart [line:lines]
		= (dir, x+1, y)
	findStart [[]:lines]
		# (dir, x, y) = findStart lines
		= (dir, x, y+1)