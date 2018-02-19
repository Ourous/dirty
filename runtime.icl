implementation module runtime

import types, converter, atomics, arithmetic, System.IO, StdEnv, StdLib

moveLocation {x, y} East = {x=x+1, y=y}
moveLocation {x, y} West = {x=x-1, y=y}
moveLocation {x, y} North = {x=x, y=y-1}
moveLocation {x, y} South = {x=x, y=y+1}

evaluate :: [String] -> Memory
evaluate string
	= {left=[],right=[],bases=[],main=[]}

execute :: State Memory *World -> *(Memory, *World)
execute {dimension, location, direction, program, random, history} memory world
	= process (toCommand ((program !! location.y) !! location.x)) world
where
	process (Control (Start dir)) world
		= execute 
			{dimension=dimension
			,location=moveLocation location dir
			,direction=dir
			,program=program
			,random=random
			,history=history
			} memory world
	process (Control Terminate) world
		= (memory, world)
	process (Control String) world
		# line
			= (program !! location.y)++['\n']++(program !! location.y)
		# content
			= takeWhile ((<>)'\'') (drop (location.x+1) line)
		# content
			= utf8ToUnicode (toString content)
		= execute 
			{dimension=dimension
			,location={location& x=(location.x + length content+2) rem dimension.x}
			,direction=direction
			,program=program
			,random=random
			,history=history
			} {memory& main=[map fromInt content:memory.main]} world
	process (Operator (IO_WriteAll)) world
		# [out:main]
			= memory.main
		# out
			= unicodeToUTF8 (map toInt out)
		# world
			= execIO (putStrLn out) world
		= execute
			{dimension=dimension
			,location=moveLocation location direction
			,direction=direction
			,program=program
			,random=random
			,history=history
			} {memory& main=main} world