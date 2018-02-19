implementation module runtime

import types, converter, atomics, arithmetic, System.IO, StdEnv, StdLib

moveLocation dist {x, y} East = {x=x+dist, y=y}
moveLocation dist {x, y} West = {x=x-dist, y=y}
moveLocation dist {x, y} North = {x=x, y=y-dist}
moveLocation dist {x, y} South = {x=x, y=y+dist}

evaluate :: [String] -> Memory
evaluate string
	= {left=[],right=[],bases=[],main=[]}

execute :: State Memory *World -> *World
execute state=:{dimension, location, direction, program, random, history, wrapping} memory world
	| location.x >= dimension.x || location.y >= dimension.y
		= if(wrapping) (execute {state&location={x=location.x rem dimension.x, y=location.y rem dimension.y}} memory world) world
	= process (toCommand ((program !! location.y) !! location.x)) world
where
	process (Control (NOOP)) world
		= execute
			{state
			&location = moveLocation 1 location direction
			} memory world
	process (Control (Start dir)) world
		= execute
			{state
			&location = moveLocation 1 location dir
			,direction = dir
			} memory world
	process (Control (Change Always dir)) world
		= execute
			{state
			&location = moveLocation 1 location dir
			,direction = dir
			} memory world
	process (Control (Mirror Always axis)) world
		= abort "Mirrors not implemented, sorry!"
	process (Control Terminate) world
		= world
	process (Control String) world
		# (line, dif)
			= case direction of
				East = (program!!location.y, location.x+1)
				West = (reverse(program!!location.y), dimension.x-location.x)
				North = ((transpose program)!!location.x, location.y+1)
				South = (reverse((transpose program)!!location.x), dimension.y-location.y)
		# line
			= line++['\n']++line
		# content
			= takeWhile ((<>)'\'') (drop dif line)
		# content
			= utf8ToUnicode (toString content)
		= execute 
			{state
			&location=moveLocation (length content + 2) location direction
			} {memory& main=[map fromInt content:memory.main]} world
	process (Operator (IO_WriteAll)) world
		# [out:main]
			= memory.main
		# out
			= unicodeToUTF8 (map toInt out)
		# world
			= execIO (putStrLn out) world
		= execute
			{state
			&location = moveLocation 1 location direction
			} {memory& main=main} world