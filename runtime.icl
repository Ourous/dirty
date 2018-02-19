implementation module runtime

import types, converter, atomics, arithmetic, System.IO, StdEnv, StdLib

moveLocation dist {x, y} East = {x=x+dist, y=y}
moveLocation dist {x, y} West = {x=x-dist, y=y}
moveLocation dist {x, y} North = {x=x, y=y-dist}
moveLocation dist {x, y} South = {x=x, y=y+dist}

evaluate :: [String] -> Memory
evaluate string
	= {left=[],right=[],bases=[],main=[]}

execute :: State Memory *World -> *(Memory, *World)
execute state=:{dimension, location, direction, program, random, history, wrapping} memory world
	//| length history > 100 = (memory, world)
	| location.x >= dimension.x || location.y >= dimension.y
		= if(wrapping) (execute {state&location={x=location.x rem dimension.x, y=location.y rem dimension.y}} memory world) (memory, world) 
	= process (toCommand ((program !! location.y) !! location.x)) world
where
	process (Control (NOOP)) world
		= execute
			{state
			&location = moveLocation 1 location direction
			,history=[(Control NOOP): history]
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
		= abort "todo"
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
			,location=moveLocation (length content + 2) location direction
			,direction=direction
			,program=program
			,random=random
			,history=history
			,wrapping=wrapping
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