implementation module runtime

import types, converter, atomics, arithmetic
import StdEnv, StdLib, System.IO, System.Time, Math.Random
import qualified GenLib as GenLib

moveLocation dist {x, y} East = {x=x+dist, y=y}
moveLocation dist {x, y} West = {x=x-dist, y=y}
moveLocation dist {x, y} North = {x=x, y=y-dist}
moveLocation dist {x, y} South = {x=x, y=y+dist}

evaluate :: [String] *World -> *(Memory, *World)
evaluate args world
	| isEmpty args
		# (Timestamp seed, world)
			= time world
		= ({left=[],right=[],bases=[0],main=[],history=[],random=[]}, world)
	# ((seed, world), args)
		= case (parseInt (hd args), world) of
			(Just seed, world) = ((seed, world), tl args)
			(Nothing, world) = ((0, world), args)
	= ({left=[],right=[],bases=[0],main=[],history=[],random=[]}, world)
where
	parseInt :: (String -> (Maybe Int))
	parseInt = 'GenLib'.parseString

execute :: State Memory Flags *World -> *World
execute state=:{dimension, location, direction, source, program, wrapping} memory=:{left, right, main, bases, random, history} flags world
	| location.x >= dimension.x || location.y >= dimension.y
		= if(wrapping) (execute {state&location={x=location.x rem dimension.x, y=location.y rem dimension.y}} memory flags world) world
	= process ((program !! location.y) !! location.x) world
where
	process (Control (NOOP)) world
		= execute
			{state
			&location = moveLocation 1 location direction
			} memory flags world
	process (Control (Start dir)) world
		= execute
			{state
			&location = moveLocation 1 location dir
			,direction = dir
			} memory flags world
	process (Control (Change Always dir)) world
		= execute
			{state
			&location = moveLocation 1 location dir
			,direction = dir
			} memory flags world
	process (Control (Mirror Always axis)) world
		= abort "Mirrors not implemented, sorry!"
	process (Control Terminate) world
		= world
	process (Control String) world
		# (line, dif)
			= case direction of
				East = (source!!location.y, location.x+1)
				West = (reverse(source!!location.y), dimension.x-location.x)
				North = ((transpose source)!!location.x, location.y+1)
				South = (reverse((transpose source)!!location.x), dimension.y-location.y)
		# line
			= line++['\n']++line
		# content
			= takeWhile ((<>)'\'') (drop dif line)
		# content
			= utf8ToUnicode (toString content)
		= execute 
			{state
			&location=moveLocation (length content + 2) location direction
			} {memory& main=[map fromInt content:memory.main]} flags world
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
			} {memory& main=main} flags world