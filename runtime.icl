implementation module runtime

import types, converter, atomics, arithmetic, System.IO

execute :: State Memory *World -> *World
execute {dimension, location, program, random, history, memory} world

where
	currentCommand
		= (program !! location.y) !! location.x