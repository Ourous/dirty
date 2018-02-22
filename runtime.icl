implementation module runtime

import types, converter, atomics, arithmetic
import StdEnv, StdLib, System.IO, System.Time, Math.Random, Text
from Math.Geometry import pi
import qualified Data.Generics.GenParse as GenParse

unsafe :: !(*World -> *(.a, !*World)) -> .a
unsafe fn = fst (fn newWorld)

newWorld :: *World
newWorld = code inline {
	fillI 65536 0
}

SAFE_HEAD list
	:== case list of
		[] = []
		[head:_] = [head]

SAFE_TAIL list
	:== case list of
		[] = []
		[_:tail] = tail

STACK_TO_STR stack
	:== "["+join","(map toString stack)+"]"
	
//MEM_TO_STR memory=:{left, right, main}
//	:==

evaluate :: ![String] *World -> *(Memory, *World)
evaluate args world
	| isEmpty args
		# (Timestamp seed, world) = time world
		= ({left=[],right=[],main=[],random=genRandInt seed}, world)
	| otherwise
		# ((seed, world), args) = case (parseInt (hd args), world) of
			(Just seed, world) = ((seed, world), tl args)
			(Nothing, world) = ((\(Timestamp seed, world) -> (seed, world))(time world), args)
		= ({left=[],right=[],main=[],random=genRandInt seed}, world)
where
	parseInt :: (String -> (Maybe Int))
	parseInt = 'GenParse'.parseString

construct :: !Program !Flags -> (State Memory *World -> *World)
construct program=:{dimension, source, commands, wrapping} flags = execute
where
	terminate :: !Memory *World -> *World
	terminate memory=:{left, right, main} world
		= world // TODO: dump based on flags
	execute :: !State !Memory *World -> *World
	execute state=:{location, direction, history} memory world
		| 0 > location.x || location.x >= dimension.x || 0 > location.y || location.y >= dimension.y
			| wrapping
				# location = {x=location.x rem dimension.x, y=location.y rem dimension.y}
				= execute {state&location=location} memory world
			| otherwise
				= terminate memory world
		| otherwise
			= process commands.[location.y].[location.x]state memory world
	process :: !Command !State !Memory *World -> *World
	process (Control (Terminate)) _ memory world
		= terminate memory world
	
