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

TRAVERSE_SOME dist state=:{location, direction}
	:== case direction of
		East = {state&location={location&x=location.x+dist}}
		West = {state&location={location&x=location.x-dist}}
		North = {state&location={location&y=location.y-dist}}
		South = {state&location={location&y=location.y+dist}}

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

construct :: !Program !Flags -> (*(State, Memory, *World) -> *World)
construct program=:{dimension, source, commands, wrapping} flags = execute
where
	terminate :: !Memory *World -> *World
	terminate memory=:{left, right, main} world
		= world // TODO: dump based on flags
	execute :: !*(!State, !Memory, *World) -> *World
	execute (state=:{terminated=True}, memory, world)
		= abort "la la"
	execute smw=:(state=:{location, direction, history}, memory, world)
		| 0 > location.x || location.x >= dimension.x || 0 > location.y || location.y >= dimension.y
			# location = {x=location.x rem dimension.x, y=location.y rem dimension.y}
			= execute ({state&location=location,terminated=wrapping}, memory, world)
		| otherwise
			= execute (process commands.[location.y, location.x] smw)
	process :: !Command -> (*(State, Memory, *World) -> *(State, Memory, *World))
	process (Control (NOOP)) = id
	
	
