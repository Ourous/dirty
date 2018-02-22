implementation module runtime

import types, converter, atomics, arithmetic
import StdEnv, StdLib, System.IO, System.Time, Math.Random, Text
from Math.Geometry import pi
import qualified Data.Generics.GenParse as GenParse

instance == Direction where
	(==) East East = True
	(==) West West = True
	(==) North North = True
	(==) South South = True
	(==) NorthEast NorthEast = True
	(==) NorthWest NorthWest = True
	(==) SouthWest SouthWest = True
	(==) SouthEast SouthEast = True
	(==) _ _ = False

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

TRAVERSE_ONE :== TRAVERSE_SOME 1

MOVE_TO_NEXT :== app3 (TRAVERSE_ONE, id, id)

IS_TRUTHY stack
	:== case stack of
		[] = False
		[Zero:_] = False
		[NaN:_] = False
		_ = True
		
GET_MIDDLE :== \[El stack:_] -> stack

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
	execute :: !*(!State, !Memory, *World) -> *World
	execute (state=:{terminate=True}, memory, world)
		= abort "la la"
	// TODO: pattern match to handle memory config here
	//execute (state, memory=:{main=[]}
	execute smw=:(state=:{location, direction, history}, memory, world)
		| 0 > location.x || location.x >= dimension.x || 0 > location.y || location.y >= dimension.y
			# location = {x=location.x rem dimension.x, y=location.y rem dimension.y}
			= execute ({state&location=location,terminate=wrapping}, memory, world)
		| otherwise
			# (state, memory, world) = process commands.[location.y, location.x] smw
			# state = {state&history=source.[location.y, location.x]}
			= execute (state, memory, world)
	process :: !Command -> (*(State, Memory, *World) -> *(State, Memory, *World))
	process (Control (Terminate))
		= app3 (\state -> {state&terminate=True}, id, id)
	process (Control (NOOP))
		= MOVE_TO_NEXT
	process (Control (Start _))
		= MOVE_TO_NEXT
	process (Control (Change dir))
		= app3(TRAVERSE_ONE o \state -> {state&direction=dir}, id, id)
	process (Control (Goto dir (Just loc))) = let
		goto (state=:{direction}, memory=:{main}, world)
			| direction == dir && IS_TRUTHY (GET_MIDDLE main)
				= ({state&location=loc}, memory, world)
			| otherwise
				= (state, memory, world)
	in MOVE_TO_NEXT o goto
		
