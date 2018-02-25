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

instance toString Element where
	toString (El val) = STACK_TO_STR val
	toString Delimiter = "|"

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
	:== "["+(join","(map toString stack))+"]"
	
MEM_TO_STR memory=:{left, right, main}
	:== "{left="+STACK_TO_STR left+",right="+STACK_TO_STR right+",main="+STACK_TO_STR main+"}"

TRAVERSE_SOME dist state=:{location, direction}
	:== case direction of
		East = {state&location={location&x=location.x+dist}}
		West = {state&location={location&x=location.x-dist}}
		North = {state&location={location&y=location.y-dist}}
		South = {state&location={location&y=location.y+dist}}

TRAVERSE_ONE :== TRAVERSE_SOME 1

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
		= ({left=[],right=[],main=[El []],random=genRandInt seed}, world)
	| otherwise
		# ((seed, world), args) = case (parseInt (hd args), world) of
			(Just seed, world) = ((seed, world), tl args)
			(Nothing, world) = ((\(Timestamp seed, world) -> (seed, world))(time world), args)
		= ({left=[],right=[],main=[El []],random=genRandInt seed}, world)
where
	parseInt :: (String -> (Maybe Int))
	parseInt = 'GenParse'.parseString

initialize :: !Program ![String] *World -> *(State, Memory, *World)
initialize program=:{commands} args world
	# (memory=:{random=[randpos,randdir:random]}, world)
		= evaluate args world
	| isEmpty annotated
		= ({direction=East, location={x=0,y=0}, history='\n', terminate=False}, {memory&random=random}, world)
	# (orn, loc)
		= annotated !! (randpos rem (length annotated))
	# dir = case orn of
		(Axis Vertical) = if(isEven randdir) North South
		(Axis Horizontal) = if(isEven randdir) East West
		(Dir dir) = dir
	| otherwise
		= ({direction=dir, location=loc, history='\n', terminate=False}, {memory&random=random}, world)
where
	annotated = [(orn, {x=x, y=y}) \\ y <- [0..] & line <-: commands, x <- [0..] & (Control (Start orn)) <-: line]


construct :: !Program !Flags -> (*(!State, !Memory, !*World) -> *World)
construct program=:{dimension, source, commands, wrapping} flags = execute
where

	execute :: !*(!State, !Memory, !*World) -> *World
	
	execute (state=:{terminate=True}, memory, world)
		| flags.dump
			= execIO (putStrLn (MEM_TO_STR memory)) world
		| otherwise
			= world
	
	execute (state, memory=:{main=[]}, world)
		= execute (state, {memory&main=[El[]]}, world)
	execute (state, memory=:{main=[Delimiter:other]}, world)
		= execute (state, {memory&main=other}, world)
	
	execute smw=:(state=:{location, direction, history}, memory, world)
		| 0 > location.x || location.x >= dimension.x || 0 > location.y || location.y >= dimension.y = let
			wrappedLocation = {x=location.x rem dimension.x, y=location.y rem dimension.y}
			in execute ({state&location=wrappedLocation,terminate=not wrapping}, memory, world)
		| otherwise
			# (state, memory, world) = process commands.[location.y, location.x] smw
			= execute (TRAVERSE_ONE {state&history=source.[location.y, location.x]} , memory, world)
			
	writeLine :: ![Number] -> (IO ())
	
	writeLine stack = putStrLn (if(flags.nums) (STACK_TO_STR stack)  (unicodeToUTF8 (map toInt stack)))
	
	writeChar :: !Number -> (IO ())
	
	writeChar char = putStr (if(flags.nums) (toString char) (unicodeToUTF8 [toInt char]))

	process :: !Command -> (*(!State, !Memory, !*World) -> *(State, Memory, *World))
	
	process (Control (Terminate)) = app3 (\state -> {state&terminate=True}, id, id)
		
	process (Control (NOOP)) = id
		
	process (Control (Start _)) = id
		
	process (Control (Change dir)) = app3(\state -> {state&direction=dir}, id, id)
	
	process (Control (Either axes)) = either
	where
	
		either (state, memory=:{random=[rng:random]}, world) = let
			newDirection = case axes of
				Horizontal = if(isEven rng) East West
				Vertical = if(isEven rng) North South
		in ({state&direction=newDirection}, {memory&random=random}, world)
	
	process (Control (Goto dir (Just loc))) = goto
	where
	
		goto (state=:{direction}, memory=:{main}, world)
			| direction == dir && IS_TRUTHY (GET_MIDDLE main)
				= ({state&location=loc}, memory, world)
			| otherwise
				= (state, memory, world)
				
	process (Control (String)) = makeString
	where
		
		makeString (state=:{direction, location}, memory=:{main}, world)
			= (TRAVERSE_SOME (length content + 1) state, {memory&main=[El(map fromInt(utf8ToUnicode(toString content))),Delimiter:main]}, world)
		where
			
			delta = case direction of
				East = location.x+1
				West = dimension.x-location.x
				North = dimension.y-location.y
				South = location.y+1
				
			wrappedLine = let
				line = case direction of
					East = [c \\ c <-: source.[location.y]]
					West = reverse [c \\ c <-: source.[location.y]]
					North = reverse [src.[location.x] \\ src <-: source]
					South = [src.[location.x] \\ src <-: source]
			in line ++ ['\n'] ++ line
			
			content :: [Char]
			content = (takeWhile ((<>)'\'') o drop delta) wrappedLine
			
	process (Literal (Pi)) = app3 (id, \memory=:{main=[El mid:other]} -> {memory&main=[El[fromReal pi:mid]:other]}, id)

	process (Literal (Quote)) = app3 (id, \memory=:{main=[El mid:other]} -> {memory&main=[El[fromInt(toInt'\''):mid]:other]}, id)

	process (Literal (Digit val)) = literal
	where
	
		literal :: !*(!State, !Memory, *World) -> *(State, Memory, *World)
		
		literal (state=:{history}, memory=:{main}, world)
			| isDigit history = let
				[El [top:mid]:base] = main
				res = top * (fromInt 10) + val
				in (state, {memory&main=[El [res:mid]:base]}, world)
			| otherwise = let
				[El mid:base] = main
				in (state, {memory&main=[El [val:mid]:base]}, world)
				
	process (Literal (Alphabet lettercase)) = app3 (id, \memory -> {memory&main=[El literal,Delimiter:memory.main]}, id)
	where
	
		literal :: [Number]
		literal = case lettercase of
			Lowercase = [fromInt (toInt c) \\ c <-: "abcdefghijklmnopqrstuvwxyz"]
			Uppercase = [fromInt (toInt c) \\ c <-: "ABCDEFGHIJKLMNOPQRSTUVWXYZ"]
				
	process (Operator (IO_WriteAll)) = writeAll
	where
		
		writeAll (stack, memory=:{main=[El[]:other]}, world)
			= (stack, {memory&main=other}, world)
		
		writeAll (stack, memory=:{main=[El mid:other]}, world)
			# world = execIO (writeLine mid) world
			= (stack, {memory&main=other}, world)	
			
	process (Operator (Binary_NN_N op)) = app3 (id, binary, id)
	where
		
		binary :: !Memory -> Memory
		
		binary memory = case memory of
			{left=[lhs:_], main=[El mid:other], right=[rhs:_]}
				= {memory&main=[El[op lhs rhs:mid]:other]}
			{left=[lhs:_], main=[El [rhs:mid]:other], right=[]}
				= {memory&main=[El[op lhs rhs:mid]:other]}
			{left=[], main=[El [lhs:mid]:other], right=[rhs:_]}
				= {memory&main=[El[op lhs rhs:mid]:other]}
			{left=[lhs,rhs:_], main=[El []:other], right=[]}
				= {memory&main=[El[op lhs rhs]:other]}
			{left=[], main=[El []:other], right=[rhs,lhs:_]}
				= {memory&main=[El[op lhs rhs]:other]}
			_ = memory
			
	process (Operator (Binary_NN_S op)) = app3 (id, binary, id)
	where
		
		binary :: !Memory -> Memory
		
		binary memory = case memory of
			{left=[lhs:_], right=[rhs:_]}
				= {memory&main=[El(op lhs rhs),Delimiter:memory.main]}
			{left=[lhs:_], main=[El [rhs:mid]:other], right=[]}
				= {memory&main=[El(op lhs rhs),Delimiter,El mid:other]}
			{left=[], main=[El [lhs:mid]:other], right=[rhs:_]}
				= {memory&main=[El(op lhs rhs),Delimiter,El mid:other]}
			{left=[lhs,rhs:_], main=[El []:other], right=[]}
				= {memory&main=[El(op lhs rhs),Delimiter,El[]:other]}
			{left=[], main=[El []:other], right=[rhs,lhs:_]}
				= {memory&main=[El(op lhs rhs),Delimiter,El[]:other]}
			_ = memory
			
	process (Operator (Unary_N_N op)) = app3 (id, unary, id)
	where
		
		unary :: !Memory -> Memory
		
		unary memory = case memory of
			{main=[El [arg:mid]:other]} = {memory&main=[El [op arg:mid]:other]}
			_ = memory
				
	// TODO: move stack operations into the tokens as well			
	
	process (Stack (MoveTop dir)) = app3 (id, moveTop dir, id)
	where
		
		moveTop :: !Direction !Memory -> Memory
		
		moveTop East memory = case memory of
			{left=[head:tail]} = {memory&left=tail,right=[head:memory.right]}
			_ = memory
		
		moveTop West memory = case memory of
			{right=[head:tail]} = {memory&left=[head:memory.left],right=tail}
			_ = memory
			
		moveTop NorthEast memory = case memory of
			{main=[El [head:tail]:other]} = {memory&main=[El tail:other],right=[head:memory.right]}
			_ = memory
			
		moveTop NorthWest memory = case memory of
			{main=[El [head:tail]:other]} = {memory&left=[head:memory.left],main=[El tail:other]}
			_ = memory
