implementation module runtime

import types, atomics, arithmetic, builtins, utilities, unicode, environment
import StdEnv, StdLib, System.IO, System.Time, Math.Random, Text
from Math.Geometry import pi
import qualified Data.Generics.GenParse as GenParse

instance toString Element where
	toString (El val) = STACK_TO_STR val
	toString (Delim cur) = if(cur) "!" "|"

unsafe :: !(*World -> *(.a, !*World)) -> .a
unsafe fn = fst (fn newWorld)

newWorld :: *World
newWorld = code inline {
	fillI 65536 0
}

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
		
GET_MIDDLE :== \[El stack:_] -> stack

evaluate :: ![String] *World -> *(Memory, *World)
evaluate args world
	| isEmpty args
		# (Timestamp seed, world) = time world
		= ({left=[],right=[],main=[El [],Delim True],random=genRandInt seed,note=NaN}, world)
	| otherwise
		# ((seed, world), args) = case (parseInt (hd args), world) of
			(Just seed, world) = ((seed, world), tl args)
			(Nothing, world) = ((\(Timestamp seed, world) -> (seed, world))(time world), args)
		= ({left=[],right=[],main=parseArgs args++[Delim True],random=genRandInt seed,note=NaN}, world)
where

	parseArgs :: [String] -> [Element]
	parseArgs [] = []
	parseArgs [head:tail] = [parseArg head:parseArgs tail]
	where
		parseArg arg
			# try = parseString arg
			| isJust try
				# (Just try) = try
				= (El (map fromInt (utf8ToUnicode try)))
			# try = parseInts arg
			| isJust try
				# (Just try) = try
				= (El (map fromInt try))
			# try = parseReals arg
			| isJust try
				# (Just try) = try
				= (El (map fromReal try))
			| otherwise
				= abort "Invalid memory arguments!"

	parseInt :: (String -> (Maybe Int))
	parseInt = 'GenParse'.parseString
	
	parseString :: (String -> (Maybe String))
	parseString = 'GenParse'.parseString
	
	parseInts :: (String -> (Maybe [Int]))
	parseInts = 'GenParse'.parseString
	
	parseReals :: (String -> (Maybe [Real]))
	parseReals = 'GenParse'.parseString

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
		= execute (state, {memory&main=[El[],Delim True]}, world)
	execute (state, memory=:{main=[El mid]}, world)
		= execute (state, {memory&main=[El mid,Delim True]}, world)
	execute (state, memory=:{main=[Delim _]}, world)
		= execute (state, {memory&main=[El[],Delim True]}, world)
	execute (state, memory=:{main=[Delim cur,head:tail]}, world)
		= execute (state, {memory&main=ENSURE_ACTIVE[head:tail]}, world)
	
	execute smw=:(state=:{location, direction, history}, memory, world)
		| 0 > location.x || location.x >= dimension.x || 0 > location.y || location.y >= dimension.y = let
			wrappedLocation = {x=(location.x + dimension.x) rem dimension.x, y=(location.y + dimension.y) rem dimension.y}
			in execute ({state&location=wrappedLocation,terminate=not wrapping}, memory, world)
		| otherwise
			# (state, memory, world) = process commands.[location.y, location.x] smw
			= execute (TRAVERSE_ONE {state&history=source.[location.y, location.x]} , memory, world)
			
	writeLine :: ![Number] -> (IO ())
	
	writeLine stack = putStrLn (if(flags.nums) (STACK_TO_STR stack)  (unicodeToUTF8 (map toInt stack)))
	
	writeChar :: !Number -> (IO ())
	
	writeChar char = putStr (if(flags.nums) (toString char) (unicodeToUTF8 [toInt char]))
	
	readLine :: (*World -> ([Number], *World))
	readLine => app2 (map fromInt o utf8ToUnicode, id) o evalIO getLine
	
	readChar :: (*World -> (Number, *World))
	readChar => app2 (hd o map fromInt o utf8ToUnicode, id) o getUTF8 o evalIO getChar
	where 
		
		getUTF8 :: !(!Char, !*World) -> (String, *World)
		getUTF8 (chr, world)
			| chr < '\302'
				= ({#chr}, world)
			# (str, world)
				= getMore ({#chr}, world)
			| chr < '\340'
				= (str, world)
			# (str, world)
				= getMore (str, world)
			| chr < '\360'
				= (str, world)
			| otherwise
				= getMore (str, world)
				
		getMore :: !(!String, !*World) -> (String, *World)
		getMore (str, world)
			# (chr, world) = evalIO getChar world
			= (str <+ chr, world)

	process :: !Command -> (*(!State, !Memory, !*World) -> *(State, Memory, *World))
	
	process (Control (Terminate)) = app3 (\state -> {state&terminate=True}, id, id)
		
	process (Control (NOOP)) = id
		
	process (Control (Start _)) = id
		
	process (Control (Change dir)) = app3(\state -> {state&direction=dir}, id, id)
	
	process (Control (Bounce dir)) = app3(bounce, id, id)
	where
	
		bounce state=:{direction, location={x, y}}
			= case (dir, direction) of
				(NorthEast, West) = {state&direction=North,location={x=x+1,y=y}}
				(NorthEast, South) = {state&direction=East,location={x=x,y=y-1}}
				(NorthEast, North) = {state&location={x=x+1,y=y}}
				(NorthEast, East) = {state&location={x=x,y=y-1}}
				(SouthEast, West) = {state&direction=South,location={x=x+1,y=y}}
				(SouthEast, North) = {state&direction=East,location={x=x,y=y+1}}
				(SouthEast, South) = {state&location={x=x+1,y=y}}
				(SouthEast, East) = {state&location={x=x,y=y+1}}
				(SouthWest, East) = {state&direction=South,location={x=x-1,y=y}}
				(SouthWest, North) = {state&direction=West,location={x=x,y=y+1}}
				(SouthWest, South) = {state&location={x=x-1,y=y}}
				(SouthWest, West) = {state&location={x=x,y=y+1}}
				(NorthWest, East) = {state&direction=North,location={x=x-1,y=y}}
				(NorthWest, South) = {state&direction=West,location={x=x,y=y-1}}
				(NorthWest, North) = {state&location={x=x-1,y=y}}
				(NorthWest, West) = {state&location={x=x,y=y-1}}
	
	process (Control (Either axes)) = either
	where
	
		either (state, memory=:{random=[rng:random]}, world) = let
			newDirection = case axes of
				Horizontal = if(isEven rng) East West
				Vertical = if(isEven rng) North South
		in ({state&direction=newDirection}, {memory&random=random}, world)
		
	process (Control (Mirror cond axes)) = mirror
	where
		
		mirror (state=:{direction}, memory=:{main=[El mid:other]}, world)
			| axesCollide direction && (cond || TO_BOOL mid) = let
					reflector = case axes of
						Inverse = reflectInverse
						Identity = reflectIdentity
						_ = reflectComplete
				in ({state&direction=reflector direction}, memory, world)
			| otherwise
				= (state, memory, world)
			
		reflectIdentity East = North
		reflectIdentity North = East
		reflectIdentity West = South
		reflectIdentity South = West
		
		reflectInverse East = South
		reflectInverse South = East
		reflectInverse West = North
		reflectInverse North = West
			
		reflectComplete East = West
		reflectComplete West = East
		reflectComplete South = North
		reflectComplete North = South
			
		axesCollide dir = case (axes, dir) of
			(Horizontal, East) = False
			(Horizontal, West) = False
			(Vertical, North) = False
			(Vertical, South) = False
			_ = True
		
	process (Control (Skip cond)) = skip
	where
		
		skip (state, memory=:{main=[El mid:other]}, world)
			| cond || TO_BOOL mid
				= (TRAVERSE_ONE state, memory, world)
			| otherwise
				= (state, memory, world)
				
	process (Control (Turn rot)) = app3 (turn, id, id)
	where
		
		turn state=:{direction} = let
			dir = case (direction, rot) of
				(East, Clockwise) = South
				(East, Anticlockwise) = North
				(West, Clockwise) = North
				(West, Anticlockwise) = South
				(North, Clockwise) = East
				(North, Anticlockwise) = West
				(South, Clockwise) = West
				(South, Anticlockwise) = East
		in {state&direction=dir}
	
	process (Control (Loop Left dir (Just loc))) = loop
	where
		
		loop (state=:{direction}, memory=:{left}, world)
			| direction == dir && TO_BOOL left
				= ({state&location=loc}, {memory&left=SAFE_TAIL left}, world)
			| otherwise
				= (state, memory, world)
		
	process (Control (Loop Right dir (Just loc))) = loop
	where
		
		loop (state=:{direction}, memory=:{right}, world)
			| direction == dir && TO_BOOL right
				= ({state&location=loc}, {memory&right=SAFE_TAIL right}, world)
			| otherwise
				= (state, memory, world)
	
	process (Control (Goto dir (Just loc))) = goto
	where
	
		goto (state=:{direction}, memory=:{main=[El mid:other]}, world)
			| direction == dir && TO_BOOL mid
				= ({state&location=loc}, memory, world)
			| otherwise
				= (state, memory, world)
				
	process (Control (String)) = makeString
	where
		
		makeString (state=:{direction, location}, memory=:{main}, world)
			= (TRAVERSE_SOME (length content + 1) state, {memory&main=[El(map fromInt(utf8ToUnicode(toString content))):SET_NEW_DELIM main]}, world)
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
				
	process (Literal (Alphabet lettercase)) = app3 (id, \memory -> {memory&main=[El literal:SET_NEW_DELIM memory.main]}, id)
	where
	
		literal :: [Number]
		literal = case lettercase of
			Lowercase = [fromInt (toInt c) \\ c <-: "abcdefghijklmnopqrstuvwxyz"]
			Uppercase = [fromInt (toInt c) \\ c <-: "ABCDEFGHIJKLMNOPQRSTUVWXYZ"]
			
	process (Literal (EmptySet)) = app3 (id, \memory -> {memory&main=[El []:memory.main]}, id)
				
	process (Variable (Random)) = app3 (id, rand, id)
	where
	
		rand memory=:{main=[El mid:other], random=[rng:random]}
			= {memory&main=[El[fromInt rng:mid]:other],random=random}
			
	process (Environment env) = environment
	where
	
		environment (state, memory, world)
			# (memory, world) = env (memory, world)
			= (state, memory, world)
				
	process (Operator (IO_WriteAll)) = writeAll
	where
		
		writeAll (state, memory=:{main=[El[]:other]}, world)
			= (state, {memory&main=other}, world)
		
		writeAll (state, memory=:{main=[El mid:other]}, world)
			# world = execIO (writeLine mid) world
			= (state, {memory&main=other}, world)
			
	process (Operator (IO_ReadAll)) = readAll
	where
		
		readAll (state, memory=:{main}, world)
			# (str, world) = readLine world
			= (state, {memory&main=[El str:SET_NEW_DELIM main]}, world)
			
	process (Operator (IO_WriteOnce)) = writeOnce
	where
	
		writeOnce (state, memory=:{main=[El[]:_]}, world)
			= (state, memory, world)
			
		writeOnce (state, memory=:{main=[El[top:mid]:other]}, world)
			# world = execIO (writeChar top) world
			= (state, {memory&main=[El mid:other]}, world)
			
	process (Operator (IO_ReadOnce)) = readOnce
	where
	
		readOnce (state, memory=:{main=[El mid:other]}, world)
			# (chr, world) = readChar world
			= (state, {memory&main=[El[chr:mid]:other]}, world)
			
	process (Operator (IO_Interrobang)) = interrobang
	where
	
		interrobang (state, memory=:{main=[El []:other]}, world)
			# (chr, world) = readChar world
			= (state, {memory&main=[El[chr]:other]}, world)
		interrobang (state, memory=:{main=[El [top:mid]:other]}, world)
			# world = execIO (writeChar top) world
			# (chr, world) = readChar world
			= (state, {memory&main=[El[chr:mid]:other]}, world)

	process (Operator (Binary_NN_N inv op)) = app3 (id, binary (inv && not flags.strict), id)
	where
		
		binary :: !Bool !Memory -> Memory
		binary _ memory=:{left=[lhs:_], main=[El mid:other], right=[rhs:_]}
			= {memory&main=[El[op lhs rhs:mid]:other]}
		binary _ memory=:{left=[lhs:_], main=[El [top:mid]:other], right=[]}
			= {memory&main=[El[op lhs top:mid]:other]}
		binary _ memory=:{left=[], main=[El [top:mid]:other], right=[rhs:_]}
			= {memory&main=[El[op top rhs:mid]:other]}
		binary _ memory=:{left=[], main=[El []:other], right=[rhs,lhs:_]}
			= {memory&main=[El[op lhs rhs]:other]}
		binary _ memory=:{left=[lhs,rhs:_], main=[El []:other], right=[]}
			= {memory&main=[El[op lhs rhs]:other]}
		binary True memory=:{left=[], main=[El [arg1,arg2:mid]:other], right=[]}
			= {memory&main=[El[op arg1 arg2:mid]:other]}
		binary _ memory = memory
		
			
	process (Operator (Binary_NN_S inv op)) = app3 (id, binary (inv && not flags.strict), id)
	where // productive
		
		binary :: !Bool !Memory -> Memory
		binary _ memory=:{left=[lhs:_], right=[rhs:_]}
			= {memory&main=[El(op lhs rhs):SET_NEW_DELIM memory.main]}
		binary _ memory=:{left=[lhs:_], main=[El [top]:other], right=[]}
			= {memory&main=[El(op lhs top):SET_NEW_DELIM other]}
		binary _ memory=:{left=[], main=[El [top]:other], right=[rhs:_]}
			= {memory&main=[El(op top rhs):SET_NEW_DELIM other]}
		binary _ memory=:{left=[lhs:_], main=[El [top:mid]:other], right=[]}
			= {memory&main=[El(op lhs top):SET_NEW_DELIM[El mid:other]]}
		binary _ memory=:{left=[], main=[El [top:mid]:other], right=[rhs:_]}
			= {memory&main=[El(op top rhs):SET_NEW_DELIM[El mid:other]]}
		binary _ memory=:{left=[lhs,rhs:_], main=[El []:_], right=[]}
			= {memory&main=[El(op lhs rhs):SET_NEW_DELIM memory.main]}
		binary _ memory=:{left=[], main=[El []:_], right=[rhs,lhs:_]}
			= {memory&main=[El(op lhs rhs):SET_NEW_DELIM memory.main]}
		binary True memory=:{left=[], main=[El [arg1,arg2]:other], right=[]}
			= {memory&main=[El(op arg1 arg2):SET_NEW_DELIM other]}
		binary True memory=:{left=[], main=[El [arg1,arg2:mid]:other], right=[]}
			= {memory&main=[El(op arg1 arg2):SET_NEW_DELIM[El mid:other]]}
		binary _ memory = memory
			
	process (Operator (Binary_SN_N op)) = app3 (id, binary, id)
	where
	
		binary :: !Memory -> Memory
		binary memory=:{left, main=[El mid:other], right=[rhs:_]}
			= {memory&main=[El [op left rhs:mid]:other]}
		binary memory=:{left, main=[El [top:mid]:other], right=[]}
			= {memory&main=[El [op left top:mid]:other]}
		binary memory = memory
		
	process (Operator (Binary_SS_N inv op)) = app3 (id, binary (inv && not flags.strict), id)
	where
	
		binary :: !Bool !Memory -> Memory
		binary _ memory=:{left, main=[El mid:other], right}
			= {memory&main=[El [op left right:mid]:other]}
		binary _ memory = memory
		
	process (Operator (Binary_SS_S inv op)) = app3 (id, binary (inv && not flags.strict), id)
	where
	
		binary :: !Bool !Memory -> Memory
		binary _ memory=:{left, main, right}
			= {memory&main=[El (op left right):SET_NEW_DELIM main]}
			
	process (Operator (Unary_N_N op)) = app3 (id, unary, id)
	where
		
		unary :: !Memory -> Memory
		unary memory=:{main=[El [arg:mid]:other]}
			= {memory&main=[El [op arg:mid]:other]}
		unary memory = memory
		
	process (Operator (Unary_N_S op)) = app3 (id, unary, id)
	where
		
		unary :: !Memory -> Memory
		unary memory=:{main=[El [arg]:other]}
			= {memory&main=[El (op arg):SET_NEW_DELIM other]}
		unary memory=:{main=[El [arg:mid]:other]}
			= {memory&main=[El (op arg):SET_NEW_DELIM [El mid:other]]}
		unary memory = memory
		
	process (Operator (Unary_S_N op)) = app3 (id, unary, id)
	where
	
		unary :: !Memory -> Memory
		unary memory=:{main=[El mid:other]}
			= {memory&main=[El [op mid]:other]}
		//unary memory = memory
		
	process (Operator (Unary_S_S op)) = app3 (id, unary, id)
	where
		
		unary :: !Memory -> Memory
		unary memory=:{main=[El mid:other]}
			= {memory&main=[El (op mid):other]}
		//unary memory = memory
	
	process (Operator (Unary_S_T op)) = app3 (id, unary, id)
	where
		
		unary :: !Memory -> Memory
		unary memory=:{main=[El mid:other]}
			= {memory&main=(map (\e -> (El e)) (op mid)) ++ SET_NEW_DELIM other}
		
				
	process (Operator (Unary_M_M op)) = app3 (id, op, id)
			
