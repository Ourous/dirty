implementation module runtime

import types, atomics, arithmetic, builtins, utilities, unicode, stacks, native
import StdEnv, StdLib, System.IO, System.Time, Math.Random, Text, Data.Func
from Math.Geometry import pi
import qualified Data.Generics.GenParse as GenParse

instance toString Element where
	toString (El val) = STACK_TO_STR val
	toString (Delim cur) = "("<+cur<+")"

unsafe :: !(*World -> *(.a, !*World)) -> .a
unsafe fn = fst (fn newWorld)

newWorld :: *World
newWorld = code inline {
	fillI 65536 0
}

STACK_TO_STR stack
	:== if(stack.bounded) ("["+(join","(toList (S_map toString stack)))+"]") ("["<+headOf stack<+"...]")
	
MEM_TO_STR memory=:{cursor, note, left, right, main}
	:== "{cursor="<+cursor<+",note="<+note<+",left="+STACK_TO_STR left+",right="+STACK_TO_STR right+",main="+STACK_TO_STR main+"}"

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
		= ({note=NaN,left={stack=[!],bounded=True},right={stack=[!],bounded=True},cursor=0,main={stack=[!El {stack=[!],bounded=True},Delim 0],bounded=True},delims=1,random=genRandInt seed}, world)
	| otherwise
		# ((seed, world), args) = case (parseInt (hd args), world) of
			(Just seed, world) = ((seed, world), tl args)
			(Nothing, world) = ((\(Timestamp seed, world) -> (seed, world))(time world), args)
		= ({note=NaN,left={stack=[!],bounded=True},right={stack=[!],bounded=True},cursor=0,main=fromList (parseArgs args++[Delim 0]) True,delims=1,random=genRandInt seed}, world)
where

	parseArgs :: [String] -> [Element]
	parseArgs [] = []
	parseArgs [head:tail] = [parseArg head:parseArgs tail]
	where
		parseArg arg
			# try = parseString arg
			| isJust try
				# (Just try) = try
				= (El {stack=[!fromInt el \\ el <- utf8ToUnicode try],bounded=True})
			# try = parseInts arg
			| isJust try
				# (Just try) = try
				= (El {stack=[!fromInt el \\ el <- try],bounded=True})
			# try = parseReals arg
			| isJust try
				# (Just try) = try
				= (El {stack=[!fromReal el \\ el <- try],bounded=True})
			| otherwise
				= abort "Invalid memory arguments!"

	parseInt :: (String -> (Maybe Int))
	parseInt => 'GenParse'.parseString
	
	parseString :: (String -> (Maybe String))
	parseString => 'GenParse'.parseString
	
	parseInts :: (String -> (Maybe [Int]))
	parseInts => 'GenParse'.parseString
	
	parseReals :: (String -> (Maybe [Real]))
	parseReals => 'GenParse'.parseString

initialize :: !Program ![String] *World -> (State, Memory, *World)
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
	annotated => [(orn, {x=x, y=y}) \\ y <- [0..] & line <-: commands, x <- [0..] & (Control (Start orn)) <-: line]


construct :: !Program !Flags -> (*(State, Memory, *World) -> *World)
construct program=:{dimension, source, commands, wrapping} flags = execute
where

	execute :: *(!State, !Memory, !*World) -> *World
	
	execute ({terminate=True}, memory, world)
		| flags.dump
			=  (execIO (putStrLn (MEM_TO_STR memory)) world)
		| otherwise
			=  world
	
	execute (state, memory=:{main={stack=[!]}}, world)
		= execute (state, {memory&main={stack=[!El zero,Delim 0],bounded=True}}, world)
	execute (state, memory=:{main={stack=[!El mid]}}, world)
		= execute (state, {memory&main={stack=[!El mid,Delim 0],bounded=True}}, world)
	execute (state, memory=:{main={stack=[!Delim _]}}, world)
		= execute (state, {memory&main={stack=[!El zero,Delim 0],bounded=True}}, world)
	execute (state, memory=:{main=main`=:{stack=[!Delim val:tail]}, cursor, delims}, world)
		= execute (state, {memory&main={main`&stack=tail},delims=dec delims,cursor=if(cursor==val) dec id cursor}, world)
	
	execute smw=:(state=:{location, direction, history}, memory, world)
		| 0 > location.x || location.x >= dimension.x || 0 > location.y || location.y >= dimension.y = let
			wrappedLocation = {x=(location.x + dimension.x) rem dimension.x, y=(location.y + dimension.y) rem dimension.y}
			in execute ({state&location=wrappedLocation,terminate=not wrapping}, memory, world)
		| otherwise
			#! (state, memory, world) = process commands.[location.y, location.x] smw
			=  (execute (TRAVERSE_ONE {state&history=source.[location.y, location.x]}, memory, world))
			
	writeLine :: !(Stack Number) -> (IO ())
	
	writeLine stack = putStrLn (if(flags.nums) (STACK_TO_STR stack)  (unicodeToUTF8 (map toInt (toList stack))))
	
	writeChar :: !Number -> (IO ())
	
	writeChar char = putStr (if(flags.nums) (char<+"\n") (unicodeToUTF8 [toInt char]))
	
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

	process :: !Command -> *(*(!State, !Memory, !*World) -> *(State, Memory, *World))
	
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
		
		mirror (state=:{direction}, memory=:{main={stack=[!El mid:_]}}, world)
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
		
		skip (state, memory=:{main={stack=[!El mid:_]}}, world)
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
		loop :: (!State, !Memory, !*World) -> (State, Memory, *World)
		loop smw=:(_, {left={stack=[!]}}, _) = smw
		loop (state=:{direction}, memory, world)
			| direction == dir
				= ({state&location=loc}, {memory&left=tailOf memory.left}, world)
			| otherwise
				= (state, memory, world)
		
	process (Control (Loop Right dir (Just loc))) = loop
	where
		loop :: (!State, !Memory, !*World) -> (State, Memory, *World)
		loop smw=:(_, {right={stack=[!]}}, _) = smw
		loop (state=:{direction}, memory, world)
			| direction == dir
				= ({state&location=loc}, {memory&right=tailOf memory.right}, world)
			| otherwise
				= (state, memory, world)
	
	process (Control (Goto dir (Just loc))) = goto
	where
		goto :: (!State, !Memory, !*World) -> (State, Memory, *World)
		goto (state=:{direction}, memory=:{main={stack=[!El mid`:other]}}, world)
			| direction == dir && TO_BOOL mid`
				= ({state&location=loc}, memory, world)
			| otherwise
				= (state, memory, world)
		
	process (Control (String)) = makeString
	where
		
		makeString (state=:{direction, location}, memory=:{main, delims}, world)
			= (TRAVERSE_SOME (length content + newlineAdjust) state, {memory&delims=inc delims,main=recon2 (El (fromStrictList [!fromInt el \\ el <- content] True), Delim delims, main)}, world)
		where
			
			delta => case direction of
				East = location.x+1
				West = dimension.x-location.x
				North = dimension.y-location.y
				South = location.y+1
				
			wrappedLine => let
				line => case direction of
					East = [c \\ c <-: source.[location.y]]
					West = reverse [c \\ c <-: source.[location.y]]
					North = reverse [src.[location.x] \\ src <-: source]
					South = [src.[location.x] \\ src <-: source]
			in line ++ ['\n'] ++ line
			
			newlineAdjust :: Int
			newlineAdjust => if(isMember 10 content) 0 1
			
			content :: [Int]
			content => (utf8ToUnicode o toString o takeWhile ((<>)'\'') o drop delta) wrappedLine
			
	process (Literal (Pi)) = app3 (id, \memory=:{main={stack=[!El mid:_]}} -> {memory&main=recons (El (recons (fromReal pi, mid)), tailOf memory.main)}, id)

	process (Literal (Quote)) = app3 (id, \memory=:{main={stack=[!El mid:_]}} -> {memory&main=recons (El (recons (fromInt (toInt '\''), mid)), tailOf memory.main)}, id)

	process (Literal (Digit val)) = literal
	where
	
		literal :: (!State, !Memory, !*World) -> (State, Memory, *World)
		
		literal (state=:{history}, memory=:{main}, world)
			| isDigit history = let
				[!El mid`=:{stack=[!top:mid]}:base] = main.stack
				res = top * (fromInt 10) + val
				in (state, {memory&main={main&stack=[!El {mid`&stack=[!res:mid]}:base]}}, world)
			| otherwise = let
				[!El mid`:base] = main.stack
				in (state, {memory&main={main&stack=[!El (fromSingle val + mid`):base]}}, world)
						
	process (Literal (Alphabet lettercase)) = app3 (id, \memory -> {memory&delims=inc memory.delims,main=recon2 (El (fromStrictList literal True), Delim memory.delims, memory.main)}, id)
	where
	
		literal :: [!Number]
		literal => case lettercase of
			Lowercase = [!fromInt (toInt c) \\ c <-: "abcdefghijklmnopqrstuvwxyz"]
			Uppercase = [!fromInt (toInt c) \\ c <-: "ABCDEFGHIJKLMNOPQRSTUVWXYZ"]

	process (Literal (EmptySet)) = app3 (id, \memory -> {memory&main=recons (El zero, memory.main)}, id)
				
	process (Variable (Random)) = app3 (id, rand, id)
	where
	
		rand memory=:{main, random=[rng:random]}
			# (El mid, other) = decons main
			= {memory&main=recons (El (recons (fromInt rng, mid)), other), random=random}
	
	process (Environment env) = environment
	where
	
		environment (state, memory, world)
			# (memory, world) = env (memory, world)
			= (state, memory, world)
				
	process (Operator (IO_WriteAll)) = writeAll
	where
		
		writeAll (state, memory=:{main}, world)
			# (El mid, other) = decons main
			# world = execIO (writeLine ( mid)) world
			= (state, {memory&main=other}, world)
			
	process (Operator (IO_ReadAll)) = readAll
	where
		
		readAll (state, memory=:{main, delims}, world)
			# (str, world) = readLine world
			= (state, {memory&delims=inc delims,main=recon2 (El (fromList str True), Delim delims, main)}, world)
			
	process (Operator (IO_WriteOnce)) = writeOnce
	where
	
		writeOnce (state, memory=:{main={stack=[!El{stack=[!]}:_]}}, world)
			= (state, memory, world)
			
		writeOnce (state, memory=:{main=main`=:{stack=[!El mid`=:{stack=[!top:mid]}:other]}}, world)
			# world = execIO (writeChar top) world
			= (state, {memory&main={main`&stack=[!El {mid`&stack=mid}:other]}}, world)
			
	process (Operator (IO_ReadOnce)) = readOnce
	where
	
		readOnce (state, memory=:{main=main`=:{stack=[!El mid`:other]}}, world)
			# (chr, world) = readChar world
			= (state, {memory&main={main`&stack=[!El (fromSingle chr + mid`):other]}}, world)
			
	process (Operator (IO_Interrobang)) = interrobang
	where
	
		interrobang (state, memory=:{main=main`=:{stack=[!El {stack=[!]}:other]}}, world)
			# (chr, world) = readChar world
			= (state, {memory&main={main`&stack=[!El (fromSingle chr):other]}}, world)
		interrobang (state, memory=:{main=main`=:{stack=[!El mid`=:{stack=[!top:mid]}:other]}}, world)
			# world = execIO (writeChar top) world
			# (chr, world) = readChar world
			= (state, {memory&main={main`&stack=[!El {mid`&stack=[!chr:mid]}:other]}}, world)
			
	process (Operator (IO_ClearConsole)) = app3 (id, id, clearConsole)

	process (Operator (Binary_NN_N inv op)) = app3 (id, binary flags.strict inv, id)
	where
		
		binary :: !Bool !Bool Memory -> Memory
		binary _ _ memory=:{left={stack=[!lhs:_]}, right={stack=[!rhs:_]}}
			# (El mid, other) = decons memory.main
			#! val = op lhs rhs
			#! mid = recons (val, mid)
			= {memory&main=recons (El mid, other)}
		binary False _ memory=:{left={stack=[!lhs:_]}, main={stack=[!El {stack=[!_:_]}:_]}, right={stack=[!]}}
			# (El mid, other) = decons memory.main
			# (top, mid) = decons mid
			#! val = op lhs top
			#! mid = recons (val, mid)
			= {memory&main=recons (El mid, other)}
		binary False _ memory=:{left={stack=[!]}, main={stack=[!El {stack=[!_:_]}:_]}, right={stack=[!rhs:_]}}
			# (El mid, other) = decons memory.main
			# (top, mid) = decons mid
			#! val = op top rhs
			#! mid = recons (val, mid)
			= {memory&main=recons (El mid, other)}
		binary False _ memory=:{left={stack=[!]}, main={stack=[!El {stack=[!]}:_]}, right={stack=[!rhs,lhs:_]}}
			#! val = op lhs rhs
			#! mid = fromSingle val
			= {memory&main=recons (El mid, tailOf memory.main)}
		binary False _ memory=:{left={stack=[!lhs,rhs:_]}, main={stack=[!El {stack=[!]}:_]}, right={stack=[!]}}
			#! val = op lhs rhs
			#! mid = fromSingle val
			= {memory&main=recons (El mid, tailOf memory.main)}
		binary False True memory=:{left={stack=[!]}, main={stack=[!El {stack=[!_,_:_]}:_]}, right={stack=[!]}}
			# (El mid, other) = decons memory.main
			# (arg1, arg2, mid) = decon2 mid
			#! val = op arg1 arg2
			#! mid = recons (val, mid)
			= {memory&main=recons (El mid, other)}
		binary _ _ memory = memory
		
			
	process (Operator (Binary_NN_S inv op)) = app3 (id, binary flags.strict inv, id)
	where // productive
		
		binary :: !Bool !Bool Memory -> Memory
		binary _ _ memory=:{delims, left={stack=[!lhs:_]}, right={stack=[!rhs:_]}}
			#! val = op lhs rhs
			= {memory&delims=inc delims,main=recon2 (El val, Delim delims, memory.main)}
		binary False _ memory=:{delims, left={stack=[!lhs:_]}, main={stack=[!El {stack=[!_:_]}:_]}, right={stack=[!]}}
			# (El mid, other) = decons memory.main
			# (top, mid) = decons mid
			#! val = op lhs top
			= {memory&delims=inc delims,main=recon3 (El val, Delim delims, El mid, other)}
		binary False _ memory=:{delims, left={stack=[!]}, main={stack=[!El {stack=[!_:_]}:_]}, right={stack=[!rhs:_]}}
			# (El mid, other) = decons memory.main
			# (top, mid) = decons mid
			#! val = op top rhs
			= {memory&delims=inc delims,main=recon3 (El val, Delim delims, El mid, other)}
		binary False _ memory=:{delims, left={stack=[!lhs,rhs:_]}, main={stack=[!El {stack=[!]}:_]}, right={stack=[!]}}
			#! val = op lhs rhs
			= {memory&delims=inc delims,main=recon2 (El val, Delim delims, memory.main)}
		binary False _ memory=:{delims, left={stack=[!]}, main={stack=[!El {stack=[!]}:_]}, right={stack=[!rhs,lhs:_]}}
			#! val = op lhs rhs
			= {memory&delims=inc delims,main=recon2 (El val, Delim delims, memory.main)}
		binary False True memory=:{delims, left={stack=[!]}, main={stack=[!El {stack=[!_,_:_]}:_]}, right={stack=[!]}}
			# (El mid, other) = decons memory.main
			# (arg1, arg2, mid) = decon2 mid
			#! val = op arg1 arg2
			= {memory&delims=inc delims,main=recon3 (El val, Delim delims, El mid, other)}
		binary _ _ memory = memory
			
	process (Operator (Binary_SN_N op)) = app3 (id, binary flags.strict, id)
	where
	
		binary :: !Bool Memory -> Memory
		binary False memory=:{left={stack=[!]}, main={stack=[!El {stack=[!_:_]}:_]}, right={stack=[!rhs:_]}}
			# (El mid, other) = decons memory.main
			#! val = op mid rhs
			#! mid = fromSingle val
			= {memory&main=recons (El mid, other)}
		binary _ memory=:{left, right={stack=[!rhs:_]}}
			# (El mid, other) = decons memory.main
			#! val = op left rhs
			#! mid = recons (val, mid)
			= {memory&main=recons (El mid, other)}
		binary False memory=:{left, main={stack=[!El {stack=[!_:_]}:_]}, right={stack=[!]}}
			# (El mid, other) = decons memory.main
			# (top, mid) = decons mid
			#! val = op left top
			#! mid = recons (val, mid)
			= {memory&main=recons (El mid, other)}
		binary _ memory = memory
		
	process (Operator (Binary_NS_N op)) = app3 (id, binary flags.strict, id)
	where
	
		binary :: !Bool Memory -> Memory
		binary False memory=:{left={stack=[!lhs:_]}, main={stack=[!El {stack=[!_:_]}:_]}, right={stack=[!]}}
			# (El mid, other) = decons memory.main
			#! val = op lhs mid
			#! mid = fromSingle val
			= {memory&main=recons (El mid, other)}
		binary _ memory=:{left={stack=[!lhs:_]}, right}
			# (El mid, other) = decons memory.main
			#! val = op lhs right
			#! mid = recons (val, mid)
			= {memory&main=recons (El mid, other)}
		binary False memory=:{left={stack=[!]}, main={stack=[!El {stack=[!_:_]}:_]}, right}
			# (El mid, other) = decons memory.main
			# (top, mid) = decons mid
			#! val = op top right
			#! mid = recons (val, mid)
			= {memory&main=recons (El mid, other)}
		binary _ memory = memory
		
	process (Operator (Binary_SS_N inv op)) = app3 (id, binary flags.strict inv, id)
	where
	
		binary :: !Bool !Bool Memory -> Memory
		binary False True memory=:{left={stack=[!]}, main={stack=[!El {stack=[!_:_]},El {stack=[!_:_]}:_]}, right={stack=[!]}}
			# (El mid, El oth, other) = decon2 memory.main
			#! val = op mid oth
			#! mid = fromSingle val
			= {memory&main=recons (El mid, other)}
		binary False True memory=:{left={stack=[!]}, main={stack=[!El {stack=[!_:_]}:_]}, right={stack=[!]}}
			# (El mid, other) = decons memory.main
			#! val = op mid zero
			#! mid = fromSingle val
			= {memory&main=recons (El mid, other)}
		binary False _ memory=:{left={stack=[!]}, main={stack=[!El {stack=[!]}:_]}, right={stack=[!]}}
			#! val = op zero zero
			#! mid = fromSingle val
			= {memory&main=recons (El mid, memory.main)}
		binary False _ memory=:{left={stack=[!]}, main={stack=[!El {stack=[!_:_]}:_]}, right}
			# (El mid, other) = decons memory.main
			#! val = op mid right
			#! mid = fromSingle val
			= {memory&main=recons (El mid, other)}
		binary False _ memory=:{left, main={stack=[!El {stack=[!_:_]}:_]}, right={stack=[!]}}
			# (El mid, other) = decons memory.main
			#! val = op left mid
			#! mid = fromSingle val
			= {memory&main=recons (El mid, other)}
		binary _ _ memory=:{left, main, right}
			# (El mid, other) = decons memory.main
			#! val = op left right
			#! mid = recons (val, mid)
			= {memory&main=recons (El mid, other)}
		
	process (Operator (Binary_SS_S inv op)) = app3 (id, binary flags.strict inv, id)
	where
	
		binary :: !Bool !Bool Memory -> Memory
		binary False True memory=:{left={stack=[!]}, main={stack=[!El {stack=[!_:_]},El {stack=[!_:_]}:_]}, right={stack=[!]}}
			# (El mid, El oth, other) = decon2 memory.main
			#! val = op mid oth
			= {memory&main=recons (El val, other)}
		binary False True memory=:{left={stack=[!]}, main={stack=[!El {stack=[!_:_]}:_]}, right={stack=[!]}}
			# (El mid, other) = decons memory.main
			#! val = op mid zero
			= {memory&main=recons (El val, other)}
		binary False _ memory=:{delims, left={stack=[!]}, main={stack=[!El {stack=[!]}:_]}, right={stack=[!]}}
			#! val = op zero zero
			= {memory&delims=inc delims,main=recon2 (El val, Delim delims, memory.main)}
		binary False _ memory=:{left={stack=[!]}, main={stack=[!El{stack=[!_:_]}:_]}, right}
			# (El mid, other) = decons memory.main
			#! val = op mid right
			= {memory&main=recons (El val, other)}
		binary False _ memory=:{left, main={stack=[!El{stack=[!_:_]}:_]}, right={stack=[!]}}
			# (El mid, other) = decons memory.main
			#! val = op left mid
			= {memory&main=recons (El val, other)}
		binary _ _ memory=:{delims, left, main, right}
			#! val = op left right
			= {memory&delims=inc delims,main=recon2 (El val, Delim delims, main)}
			
	process (Operator (Unary_N_N op)) = app3 (id, unary, id)
	where
		
		unary :: Memory -> Memory
		unary memory=:{main={stack=[!El {stack=[!_:_]}:_]}}
			# (El mid, other) = decons memory.main
			# (arg, mid) = decons mid
			#! val = op arg
			#! mid = recons (val, mid)
			= {memory&main=recons (El mid, other)}
		unary memory = memory
		
	process (Operator (Unary_N_S op)) = app3 (id, unary, id)
	where
		
		unary :: Memory -> Memory
		//unary memory=:{delims, main=[El [arg]:other]}
		//	= {memory&delims=inc delims,main=[El (op arg),Delim delims: other]}
		unary memory=:{delims, main={stack=[!El {stack=[!_:_]}:_]}}
			# (El mid, other) = decons memory.main
			# (arg, mid) = decons mid
			#! val = op arg
			= {memory&delims=inc delims,main=recon3 (El val, Delim delims, El mid, other)}
		unary memory = memory
		
	process (Operator (Unary_S_N op)) = app3 (id, unary, id)
	where
	
		unary :: Memory -> Memory
		unary memory
			# (El mid, other) = decons memory.main
			#! val = op mid
			#! mid = fromSingle val
			= {memory&main=recons (El mid, other)}
		//unary memory = memory
		
	process (Operator (Unary_S_S op)) = app3 (id, unary, id)
	where
		
		unary :: Memory -> Memory
		unary memory
			# (El mid, other) = decons memory.main
			#! mid = op mid
			= {memory&main=recons (El mid, other)}
		//unary memory = memory
	
	process (Operator (Unary_S_T op)) = app3 (id, unary, id)
	where
		
		unary :: Memory -> Memory
		unary memory=:{cursor, delims, main={stack=[!El mid`:other],bounded}}
			= mergeDelims {memory&cursor=delims,delims=inc delims,main=(op mid`) + {stack=[!Delim delims: other],bounded=bounded}}
		
				
	process (Operator (Unary_M_M op)) = app3 (id, op, id)
			
