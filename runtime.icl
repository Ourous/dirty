implementation module runtime

import types, converter, atomics, arithmetic
import StdEnv, StdLib, System.IO, System.Time, Math.Random, System._Unsafe, Text
from Math.Geometry import pi
import qualified Data.Generics.GenParse as GenParse

STACK_TO_STR stack
	:== "["+join","(map toString stack)+"]"

SAME_STACK_ID lhs rhs
	:== case (lhs, rhs) of
		(Left, Left) = True
		(Right, Right) = True
		(Middle, Middle) = True
		_ = False
		
SAME_DIRECTION lhs rhs
	:== case (lhs, rhs) of
		(North, North) = True
		(West, West) = True
		(East, East) = True
		(South, South) = True
		_ = False

TRAVERSE_SOME dist loc=:{x,y} dir
	:== case dir of 
		East = {loc&x=x+dist}
		West = {loc&x=x-dist}
		North = {loc&y=y-dist}
		South = {loc&y=y+dist}
		NorthEast = {x=x+dist,y=y-dist}
		NorthWest = {x=x-dist,y=y-dist}
		SouthWest = {x=x-dist,y=y+dist}
		SouthEast = {x=x+dist,y=y+dist}
		
TRAVERSE_ONE :== (TRAVERSE_SOME 1)

CHECK_BASELINE stack
	:== case stack of
		[] = [[]]
		stack = stack

CHECK_MIDDLE stack
	:== case stack of
		[] = [[[]]]
		[[]:tail] = [[[]]:tail]
		stack = stack
	
SET_HISTORY memory=:{history} command
	:== {memory&history=[command:history]}

evaluate :: [String] *World -> *(Memory, *World)
evaluate args world
	| isEmpty args
		# (Timestamp seed, world)
			= time world
		= ({left=[],right=[],main=[],history=[],random=genRandInt seed}, world)
	# ((seed, world), args)
		= case (parseInt (hd args), world) of
			(Just seed, world) = ((seed, world), tl args)
			(Nothing, world) = ((\(Timestamp seed, world) -> (seed, world))(time world), args)
	= ({left=[],right=[],main=[],history=[],random=genRandInt seed}, world)
where
	parseInt :: (String -> (Maybe Int))
	parseInt = 'GenParse'.parseString
	parseReal :: (String -> (Maybe Real))
	parseReal = 'GenParse'.parseString

execute :: State Memory *World Flags -> *World
execute state=:{dimension, location, direction, source, program, wrapping} memory=:{left, right, main, random, history} world flags
	| location.x < 0 || location.x >= dimension.x || location.y < 0 || location.y >= dimension.y
		= if(wrapping) (execute {state&location={x=location.x rem dimension.x, y=location.y rem dimension.y}} (SET_HISTORY memory '\n') world flags) (if(flags.dump) (execIO (putStrLn("{left="+STACK_TO_STR left+",right="+STACK_TO_STR right+",main="+(STACK_TO_STR o (map STACK_TO_STR) o (map (map STACK_TO_STR))) main+"}"))) id world)
	= process ((program !! location.y) !! location.x) world flags
where
	command = (source !! location.y) !! location.x
	isTrue stack
		# val = case stack of
			Left = left
			Right = right
			Middle = hd(hd(CHECK_MIDDLE main))
		= case val of
			[] = False
			[NaN:_] = False
			[Zero:_] = False
			_ = True
	getBothArgs memory=:{left, right, main}
		# (lhs, rhs, main) = case (left, right, CHECK_MIDDLE main) of
			([lhs:_],  [rhs:_], _) = (lhs, rhs, main)
			([], [rhs:_], [[[lhs:mid]:base]:other]) = (lhs, rhs, [[mid:base]:other])
			([], [rhs,lhs:_], [[[]:_]:_]) = (lhs, rhs, main)
			([lhs:_], [], [[[rhs:mid]:base]:other]) = (lhs, rhs, [[mid:base]:other])
			([lhs,rhs:_], [], [[[]:_]:_]) = (lhs, rhs, main)
			([], [], [[[lhs,rhs:mid]:base]:other]) = (lhs, rhs, [[mid:base]:other])
			_ = abort "Cannot find arguments, perhaps you lost them?"
		= ((lhs, rhs), {memory&main=main})
	writeSingle number = if(flags.nums) (putStr (toString number)) (putStr (unicodeToUTF8 [toInt number]))
	writeMany numbers = if(flags.nums) (putStrLn ("["+join","(map toString numbers)+"]")) (putStrLn (unicodeToUTF8 (map toInt numbers)))
	doNOOP = {state&location = TRAVERSE_ONE location direction}
	//process :: Command *World -> *(Flags -> *World)
	process (Control Terminate) world flags
		# world = if(flags.dump) (execIO (putStrLn("{left="+STACK_TO_STR left+",right="+STACK_TO_STR right+",main="+(STACK_TO_STR o (map STACK_TO_STR) o (map (map STACK_TO_STR))) main+"}"))) id world
		= const world flags
	process (Control (NOOP)) world flags
		= execute doNOOP (SET_HISTORY memory command) world flags
	process (Control (Start dir)) world flags
		= execute {state
			&location = TRAVERSE_ONE location dir
			,direction = dir
			} (SET_HISTORY memory command) world flags
	process (Control (Change cond dir)) world flags
		= execute (if(cond || isTrue Middle) {state
			&location = TRAVERSE_ONE location dir
			,direction = dir
			} doNOOP) (SET_HISTORY memory command) world flags
	process (Control (Bounce cond angle)) world flags
		# dir = case (direction, angle) of
			(West, NorthEast) = North
			(South, NorthEast) = East
			(East, NorthWest) = North
			(South, NorthWest) = West
			(East, SouthWest) = South
			(North, SouthWest) = West
			(West, SouthEast) = South
			(North, SouthEast) = East
			(dir, _) = dir
		= execute (if(cond || isTrue Middle) {state
			&location = TRAVERSE_ONE location angle
			,direction = dir
			} doNOOP) (SET_HISTORY memory command) world flags
	process (Control (Either cond axis)) world flags
		# [val:random] = random
		# dir = case axis of
			Horizontal = if(isEven val) West East
			Vertical = if(isEven val) North South
		= execute (if(cond || isTrue Middle) {state
			&location = TRAVERSE_ONE location dir
			,direction = dir
			} doNOOP) (SET_HISTORY memory command) world flags
	process (Control (Mirror cond axis)) world flags
		# dir = case (direction, axis) of
			(East, Reflection) = West
			(West, Reflection) = East
			(North, Reflection) = South
			(South, Reflection) = North
			(East, Vertical) = West
			(West, Vertical) = East
			(dir, Vertical) = dir
			(North, Horizontal) = South
			(South, Horizontal) = North
			(dir, Horizontal) = dir
			(East, Identity) = North
			(West, Identity) = South
			(North, Identity) = East
			(South, Identity) = West
			(East, Inverse) = South
			(West, Inverse) = North
			(North, Inverse) = West
			(South, Inverse) = East
		= execute (if(cond || isTrue Middle) {state
			&location = TRAVERSE_ONE location dir
			,direction = dir
			} doNOOP) (SET_HISTORY memory command) world flags
	process (Control (Turn rot)) world flags
		# dir = case (direction, rot) of
			(East, Anticlockwise) = North
			(West, Anticlockwise) = South
			(North, Anticlockwise) = West
			(South, Anticlockwise) = East
			(East, Clockwise) = South
			(West, Clockwise) = North
			(North, Clockwise) = East
			(South, Clockwise) = West
		= execute {state
			&location = TRAVERSE_ONE location dir
			,direction = dir
			} (SET_HISTORY memory command) world flags
	process (Control (Loop stack dir)) world flags
		# match = case dir of
			West = TRAVERSE_SOME (hd [i
				\\(Control (Loop s West)) <- drop (location.x+1) ((flatten o repeatn 2)program!!location.y)
				& i <- [0..] | SAME_STACK_ID stack s]) location East
			East = TRAVERSE_SOME (hd [i
				\\(Control (Loop s East)) <- drop (dimension.x-location.x) ((flatten o repeatn 2 o reverse)program!!location.y)
				& i <- [0..] | SAME_STACK_ID stack s]) location West
			South = TRAVERSE_SOME (hd [i
				\\(Control (Loop s South)) <- drop (dimension.y-location.y) ((flatten o repeatn 2 o reverse)(transpose program!!location.x))
				& i <- [0..] | SAME_STACK_ID stack s]) location North
			North = TRAVERSE_SOME (hd [i
				\\(Control (Loop s North)) <- drop (location.y+1) ((flatten o repeatn 2)(transpose program!!location.x))
				& i <- [0..] | SAME_STACK_ID stack s]) location South
		= execute (if(SAME_DIRECTION direction dir && isTrue stack) {state
			&location = match
			} doNOOP) (SET_HISTORY memory command) world flags
	process (Control String) world flags
		# (line, dif) = case direction of
			East = (source!!location.y, location.x+1)
			West = (reverse(source!!location.y), dimension.x-location.x)
			North = (reverse(transpose source!!location.x), dimension.y-location.y)
			South = (transpose source!!location.x, location.y+1)
		# line = line++['\n']++line
		# content = takeWhile ((<>)'\'') (drop dif line)
		# content = utf8ToUnicode (toString content)
		# [base:other] = CHECK_BASELINE main
		# memory = {memory&main=[[map fromInt content:base]:other]}
		= execute {state
			&location=TRAVERSE_SOME (length content + 2) location direction
			} (SET_HISTORY memory command) world flags
	process (Literal (Pi)) world flags
		# [[mid:base]:other] = CHECK_MIDDLE main
		# memory = {memory&main=[[[fromReal pi:mid]:base]:other]}
		= execute doNOOP (SET_HISTORY memory command) world flags
	process (Literal (Quote)) world flags
		# [[mid:base]:other] = CHECK_MIDDLE main
		# memory = {memory&main=[[[fromInt(toInt'\''):mid]:base]:other]}
		= execute doNOOP (SET_HISTORY memory command) world flags
	process (Literal (Digit int)) world flags
		# [[mid:base]:other] = CHECK_MIDDLE main
		# (top, mid) = if(isEmpty history || not (isDigit (hd history))) (Zero, mid) (hd mid, tl mid)
		# memory = {memory&main=[[[top*(fromInt 10)+(fromInt int):mid]:base]:other]}
		= execute doNOOP (SET_HISTORY memory command) world flags
	process (Literal (Alphabet ltr)) world flags
		# [base:other] = CHECK_BASELINE main
		# alphabet = utf8ToUnicode case ltr of
			Uppercase = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
			Lowercase = "abcdefghijklmnopqrtsuvwxyz"
		# memory = {memory&main=[[map fromInt alphabet:base]:other]}
		= execute doNOOP (SET_HISTORY memory command) world flags
	process (Variable (Random)) world flags
		# [[mid:base]:other] = CHECK_MIDDLE main
		# memory = {memory&main=[[[fromInt (hd random):mid]:base]:other],random=tl random}
		= execute doNOOP (SET_HISTORY memory command) world flags
	process (Variable (Quine)) world flags
		= abort "Quine unimplemented!"
	process (Variable (History)) world flags
		= abort "History unimplemented!" 
	process (Operator (IO_WriteAll)) world flags
		# [[mid:base]:other] = CHECK_MIDDLE main
		# out = writeMany mid
		# world = execIO out world
		# memory = {memory& main=[base:other]}
		= execute doNOOP (SET_HISTORY memory command) world flags
	process (Operator (IO_ReadAll)) world flags
		# [base:other] = CHECK_MIDDLE main
		# str = accUnsafe (evalIO getLine)
		# str = utf8ToUnicode str
		# memory = {memory&main=[[map fromInt str:base]:other]}
		= execute doNOOP (SET_HISTORY memory command) world flags
	process (Operator (IO_ReadWrite)) world flags
		= abort "ReadWrite unimplemented!"
	process (Operator (IO_WriteRead)) world flags
		= abort "WriteRead unimplemented!"
	process (Operator (IO_WriteOnce)) world flags
		# [[mid:base]:other] = CHECK_MIDDLE main
		# [top:mid] = mid
		# out = writeSingle top
		# world = execIO out world
		# memory = {memory&main=[[mid:base]:other]}
		= execute doNOOP (SET_HISTORY memory command) world flags
	process (Operator (IO_ReadOnce)) world flags
		# [[mid:base]:other] = CHECK_MIDDLE main
		# chr = toInt (accUnsafe (evalIO getChar))
		# str = [chr]
		# str = str ++ if(chr >= 194) [toInt (accUnsafe (evalIO getChar))] []
		# str = str ++ if(chr >= 224) [toInt (accUnsafe (evalIO getChar))] []
		# str = str ++ if(chr >= 240) [toInt (accUnsafe (evalIO getChar))] []
		# str = utf8ToUnicode (toString str)
		# memory = {memory&main=[[map fromInt str++mid:base]:other]}
		= execute doNOOP (SET_HISTORY memory command) world flags
	process (Operator (IO_Interrobang)) world flags
		= abort "Interrobang unimplemented!"
	process (Operator (IO_Bell)) world flags
		= abort "Bell unimplemented!"
	process (Operator (IO_Timestamp)) world flags
		# [[mid:base]:other] = CHECK_MIDDLE main
		# transform = (\e -> (\{sec,min,hour,mday,mon,year,wday,yday} -> [toInt(accUnsafe time),sec,min,hour,wday,mday-1,mon,yday,year+1900]) (accUnsafe (toLocalTime (Timestamp e))))
		# (stamp, mid) = case mid of
			[] = (transform (toInt (accUnsafe time)), mid)
			[top:mid] = (transform (if(isTrue Middle) (toInt top) (toInt (accUnsafe time))), mid)
		# memory = {memory&main=[[map fromInt stamp++mid:base]:other]}
		= execute doNOOP (SET_HISTORY memory command) world flags
	process (Operator (IO_Sleep)) world flags
		= abort "Sleep unimplemented!"
	process (Operator (Math_Modulus)) world flags
		= abort "Modulus unimplemented!"
	process (Operator (Math_Addition)) world flags
		# ((lhs, rhs), memory=:{main}) = getBothArgs memory
		# [[mid:base]:other] = CHECK_MIDDLE main
		# memory = {memory&main=[[[lhs+rhs:mid]:base]:other]}
		= execute doNOOP (SET_HISTORY memory command) world flags
	process (Operator (Math_Multiplication)) world flags
		# ((lhs, rhs), memory=:{main}) = getBothArgs memory
		# [[mid:base]:other] = CHECK_MIDDLE main
		# memory = {memory&main=[[[lhs*rhs:mid]:base]:other]}
		= execute doNOOP (SET_HISTORY memory command) world flags
	process (Operator (Math_Subtraction)) world flags
		# ((lhs, rhs), memory=:{main}) = getBothArgs memory
		# [[mid:base]:other] = CHECK_MIDDLE main
		# memory = {memory&main=[[[lhs-rhs:mid]:base]:other]}
		= execute doNOOP (SET_HISTORY memory command) world flags