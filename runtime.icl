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

UNCURRY_EXEC (stack, memory, flags) :== execute stack memory flags

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

CHECK_MIDDLE stack //:== stack
	:== case stack of
		[] = [[[]]]
		[[]:tail] = [[[]]:tail]
		stack = stack

//SET_HISTORY memory=:{history} command
//	:== {memory&history=[command:history]}

evaluate :: [String] *World -> *(Memory, *World)
evaluate args world
	| isEmpty args
		# (Timestamp seed, world)
			= time world
		= ({left=[],right=[],main=[],random=genRandInt seed}, world)
	# ((seed, world), args)
		= case (parseInt (hd args), world) of
			(Just seed, world) = ((seed, world), tl args)
			(Nothing, world) = ((\(Timestamp seed, world) -> (seed, world))(time world), args)
	= ({left=[],right=[],main=[],random=genRandInt seed}, world)
where
	parseInt :: (String -> (Maybe Int))
	parseInt = 'GenParse'.parseString
	parseReal :: (String -> (Maybe Real))
	parseReal = 'GenParse'.parseString

execute :: State Memory *(Flags, *World) -> *World
execute state=:{dimension, location, direction, source, program, wrapping, history} memory=:{left, right, main, random} (flags, world)
	| location.x < 0 || location.x >= dimension.x || location.y < 0 || location.y >= dimension.y
		= if(wrapping) (execute {state&location={x=location.x rem dimension.x, y=location.y rem dimension.y},history='\n'} memory (flags, world)) (if(flags.dump) (execIO (putStrLn("{left="+STACK_TO_STR left+",right="+STACK_TO_STR right+",main="+(STACK_TO_STR o (map STACK_TO_STR) o (map (map STACK_TO_STR))) main+"}"))) id world)
	= process ((program !! location.y) !! location.x) (flags, world)
where
	curryExec state = execute {state&history=command}
	curryNone = execute {contState&history=command}
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
	getBothSingleArgs memory=:{left, right, main}
		= case (left, right, CHECK_MIDDLE main) of
			([lhs:left],  [rhs:right], _) = (Just (lhs, rhs), {memory&left=left,right=right})
			([], [rhs:right], [[[lhs:mid]:base]:other]) = (Just (lhs, rhs), {memory&right=right,main=[[mid:base]:other]})
			([], [rhs,lhs:right], [[[]:_]:_]) = (Just (lhs, rhs), {memory&right=right})
			([lhs:left], [], [[[rhs:mid]:base]:other]) = (Just (lhs, rhs), {memory&left=left,main=[[mid:base]:other]})
			([lhs,rhs:left], [], [[[]:_]:_]) = (Just (lhs, rhs), {memory&left=left})
			//([], [], [[[lhs,rhs:mid]:base]:other]) = (Just (lhs, rhs), {memory&main=[[mid:base]:other]})
			_ = (Nothing, memory)
	getMiddleSingleArg memory=:{main}
		= case (CHECK_MIDDLE main) of
			[[[arg:mid]:base]:other] = (Just arg, {memory&main=[[mid:base]:other]})
			_ = (Nothing, memory)// abort "Cannot find argument, perhaps you lost it?"
	getLeftSingleArg memory=:{left, main}
		= case (left, CHECK_MIDDLE main) of
			([arg:left], _) = (Just arg, {memory&left=left})
			([], [[[arg:mid]:base]:other]) = (Just arg, {memory&main=[[mid:base]:other]})
			_ = (Nothing, memory)
	getRightSingleArg memory=:{right, main}
		= case (right, CHECK_MIDDLE main) of
			([arg:right], _) = (Just arg, {memory&right=right})
			([], [[[arg:mid]:base]:other]) = (Just arg, {memory&main=[[mid:base]:other]})
			_ = (Nothing, memory)
	getMiddleStackArg memory=:{main}
		= case (CHECK_BASELINE main) of
			[[arg:base]:other] = (Just arg, {memory&main=[base:other]})
			_ = (Nothing, memory)// abort "Not enogh stacks!"
	getBaselineArg memory=:{main}
		= case main of
			[arg:other] = (Just arg, {memory&main=other})
			_ = (Nothing, memory)//abort "Empty main stack!"
	getBothStackArgs memory=:{left, right, main}
		= case (left, right, CHECK_MIDDLE main) of
			//([], [], [[lhs,rhs:base]:other]) = (Just (lhs, rhs), {memory&main=[base:other]})
			([], [], _) = (Nothing, memory)
			([], rhs, [[lhs:base]:other]) = (Just (lhs, rhs), {memory&right=[],main=[base:other]})
			(lhs, [], [[rhs:base]:other]) = (Just (lhs, rhs), {memory&left=[],main=[base:other]})
			(lhs, rhs, _) = (Just (lhs, rhs), {memory&left=[],right=[]})
			_ = (Nothing, memory)// abort "Cannot find stack arguments, perhaps you lost them?"
	getStackSingleArgs memory=:{left, right, main}
		= case (left, right, CHECK_MIDDLE main) of
			([], [rhs:right], [[lhs:base]:other]) = (Just (lhs, rhs), {memory&right=right,main=[base:other]})
			(lhs, [], [[[rhs:mid]:base]:other]) = (Just (lhs, rhs), {memory&left=[],main=[[mid:base]:other]})
			(lhs, [rhs:right], _) = (Just (lhs, rhs), {memory&left=[],right=right})
			_ = (Nothing, memory)// abort "Cannot unpack arguments!"
	writeSingle number = if(flags.nums) (putStr (toString number)) (putStr (unicodeToUTF8 [toInt number]))
	writeMany numbers = if(flags.nums) (putStrLn ("["+join","(map toString numbers)+"]")) (putStrLn (unicodeToUTF8 (map toInt numbers)))
	contState = {state&location=TRAVERSE_ONE location direction}
	process :: Command *(Flags, *World) -> *World
	process (Control Terminate) (flags, world)
		= if(flags.dump) (execIO (putStrLn("{left="+STACK_TO_STR left+",right="+STACK_TO_STR right+",main="+(STACK_TO_STR o (map STACK_TO_STR) o (map (map STACK_TO_STR))) main+"}"))) id world
	process (Control (NOOP)) fw
		= curryNone memory fw
	process (Control (Start dir)) fw
		= curryExec {state
			&location = TRAVERSE_ONE location dir
			,direction = dir
			} memory fw
	process (Control (Change cond dir)) fw
		= curryExec (if(cond || isTrue Middle) {state
			&location = TRAVERSE_ONE location dir
			,direction = dir
			} contState) memory fw
	process (Control (Bounce cond angle)) fw
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
		= curryExec (if(cond || isTrue Middle) {state
			&location = TRAVERSE_ONE location angle
			,direction = dir
			} contState) memory fw
	process (Control (Either cond axis)) fw
		# [val:random] = random
		# dir = case axis of
			Horizontal = if(isEven val) West East
			Vertical = if(isEven val) North South
		= curryExec (if(cond || isTrue Middle) {state
			&location = TRAVERSE_ONE location dir
			,direction = dir
			} contState) memory fw
	process (Control (Mirror cond axis)) fw
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
		= curryExec (if(cond || isTrue Middle) {state
			&location = TRAVERSE_ONE location dir
			,direction = dir
			} contState) memory fw
	process (Control (Turn rot)) fw
		# dir = case (direction, rot) of
			(East, Anticlockwise) = North
			(West, Anticlockwise) = South
			(North, Anticlockwise) = West
			(South, Anticlockwise) = East
			(East, Clockwise) = South
			(West, Clockwise) = North
			(North, Clockwise) = East
			(South, Clockwise) = West
		= curryExec {state
			&location = TRAVERSE_ONE location dir
			,direction = dir
			} memory fw
	process (Control (Loop stack dir)) fw
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
		= curryExec (if(SAME_DIRECTION direction dir && isTrue stack) {state
			&location = match
			} contState) memory fw
	process (Control String) fw
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
		= curryExec {state
			&location=TRAVERSE_SOME (length content + 2) location direction
			} memory fw
	process (Literal (Pi)) fw
		# [[mid:base]:other] = CHECK_MIDDLE main
		# memory = {memory&main=[[[fromReal pi:mid]:base]:other]}
		= curryNone memory fw
	process (Literal (Quote)) fw
		# [[mid:base]:other] = CHECK_MIDDLE main
		# memory = {memory&main=[[[fromInt(toInt'\''):mid]:base]:other]}
		= curryNone memory fw
	process (Literal (Digit num)) fw
		# [[mid:base]:other] = CHECK_MIDDLE main
		# (top, mid) = if(isDigit history) (hd mid, tl mid) (Zero, mid)
		#!value = top * (Re (Fin (Int 10))) + num
		# memory = {memory&main=[[[value:mid]:base]:other]}
		= curryNone memory fw
	process (Literal (Alphabet ltr)) fw
		# [base:other] = CHECK_BASELINE main
		# alphabet = utf8ToUnicode case ltr of
			Uppercase = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
			Lowercase = "abcdefghijklmnopqrtsuvwxyz"
		# memory = {memory&main=[[map fromInt alphabet:base]:other]}
		= curryNone memory fw
	process (Variable (Random)) fw
		# [[mid:base]:other] = CHECK_MIDDLE main
		# memory = {memory&main=[[[fromInt (hd random):mid]:base]:other],random=tl random}
		= curryNone memory fw
	process (Variable (Quine)) fw
		= abort "Quine unimplemented!"
	process (Variable (History)) fw
		= abort "History unimplemented!" 
	process (Operator (IO_WriteAll)) (flags, world)
		# (mid, memory) = getMiddleStackArg memory
		| isNothing mid = curryNone memory (flags, world)
		# (Just mid) = mid
		#!world = execIO (writeMany mid) world
		= curryNone memory (flags, world)
	process (Operator (IO_ReadAll)) (flags, world)
		# [base:other] = CHECK_MIDDLE main
		#!(str, world) = evalIO getLine world
		# str = utf8ToUnicode str
		# memory = {memory&main=[[map fromInt str:base]:other]}
		= curryNone memory (flags, world)
	process (Operator (IO_ReadWrite)) fw
		= abort "ReadWrite unimplemented!"
	process (Operator (IO_WriteRead)) fw
		= abort "WriteRead unimplemented!"
	process (Operator (IO_WriteOnce)) (flags, world)
		# (arg, memory) = getMiddleSingleArg memory
		| isNothing arg = curryNone memory (flags, world)
		# (Just arg) = arg
		#!world = execIO (writeSingle arg) world
		= curryNone memory (flags, world)
	process (Operator (IO_ReadOnce)) (flags, world)
		# [[mid:base]:other] = CHECK_MIDDLE main
		#!(chr, world) = evalIO getChar world
		# chr = toInt chr
		# str = [chr]
		# str = str ++ if(chr >= 194) [toInt (unsafePerformIO (evalIO getChar))] [] // TODO: make safe
		# str = str ++ if(chr >= 224) [toInt (unsafePerformIO (evalIO getChar))] [] // TODO: make safe
		# str = str ++ if(chr >= 240) [toInt (unsafePerformIO (evalIO getChar))] [] // TODO: make safe
		# str = utf8ToUnicode (toString str)
		# memory = {memory&main=[[map fromInt str++mid:base]:other]}
		= curryNone memory (flags, world)
	process (Operator (IO_Interrobang)) (flags, world)
		= abort "Interrobang unimplemented!"
	process (Operator (IO_Bell)) (flags, world)
		= abort "Bell unimplemented!"
	process (Operator (IO_Timestamp)) (flags, world)
		# [[mid:base]:other] = CHECK_MIDDLE main
		# transform = (\e -> (\{sec,min,hour,mday,mon,year,wday,yday} -> [toInt(unsafePerformIO time),sec,min,hour,wday,mday-1,mon,yday,year+1900]) (unsafePerformIO (toLocalTime (Timestamp e)))) // TODO: make safe
		# (stamp, mid) = case mid of
			[] = (transform (toInt (unsafePerformIO time)), mid) // TODO: make safe
			[top:mid] = (transform (if(isTrue Middle) (toInt top) (toInt (unsafePerformIO time))), mid) // TODO: make safe
		# memory = {memory&main=[[map fromInt stamp++mid:base]:other]}
		= curryNone memory (flags, world)
	process (Operator (IO_Sleep)) (flags, world)
		= abort "Sleep unimplemented!"
	process (Operator (Binary op)) fw
		# (args, memory=:{main}) = getBothSingleArgs memory
		| isNothing args = curryNone memory fw
		# (Just (lhs, rhs)) = args
		# [[mid:base]:other] = CHECK_MIDDLE main
		# memory = {memory&main=[[[op lhs rhs:mid]:base]:other]}
		= curryNone memory fw
	process (Operator (Unary op)) fw
		# (arg, memory=:{main}) = getMiddleSingleArg memory
		| isNothing arg = curryNone memory fw
		# (Just arg) = arg
		# [[mid:base]:other] = CHECK_MIDDLE main
		# memory = {memory&main=[[[op arg:mid]:base]:other]}
		= curryNone memory fw
	process (Stack (SwapLeftRight)) fw
		# memory = {memory&left=right,right=left}
		= curryNone memory fw
	process (Stack (MoveTop East)) fw
		# (lhs, left) = (SAFE_HEAD left, SAFE_TAIL left)
		# memory = {memory&left=left,right=lhs++right}
		= curryNone memory fw
	process (Stack (MoveTop West)) fw
		# (rhs, right) = (SAFE_HEAD right, SAFE_TAIL right)
		# memory = {memory&right=right,left=rhs++left}
		= curryNone memory fw
	process (Stack (MoveTop NorthEast)) fw
		# [[mid:base]:other] = CHECK_MIDDLE main
		# (top, mid) = (SAFE_HEAD mid, SAFE_TAIL mid)
		# memory = {memory&right=top++right,main=[[mid:base]:other]}
		= curryNone memory fw
	process (Stack (MoveTop NorthWest)) fw
		# [[mid:base]:other] = CHECK_MIDDLE main
		# (top, mid) = (SAFE_HEAD mid, SAFE_TAIL mid)
		# memory = {memory&left=top++left,main=[[mid:base]:other]}
		= curryNone memory fw
			