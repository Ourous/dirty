implementation module Dirty.Runtime.Instruction

import Dirty.Backend
from Dirty.Runtime import ::Memory(..), ::State(..)
from Dirty.Frontend.Arguments import ::RuntimeFlags(..)
from Dirty.Types import ::Point(..), ::Region(..), ::Direction(..)
//import Dirty.Types
import Data.Matrix, Data.Func, Data.Maybe
import StdEnv, StdDebug, Text
import System.IO
import Text.Unicode.Encodings.UTF8

:: Instruction :== RuntimeFlags -> Operation

finalizeProgram :: RuntimeFlags (Matrix Instruction) -> Matrix Operation
finalizeProgram f m = hyperstrict {{i f \\ i <-: j} \\ j <-: m}

maybePrepend val :== case val of Nothing = id; (Just v) = push v
//maybeAppH fn arg out :== appH (maybe id fn (peek out)) arg
maybeAppBinary fn mem :== case pop2 mem.arg of
	(Just (a, b), arg) = {mem&arg=push (fn a b) arg,out=push b (push a mem.out)}
	_ = mem
	
maybeAppUnary fn mem :== case pop mem.arg of
	(Just a, arg) = {mem&arg=push (fn a) arg,out=push a mem.out}
	_ = mem

/*
maybeAppBinary fn mem :== case (pop mem.arg, peek mem.out) of
	((Just l, arg), Just r) = {mem&arg=arg,out=push (fn l r) mem.out}
	_ = mem
*/

appUnary :: (Value -> Value) -> Instruction
appUnary fn = \_ st=:{mem={arg,out}} w = ({st&mem.arg=appH fn arg,mem.out=maybe out (\e=push e out) (peek arg)}, w)

vecBinary :: (Value Value -> Value) -> Instruction
vecBinary fn = \_ st w = ({st&mem=maybeAppBinary fn st.mem}, w)

vecUnary :: (Number -> a) -> Instruction | toValue a
vecUnary fn = \_ st w = ({st&mem=maybeAppUnary (vectorizeUnary fn) st.mem}, w)
//vectorizedInstr fn :== \_ st w = ({st&mem.arg=appH (vectorizeUnary fn) st.mem.arg}, w)

// region literals
I_LITERAL_REGION :: (Value, Point) (Value, Point) (Value, Point) (Value, Point) -> Instruction
I_LITERAL_REGION (n_val, n_end) (e_val, e_end) (s_val, s_end) (w_val, w_end) = op
where
	op _ st=:{dir=North,mem} w
		= ({st&mem.arg=push n_val mem.arg,pos=n_end}, w)
	op _ st=:{dir=East,mem} w
		= ({st&mem.arg=push e_val mem.arg,pos=e_end}, w)
	op _ st=:{dir=South,mem} w
		= ({st&mem.arg=push s_val mem.arg,pos=s_end}, w)
	op _ st=:{dir=West,mem} w
		= ({st&mem.arg=push w_val mem.arg,pos=w_end}, w)

I_LITERAL_SINGLE :: Value -> Instruction
I_LITERAL_SINGLE val = \ _ st w = ({st&mem.arg=push val st.mem.arg}, w)

// tab swapping
I_HORIZONTAL_TAB_SWAP :: (Vector Point) -> Instruction
I_HORIZONTAL_TAB_SWAP vec = \ _ st=:{rng=[num:rng]} w = ({st&pos=vec.[num rem(size vec)],rng=rng}, w)
	
I_VERTICAL_TAB_SWAP :: (Vector Point) -> Instruction
I_VERTICAL_TAB_SWAP vec = \ _ st=:{rng=[num:rng]} w = ({st&pos=vec.[num rem(size vec)],rng=rng}, w)

// conditional gotos
I_MAYBE_GOTO_NORTH :: Point -> Instruction
I_MAYBE_GOTO_NORTH pos = op
where
	op _ st=:{dir=North} w
		| toBool st.mem.arg
			= ({st&pos=pos}, w)
		| otherwise
			= (st, w)
	op _ st w
		= (st, w)
	
I_MAYBE_GOTO_EAST :: Point -> Instruction
I_MAYBE_GOTO_EAST pos = op
where
	op _ st=:{dir=East} w
		| toBool st.mem.arg
			= ({st&pos=pos}, w)
		| otherwise
			= (st, w)
	op _ st w
		= (st, w)

I_MAYBE_GOTO_SOUTH :: Point -> Instruction
I_MAYBE_GOTO_SOUTH pos = op
where
	op _ st=:{dir=South} w
		| toBool st.mem.arg
			= ({st&pos=pos}, w)
		| otherwise
			= (st, w)
	op _ st w
		= (st, w)

I_MAYBE_GOTO_WEST :: Point -> Instruction
I_MAYBE_GOTO_WEST pos = op
where
	op _ st=:{dir=West} w
		| toBool st.mem.arg
			= ({st&pos=pos}, w)
		| otherwise
			= (st, w)
	op _ st w
		= (st, w)

// constant gotos
I_ALWAYS_GOTO_NORTH :: Point -> Instruction
I_ALWAYS_GOTO_NORTH pos = op
where
	op _ st=:{dir=North} w
		= ({st&pos=pos}, w)
	op _ st w
		= (st, w)

I_ALWAYS_GOTO_EAST :: Point -> Instruction
I_ALWAYS_GOTO_EAST pos = op
where
	op _ st=:{dir=East} w
		= ({st&pos=pos}, w)
	op _ st w
		= (st, w)

I_ALWAYS_GOTO_SOUTH :: Point -> Instruction
I_ALWAYS_GOTO_SOUTH pos = op
where
	op _ st=:{dir=South} w
		= ({st&pos=pos}, w)
	op _ st w
		= (st, w)

I_ALWAYS_GOTO_WEST :: Point -> Instruction
I_ALWAYS_GOTO_WEST pos = op
where
	op _ st=:{dir=West} w
		= ({st&pos=pos}, w)
	op _ st w
		= (st, w)

// conditional loops
I_MAYBE_LOOP_NORTH :: Point -> Instruction
I_MAYBE_LOOP_NORTH pos = op
where
	op _ st=:{dir=North} w
		| toBool st.mem.arg
			= ({st&pos=pos}, w)
		| otherwise
			= (st, w)
	op _ st w
		= (st, w)
	
I_MAYBE_LOOP_EAST :: Point -> Instruction
I_MAYBE_LOOP_EAST pos = op
where
	op _ st=:{dir=East} w
		| toBool st.mem.arg
			= ({st&pos=pos}, w)
		| otherwise
			= (st, w)
	op _ st w
		= (st, w)

I_MAYBE_LOOP_SOUTH :: Point -> Instruction
I_MAYBE_LOOP_SOUTH pos = op
where
	op _ st=:{dir=South} w
		| toBool st.mem.arg
			= ({st&pos=pos}, w)
		| otherwise
			= (st, w)
	op _ st w
		= (st, w)

I_MAYBE_LOOP_WEST :: Point -> Instruction
I_MAYBE_LOOP_WEST pos = op
where
	op _ st=:{dir=West} w
		| toBool st.mem.arg
			= ({st&pos=pos}, w)
		| otherwise
			= (st, w)
	op _ st w
		= (st, w)

// constant loops
I_ALWAYS_LOOP_NORTH :: Point -> Instruction
I_ALWAYS_LOOP_NORTH pos = op
where
	op _ st=:{dir=North} w
		= ({st&pos=pos}, w)
	op _ st w
		= (st, w)

I_ALWAYS_LOOP_EAST :: Point -> Instruction
I_ALWAYS_LOOP_EAST pos = op
where
	op _ st=:{dir=East} w
		= ({st&pos=pos}, w)
	op _ st w
		= (st, w)

I_ALWAYS_LOOP_SOUTH :: Point -> Instruction
I_ALWAYS_LOOP_SOUTH pos = op
where
	op _ st=:{dir=South} w
		= ({st&pos=pos}, w)
	op _ st w
		= (st, w)

I_ALWAYS_LOOP_WEST :: Point -> Instruction
I_ALWAYS_LOOP_WEST pos = op
where
	op _ st=:{dir=West} w
		= ({st&pos=pos}, w)
	op _ st w
		= (st, w)

// start
I_START_NORTH :: Point -> Instruction
I_START_NORTH pos = op
where
	op _ st=:{ini=Nothing} w
		= ({st&ini=Just pos,dir=North}, w)
	op _ st w
		= (st, w)
		
I_START_EAST :: Point -> Instruction
I_START_EAST pos = op
where
	op _ st=:{ini=Nothing} w
		= ({st&ini=Just pos,dir=East}, w)
	op _ st w
		= (st, w)
		
I_START_SOUTH :: Point -> Instruction
I_START_SOUTH pos = op
where
	op _ st=:{ini=Nothing} w
		= ({st&ini=Just pos,dir=South}, w)
	op _ st w
		= (st, w)
		
I_START_WEST :: Point -> Instruction
I_START_WEST pos = op
where
	op _ st=:{ini=Nothing} w
		= ({st&ini=Just pos,dir=West}, w)
	op _ st w
		= (st, w)
		
I_START_RANDOM :: Point -> Instruction
I_START_RANDOM pos = op
where
	op _ st=:{ini=Nothing,rng=[num:rng]} w
		= ({st&ini=Just pos,dir=case num rem 4 of 0=North;1=East;2=South;3=West,rng=rng}, w)
	op _ st w
		= (st, w)

// restarts
I_RESTART_RANDOM :: (Vector Point) -> Instruction
I_RESTART_RANDOM vec = op
where
	op {warnings=True} st=:{ini=Nothing} w
		= trace_n "Warning: restart used without start location" (st, w)
	op _ st=:{ini=Nothing} w
		= (st, w)
	op _ st=:{rng=[num:rng]} w
		= ({st&ini=Nothing,pos=vec.[num rem(size vec)],rng=rng}, w)

I_RESTART_LAST :: Instruction
I_RESTART_LAST = op
where
	op {warnings=True} st=:{ini=Nothing} w
		= trace_n "Warning: restart used without start location" (st, w)
	op _ st=:{ini=Nothing} w
		= (st, w)
	op _ st=:{ini=Just pos} w
		= ({st&ini=Nothing,pos=pos}, w)

// terminate
I_TERMINATE :: Instruction
I_TERMINATE = \ _ st w = ({st&end=True}, w)

// reflections
I_REFLECT_VERTICAL :: Instruction
I_REFLECT_VERTICAL = op
where
	op _ st w
		= ({st&dir=case st.dir of
				East = West
				West = East
				else = else
				},
			w)

I_REFLECT_HORIZONTAL :: Instruction
I_REFLECT_HORIZONTAL = op
where
	op _ st w
		= ({st&dir=case st.dir of
				North = South
				South = North
				else = else
				},
			w)

I_REFLECT_IDENTITY :: Instruction
I_REFLECT_IDENTITY = op
where
	op _ st w
		= ({st&dir=case st.dir of
				East = North
				North = East
				West = South
				South = West
				},
			w)

I_REFLECT_INVERSE :: Instruction
I_REFLECT_INVERSE = op
where
	op _ st w
		= ({st&dir=case st.dir of
				East = South
				South = East
				West = North
				North = West
				},
			w)
			
// skips
I_MAYBE_SKIP_NEXT :: Instruction
I_MAYBE_SKIP_NEXT = op
where
	op _ st=:{mem,pos,dir} w
		| toBool mem.arg
			= ({st&pos=case dir of
					North = {pos&y=pos.y-1}
					East = {pos&x=pos.x+1}
					South = {pos&y=pos.y+1}
					West = {pos&x=pos.x-1}
					},
				w)
		| otherwise
			= (st, w)

I_ALWAYS_SKIP_NEXT :: Instruction
I_ALWAYS_SKIP_NEXT = op
where
	op _ st=:{pos,dir} w
		= ({st&pos=case dir of
				North = {pos&y=pos.y-1}
				East = {pos&x=pos.x+1}
				South = {pos&y=pos.y+1}
				West = {pos&x=pos.x-1}
				},
			w)
	
// jumps		
I_MAYBE_JUMP_SE :: Instruction
I_MAYBE_JUMP_SE = op
where
	op _ st=:{mem,pos} w
		| toBool mem.arg
			= ({st&pos.y=pos.y+1,pos.x=pos.x+1}, w)
		| otherwise
			= (st, w)
			
I_MAYBE_JUMP_SW :: Instruction
I_MAYBE_JUMP_SW = op
where
	op _ st=:{mem,pos} w
		| toBool mem.arg
			= ({st&pos.y=pos.y+1,pos.x=pos.x-1}, w)
		| otherwise
			= (st, w)
			
I_MAYBE_JUMP_NW :: Instruction
I_MAYBE_JUMP_NW = op
where
	op _ st=:{mem,pos} w
		| toBool mem.arg
			= ({st&pos.y=pos.y-1,pos.x=pos.x-1}, w)
		| otherwise
			= (st, w)

I_MAYBE_JUMP_NE :: Instruction
I_MAYBE_JUMP_NE = op
where
	op _ st=:{mem,pos} w
		| toBool mem.arg
			= ({st&pos.y=pos.y-1,pos.x=pos.x+1}, w)
		| otherwise
			= (st, w)

I_ALWAYS_JUMP_SE :: Instruction
I_ALWAYS_JUMP_SE = \ _ st=:{pos={x,y}} w = ({st&pos={y=y-1,x=x+1}}, w)
		
I_ALWAYS_JUMP_SW :: Instruction
I_ALWAYS_JUMP_SW = \ _ st=:{pos={x,y}} w = ({st&pos={y=y+1,x=x+1}}, w)
		
I_ALWAYS_JUMP_NW :: Instruction
I_ALWAYS_JUMP_NW = \ _ st=:{pos={x,y}} w = ({st&pos={y=y+1,x=x-1}}, w)
		
I_ALWAYS_JUMP_NE :: Instruction
I_ALWAYS_JUMP_NE = \ _ st=:{pos={x,y}} w = ({st&pos={y=y-1,x=x-1}}, w)
	
// direction swaps
I_MAYBE_MOVE_NORTH :: Instruction
I_MAYBE_MOVE_NORTH =: \ _ st w | toBool st.mem.arg = ({st&dir=North}, w) = (st, w)
			
I_MAYBE_MOVE_EAST :: Instruction
I_MAYBE_MOVE_EAST =: \ _ st w | toBool st.mem.arg = ({st&dir=East}, w) = (st, w)
			
I_MAYBE_MOVE_SOUTH :: Instruction
I_MAYBE_MOVE_SOUTH =: \ _ st w | toBool st.mem.arg = ({st&dir=South}, w) = (st, w)
			
I_MAYBE_MOVE_WEST :: Instruction
I_MAYBE_MOVE_WEST =: \ _ st w | toBool st.mem.arg = ({st&dir=West}, w) = (st, w)

I_MAYBE_MOVE_RANDOM :: Instruction
I_MAYBE_MOVE_RANDOM = op
where
	op _ st=:{mem, rng=[num:rng]} w
		| toBool mem.arg
			= ({st&dir=case num rem 4 of 0 = North; 1 = East; 2 = South; 3 = West,rng=rng}, w)
		| otherwise
			= (st, w)

I_ALWAYS_MOVE_NORTH :: Instruction
I_ALWAYS_MOVE_NORTH = \ _ st w = ({st&dir=North}, w)
		
I_ALWAYS_MOVE_EAST :: Instruction
I_ALWAYS_MOVE_EAST = \ _ st w = ({st&dir=East}, w)
		
I_ALWAYS_MOVE_SOUTH :: Instruction
I_ALWAYS_MOVE_SOUTH = \ _ st w = ({st&dir=South}, w)
		
I_ALWAYS_MOVE_WEST :: Instruction
I_ALWAYS_MOVE_WEST = \ _ st w = ({st&dir=West}, w)

I_ALWAYS_MOVE_RANDOM :: Instruction
I_ALWAYS_MOVE_RANDOM = op
where
	op _ st=:{rng=[num:rng]} w
		= ({st&dir=case num rem 4 of 0 = North; 1 = East; 2 = South; 3 = West,rng=rng}, w)
		
// reposition
I_REPOSITION :: Instruction
I_REPOSITION = abort "reposition not implemented"

// rotations
I_TURN_ANTICLOCKWISE :: Instruction
I_TURN_ANTICLOCKWISE = \ _ st w = ({st&dir=case st.dir of North=West;East=North;South=East;West=South}, w)
I_TURN_CLOCKWISE :: Instruction
I_TURN_CLOCKWISE = \ _ st w = ({st&dir=case st.dir of North=East;East=South;South=West;West=North}, w)

// io
I_WRITE_SHORT :: Instruction
I_WRITE_SHORT = op where
	op _ st=:{mem={arg,out}} w
		# (val, out) = app2 (mapMaybe (repr True), id) (pop out)
		| isNothing val
			# (val, arg) = app2 (maybe [] (repr True), id) (pop arg)
			# w = foldl (\w c = execIO (putStr {#c}) w) w (val ++ ['\n'])
			= ({st&mem.arg=arg}, w)
		| otherwise
			#w = foldl (\w c = execIO (putStr {#c}) w) w ((fromJust val) ++ ['\n'])
			= ({st&mem.out=out}, w)
	
	/*
		# val = case peek out of (Just val) = repr True val; _ = []
		# w = foldl (\w c = execIO (putStr {#c}) w) w val
		= (st, w)
	*/
		
I_WRITE_LONG :: Instruction
I_WRITE_LONG = op where
	op _ st=:{mem={arg,out}} w
		# (val, out) = app2 (mapMaybe disp, id) (pop out)
		| isNothing val
			# (val, arg) = app2 (maybe [] disp, id) (pop arg)
			# w = foldl (\w c = execIO (putStr {#toChar c}) w) w val
			= ({st&mem.arg=arg}, w)
		| otherwise
			#w = foldl (\w c = execIO (putStr {#toChar c}) w) w (fromJust val)
			= ({st&mem.out=out}, w)
	
	/*
		# val = case peek out of (Just val) = disp val; _ = []
		/*
		# w = foldl (\w c = execIO (putStr (let
				str :: UTF8 // required because of ambiguity
				str = fromUnicode [c]
			in toString str)) w) w val
		*/ // don't force UTF8, allow arbitrary char output
		# w = foldl (\w c = execIO (putStr {#toChar c}) w) w val
		= (st, w)
	*/
		
I_WRITE_FILE :: Instruction
I_WRITE_FILE = abort "file writing not implemented"
I_READ_SHORT :: Instruction
I_READ_SHORT = abort "short reading not implemented"
I_READ_LONG :: Instruction
I_READ_LONG = abort "long reading not implemented"
I_READ_FILE :: Instruction
I_READ_FILE = abort "file reading not implemented"
I_GET_TIME :: Instruction
I_GET_TIME = abort "time not implemented"
I_GET_ENV_VAR :: Instruction
I_GET_ENV_VAR = abort "environment var not implemented"
I_SYSTEM_COMMAND :: Instruction
I_SYSTEM_COMMAND = abort "system command not implemented"
I_BEEP :: Instruction
I_BEEP = abort "beep not implemented"
I_DELETE_FILE :: Instruction
I_DELETE_FILE = abort "delete file not implemented"
I_CLEAR_CONSOLE :: Instruction
I_CLEAR_CONSOLE = abort "clear console not implemented"

// memory stuff
I_STORE_LEFT :: Instruction
I_STORE_LEFT = \_ st=:{mem={arg}} w = ({st&mem.tmp=peek arg,mem.arg=snd(pop arg)}, w)
I_RECALL_LEFT :: Instruction
I_RECALL_LEFT = \_ st=:{mem={arg,tmp}} w =({st&mem.arg=maybePrepend tmp arg}, w)
I_STORE_RIGHT :: Instruction
I_STORE_RIGHT = \_ st=:{mem={out}} w = ({st&mem.tmp=peek out,mem.out=snd(pop out)}, w)
I_RECALL_RIGHT :: Instruction
I_RECALL_RIGHT = \_ st=:{mem={out,tmp}} w = ({st&mem.out=maybePrepend tmp out}, w)
I_DUPLICATE_TOP :: Instruction
I_DUPLICATE_TOP = \_ st=:{mem={arg}} w = ({st&mem.arg=maybePrepend (peek arg) arg}, w)
I_TOP_RIGHT_TO_LEFT :: Instruction
I_TOP_RIGHT_TO_LEFT
	= \_ st=:{mem={arg,out}} w = let
		(h, t) = pop out
	in ({st&mem.arg=maybePrepend h arg,mem.out=t}, w)
I_TOP_LEFT_TO_RIGHT :: Instruction
I_TOP_LEFT_TO_RIGHT
	= \_ st=:{mem={arg,out}} w = let 
		(h, t) = pop arg
	in ({st&mem.out=maybePrepend h out,mem.arg=t}, w)
I_SWAP_STACK_TOPS :: Instruction
I_SWAP_STACK_TOPS
	= \_ st=:{mem={arg,out}} w = let
		(lh, lt) = pop arg
		(rh, rt) = pop out
	in ({st&mem.arg=maybePrepend rh lt,mem.out=maybePrepend lh rt}, w)
	
I_PREPEND_RIGHT_TO_LEFT :: Instruction
I_PREPEND_RIGHT_TO_LEFT = \_ st=:{mem={arg,out}} w = ({st&mem.arg=out+++arg,mem.out=zero}, w)
I_PREPEND_LEFT_TO_RIGHT :: Instruction
I_PREPEND_LEFT_TO_RIGHT = \_ st=:{mem={arg,out}} w = ({st&mem.out=arg+++out,mem.arg=zero}, w)
I_SWAP_FULL_STACKS :: Instruction
I_SWAP_FULL_STACKS = \_ st=:{mem={arg,out}} w = ({st&mem.arg=out,mem.out=arg}, w)
I_WIPE_ARG :: Instruction
I_WIPE_ARG = \_ st w = ({st&mem.arg=zero}, w)
I_POP_ARG :: Instruction
I_POP_ARG = \_ st w = ({st&mem.arg=snd (pop st.mem.arg)}, w)
I_WIPE_OUT :: Instruction
I_WIPE_OUT = \_ st w = ({st&mem.out=zero}, w)
I_POP_OUT :: Instruction
I_POP_OUT = \_ st w = ({st&mem.out=snd (pop st.mem.out)}, w)
I_SWAP_ARG_TOP :: Instruction
I_SWAP_ARG_TOP
	= \_ st=:{mem={arg}} w
		= ({st&mem.arg=case pop2 arg of (Nothing, _) = arg; (Just (a, b), arg) = push b (push a arg)}, w)
I_ENLIST_FULL_ARG :: Instruction
I_ENLIST_FULL_ARG = \_ st=:{mem={arg}} w = ({st&mem.arg=toStack (toValue arg)}, w)
I_EXPLODE_TOP_ARG :: Instruction
I_EXPLODE_TOP_ARG
	= \_ st=:{mem={arg}} w
		= ({st&mem.arg=case pop arg of (Just (Stk a), arg) = a +++ arg; _ = arg}, w) 

// no-op
I_NO_OP :: Instruction
I_NO_OP = \ _ st w = (st, w)

// number stuff
I_ADD :: Instruction
I_ADD = vecBinary (vectorizeFull (+))//\ _ st w = ({st&mem=maybeAppBinary (vectorizeFull (+)) st.mem}, w)
I_SUBTRACT :: Instruction
I_SUBTRACT = vecBinary (vectorizeFull (-))//abort "I_SUBTRACT  not implemented"
I_DIVIDE :: Instruction
I_DIVIDE = vecBinary (vectorizeFull (/))
I_MULTIPLY :: Instruction
I_MULTIPLY = vecBinary (vectorizeFull (*))
I_RECIPROCAL :: Instruction
I_RECIPROCAL = vecUnary ((/)one) //abort "I_RECIPROCAL  not implemented"
I_SQUARE_ROOT :: Instruction
I_SQUARE_ROOT = vecUnary sqrt//abort "I_SQUARE_ROOT  not implemented"
I_MODULUS :: Instruction
I_MODULUS = vecBinary (vectorizeFull (mod)) //abort "I_MODULUS  not implemented"
I_NEGATE :: Instruction
I_NEGATE = vecUnary (~)//abort "I_NEGATE  not implemented"
I_OR :: Instruction
I_OR = vecBinary (vectorizeFull bitOR)
I_AND :: Instruction
I_AND = vecBinary (vectorizeFull bitAND)
I_XOR :: Instruction
I_XOR = vecBinary (vectorizeFull bitXOR)
I_NOT :: Instruction
I_NOT = vecUnary bitNOT
I_SHIFT_LEFT :: Instruction
I_SHIFT_LEFT = vecBinary (vectorizeFull bitLEFT)
I_SHIFT_RIGHT :: Instruction
I_SHIFT_RIGHT = vecBinary (vectorizeFull bitRIGHT)
I_ARC_SINE :: Instruction
I_ARC_SINE = vecUnary asin
I_SINE :: Instruction
I_SINE = vecUnary sin
I_ARC_COSINE :: Instruction
I_ARC_COSINE = vecUnary acos
I_COSINE :: Instruction
I_COSINE = vecUnary cos
I_ARC_TANGENT :: Instruction
I_ARC_TANGENT = vecUnary atan
I_TANGENT :: Instruction
I_TANGENT = vecUnary tan
I_TO_BINARY :: Instruction
I_TO_BINARY = abort "I_TO_BINARY  not implemented"
I_FROM_BINARY :: Instruction
I_FROM_BINARY = abort "I_FROM_BINARY  not implemented"
I_ABSOLUTE :: Instruction
I_ABSOLUTE = vecUnary abs
I_PRIMES :: Instruction
I_PRIMES = abort "I_PRIMES  not implemented"
I_IS_PRIME :: Instruction
I_IS_PRIME = abort "I_IS_PRIME  not implemented"
I_RANDOM :: Instruction
I_RANDOM = \_ st=:{rng=[num:rng],mem={arg}} w = ({st&rng=rng,mem.arg=push (toValue num) arg}, w)
I_LOGARITHM :: Instruction
I_LOGARITHM = vecUnary ln //abort "I_LOGARITHM  not implemented"
I_EXPONENTIATE :: Instruction
I_EXPONENTIATE = vecBinary (vectorizeFull (^))
I_ROUND :: Instruction
I_ROUND = vecUnary numRound
I_IS_INTEGER :: Instruction
I_IS_INTEGER = abort "I_IS_INTEGER  not implemented"
I_IS_NUMBER :: Instruction
I_IS_NUMBER = abort "I_IS_NUMBER  not implemented"
I_IS_RATIONAL :: Instruction
I_IS_RATIONAL = abort "I_IS_RATIONAL  not implemented"
I_REAL_PART :: Instruction
I_REAL_PART = abort "I_REAL_PART  not implemented"
I_IMAGINARY_PART :: Instruction
I_IMAGINARY_PART = abort "I_IMAGINARY_PART  not implemented"
I_JOIN_COMPLEX :: Instruction
I_JOIN_COMPLEX = abort "I_JOIN_COMPLEX  not implemented"
I_SPLIT_COMPLEX :: Instruction
I_SPLIT_COMPLEX = abort "I_SPLIT_COMPLEX  not implemented"
I_CONJUGATE :: Instruction
I_CONJUGATE = abort "I_CONJUGATE  not implemented"


// list stuff
I_SUM :: Instruction
I_SUM = appUnary (appS (S_reduce (vectorizeFull (+)) (toValue 0)))
I_PRODUCT :: Instruction
I_PRODUCT = appUnary (appS (S_reduce (vectorizeFull (*)) (toValue 0)))
I_CROSS_PRODUCT :: Instruction
I_CROSS_PRODUCT = abort "I_CROSS_PRODUCT  not implemented"
I_DOT_PRODUCT :: Instruction
I_DOT_PRODUCT = abort "I_DOT_PRODUCT  not implemented"
I_DIAGONALIZE :: Instruction
I_DIAGONALIZE = abort "I_DIAGONALIZE  not implemented"
I_UNIFORM_COLLAPSE :: Instruction
I_UNIFORM_COLLAPSE = abort "I_UNIFORM_COLLAPSE  not implemented"
I_UNIFORM_EXPAND :: Instruction
I_UNIFORM_EXPAND = abort "I_UNIFORM_EXPAND  not implemented"
I_REPEAT :: Instruction
I_REPEAT = \_ st=:{mem={arg,out}} w = ({st&mem.arg=appH (maybe id (vectorizeLeft (S_repeat o toInt)) (peek out)) arg}, w)
I_COUNT_REPEATS :: Instruction
I_COUNT_REPEATS = abort "I_COUNT_REPEATS  not implemented"
I_PAIR :: Instruction
I_PAIR = abort "I_PAIR  not implemented"
I_SPLIT :: Instruction
I_SPLIT = abort "I_SPLIT  not implemented"
I_JOIN :: Instruction
I_JOIN = abort "I_JOIN  not implemented"
I_TAKE :: Instruction
I_TAKE = abort "I_TAKE  not implemented"
I_TAKE_WHILE :: Instruction
I_TAKE_WHILE
	= \_ st=:{mem={arg,out}} w = let
		takeWhileNum num stk = if(toBool num) stk zero
	in ({st&mem.arg=appH (appS ((maybe id (appV takeWhileNum S_takeWhile)) (peek out))) arg}, w)
I_DROP :: Instruction
I_DROP = abort "I_DROP  not implemented"
I_DROP_WHILE :: Instruction
I_DROP_WHILE
	= \_ st=:{mem={arg,out}} w = let
		dropWhileNum num stk = if(toBool num) stk zero
	in ({st&mem.arg=appH (appS ((maybe id (appV dropWhileNum S_dropWhile)) (peek out))) arg}, w)
I_SORT :: Instruction
I_SORT = appUnary (appS S_sort)
I_IS_SORTED :: Instruction
I_IS_SORTED = appUnary (toValue o (appV (const True) S_isSorted))
I_MINIMUM :: Instruction
I_MINIMUM = appUnary (appS \stk = case pop stk of (Nothing, _) = toValue stk; (Just h, t) = S_reduce min h t)//abort "I_MINIMUM  not implemented"
I_MAXIMUM :: Instruction
I_MAXIMUM = appUnary (appS \stk = case pop stk of (Nothing, _) = toValue stk; (Just h, t) = S_reduce max h t)//abort "I_MAXIMUM  not implemented"
I_LENGTH :: Instruction
I_LENGTH = appUnary (appS S_length)
I_IS_LIST :: Instruction
I_IS_LIST = appUnary (toValue o isStack)
I_REMOVE_DUPLICATES :: Instruction
I_REMOVE_DUPLICATES = appUnary (appS S_removeDup) // TODO: make this put *just* the duplicates in the out stack
I_HAS_DUPLICATES :: Instruction
I_HAS_DUPLICATES = appUnary (appS S_hasDup)
I_HEAD :: Instruction
I_HEAD = op
where
	op _ st=:{mem={arg}} w
		= case pop arg of
			(Just (Stk h), t) = ({st&mem.arg=maybe arg (\e = push e t) (peek h)}, w)
			_ = (st, w)
			
I_TAIL :: Instruction
I_TAIL = op
where
	op _ st=:{mem={arg}} w
		= case pop arg of
			(Just (Stk h), t) = ({st&mem.arg=push ((toValue o snd o pop) h) t}, w)
			_ = (st, w)
		
I_GROUP :: Instruction
I_GROUP = abort "I_GROUP  not implemented"
I_REVERSE :: Instruction
I_REVERSE = appUnary (appS S_reverse)
I_IS_PALINDROME :: Instruction
I_IS_PALINDROME = abort "I_IS_PALINDROME  not implemented"
I_FLATTEN :: Instruction
I_FLATTEN = abort "I_FLATTEN  not implemented"
I_CONCATENATE :: Instruction
I_CONCATENATE = op
where
	op _ st=:{mem={arg,out}} w
		= case (peek arg, peek out) of
			(Nothing, Nothing) = (st, w)
			(l, r) = let fn = maybe zero (appV toStack id)
			in ({st&mem.out=push (toValue ((fn l) +++ (fn r))) out}, w)
I_FILTER :: Instruction
I_FILTER = abort "I_FILTER  not implemented"



// string stuff
I_TO_STRING :: Instruction
I_TO_STRING = abort "I_TO_STRING  not implemented"
I_REGEX :: Instruction
I_REGEX = abort "I_REGEX  not implemented"

// other stuff
I_EQUAL_TO :: Instruction
I_EQUAL_TO = vecBinary (vectorizeFull (==))//abort "I_EQUAL_TO  not implemented"
I_LESS_THAN :: Instruction
I_LESS_THAN = vecBinary (vectorizeFull (<))//abort "I_LESS_THAN  not implemented"
I_MORE_THAN :: Instruction
I_MORE_THAN = vecBinary (vectorizeFull (>)) //abort "I_MORE_THAN  not implemented"
I_HASH :: Instruction
I_HASH = abort "I_HASH  not implemented"
I_EVAL :: Instruction
I_EVAL = abort "I_EVAL  not implemented"

