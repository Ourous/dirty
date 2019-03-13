implementation module Dirty.Runtime.Instruction

import Dirty.Backend
from Dirty.Runtime import ::Memory(..), ::State(..)
from Dirty.Frontend.Arguments import ::RuntimeFlags(..)
from Dirty.Types import ::Point(..), ::Region(..), ::Direction(..)
//import Dirty.Types
import Data.Matrix, Data.Func, Data.Maybe
import StdEnv, StdDebug

:: Instruction :== RuntimeFlags -> Operation

finalizeProgram :: RuntimeFlags (Matrix Instruction) -> Matrix Operation
finalizeProgram f m = hyperstrict {{i f \\ i <-: j} \\ j <-: m}

maybePrepend val :== case val of Nothing = id; (Just v) = push v
maybeAppH fn lhs rhs :== appH (maybe id fn (peek rhs)) lhs

// region literals
I_LITERAL_REGION :: (Value, Point) (Value, Point) (Value, Point) (Value, Point) -> Instruction
I_LITERAL_REGION (n_val, n_end) (e_val, e_end) (s_val, s_end) (w_val, w_end) = op
where
	op _ st=:{dir=North,mem} w
		= ({st&mem.lhs=push n_val mem.lhs,pos=n_end}, w)
	op _ st=:{dir=East,mem} w
		= ({st&mem.lhs=push e_val mem.lhs,pos=e_end}, w)
	op _ st=:{dir=South,mem} w
		= ({st&mem.lhs=push s_val mem.lhs,pos=s_end}, w)
	op _ st=:{dir=West,mem} w
		= ({st&mem.lhs=push w_val mem.lhs,pos=w_end}, w)

I_LITERAL_SINGLE :: Value -> Instruction
I_LITERAL_SINGLE val = \ _ st w = ({st&mem.lhs=push val st.mem.lhs}, w)

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
		| toBool st.mem.lhs
			= ({st&pos=pos}, w)
		| otherwise
			= (st, w)
	op _ st w
		= (st, w)
	
I_MAYBE_GOTO_EAST :: Point -> Instruction
I_MAYBE_GOTO_EAST pos = op
where
	op _ st=:{dir=East} w
		| toBool st.mem.lhs
			= ({st&pos=pos}, w)
		| otherwise
			= (st, w)
	op _ st w
		= (st, w)

I_MAYBE_GOTO_SOUTH :: Point -> Instruction
I_MAYBE_GOTO_SOUTH pos = op
where
	op _ st=:{dir=South} w
		| toBool st.mem.lhs
			= ({st&pos=pos}, w)
		| otherwise
			= (st, w)
	op _ st w
		= (st, w)

I_MAYBE_GOTO_WEST :: Point -> Instruction
I_MAYBE_GOTO_WEST pos = op
where
	op _ st=:{dir=West} w
		| toBool st.mem.lhs
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
		| toBool st.mem.lhs
			= ({st&pos=pos}, w)
		| otherwise
			= (st, w)
	op _ st w
		= (st, w)
	
I_MAYBE_LOOP_EAST :: Point -> Instruction
I_MAYBE_LOOP_EAST pos = op
where
	op _ st=:{dir=East} w
		| toBool st.mem.lhs
			= ({st&pos=pos}, w)
		| otherwise
			= (st, w)
	op _ st w
		= (st, w)

I_MAYBE_LOOP_SOUTH :: Point -> Instruction
I_MAYBE_LOOP_SOUTH pos = op
where
	op _ st=:{dir=South} w
		| toBool st.mem.lhs
			= ({st&pos=pos}, w)
		| otherwise
			= (st, w)
	op _ st w
		= (st, w)

I_MAYBE_LOOP_WEST :: Point -> Instruction
I_MAYBE_LOOP_WEST pos = op
where
	op _ st=:{dir=West} w
		| toBool st.mem.lhs
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
		| toBool mem.lhs
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
		| toBool mem.lhs
			= ({st&pos.y=pos.y+1,pos.x=pos.x+1}, w)
		| otherwise
			= (st, w)
			
I_MAYBE_JUMP_SW :: Instruction
I_MAYBE_JUMP_SW = op
where
	op _ st=:{mem,pos} w
		| toBool mem.lhs
			= ({st&pos.y=pos.y+1,pos.x=pos.x-1}, w)
		| otherwise
			= (st, w)
			
I_MAYBE_JUMP_NW :: Instruction
I_MAYBE_JUMP_NW = op
where
	op _ st=:{mem,pos} w
		| toBool mem.lhs
			= ({st&pos.y=pos.y-1,pos.x=pos.x-1}, w)
		| otherwise
			= (st, w)

I_MAYBE_JUMP_NE :: Instruction
I_MAYBE_JUMP_NE = op
where
	op _ st=:{mem,pos} w
		| toBool mem.lhs
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
I_MAYBE_MOVE_NORTH =: \ _ st w | toBool st.mem.lhs = ({st&dir=North}, w) = (st, w)
			
I_MAYBE_MOVE_EAST :: Instruction
I_MAYBE_MOVE_EAST =: \ _ st w | toBool st.mem.lhs = ({st&dir=East}, w) = (st, w)
			
I_MAYBE_MOVE_SOUTH :: Instruction
I_MAYBE_MOVE_SOUTH =: \ _ st w | toBool st.mem.lhs = ({st&dir=South}, w) = (st, w)
			
I_MAYBE_MOVE_WEST :: Instruction
I_MAYBE_MOVE_WEST =: \ _ st w | toBool st.mem.lhs = ({st&dir=West}, w) = (st, w)

I_MAYBE_MOVE_RANDOM :: Instruction
I_MAYBE_MOVE_RANDOM = op
where
	op _ st=:{mem, rng=[num:rng]} w
		| toBool mem.lhs
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
I_WRITE_SHORT = abort "short writing not implemented"
I_WRITE_LONG :: Instruction
I_WRITE_LONG = abort "long writing not implemented"
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
I_STORE_LEFT = \_ st=:{mem={lhs}} w = ({st&mem.tmp=peek lhs}, w)
I_RECALL_LEFT :: Instruction
I_RECALL_LEFT = \_ st=:{mem={lhs,tmp}} w =({st&mem.lhs=maybePrepend tmp lhs}, w)
I_STORE_RIGHT :: Instruction
I_STORE_RIGHT = \_ st=:{mem={rhs}} w = ({st&mem.tmp=peek rhs}, w)
I_RECALL_RIGHT :: Instruction
I_RECALL_RIGHT = \_ st=:{mem={rhs,tmp}} w = ({st&mem.rhs=maybePrepend tmp rhs}, w)
I_DUPLICATE_TOP :: Instruction
I_DUPLICATE_TOP = \_ st=:{mem={lhs}} w = ({st&mem.lhs=maybePrepend (peek lhs) lhs}, w)
I_TOP_RIGHT_TO_LEFT :: Instruction
I_TOP_RIGHT_TO_LEFT
	= \_ st=:{mem={lhs,rhs}} w = let
		(h, t) = pop rhs
	in ({st&mem.lhs=maybePrepend h lhs,mem.rhs=t}, w)
I_TOP_LEFT_TO_RIGHT :: Instruction
I_TOP_LEFT_TO_RIGHT
	= \_ st=:{mem={lhs,rhs}} w = let 
		(h, t) = pop lhs
	in ({st&mem.rhs=maybePrepend h rhs,mem.lhs=t}, w)
I_SWAP_STACK_TOPS :: Instruction
I_SWAP_STACK_TOPS
	= \_ st=:{mem={lhs,rhs}} w = let
		(lh, lt) = pop lhs
		(rh, rt) = pop rhs
	in ({st&mem.lhs=maybePrepend rh lt,mem.rhs=maybePrepend lh rt}, w)
	
I_PREPEND_RIGHT_TO_LEFT :: Instruction
I_PREPEND_RIGHT_TO_LEFT = \_ st=:{mem={lhs,rhs}} w = ({st&mem.lhs=rhs+++lhs,mem.rhs=zero}, w)
I_PREPEND_LEFT_TO_RIGHT :: Instruction
I_PREPEND_LEFT_TO_RIGHT = \_ st=:{mem={lhs,rhs}} w = ({st&mem.rhs=lhs+++rhs,mem.lhs=zero}, w)
I_SWAP_FULL_STACKS :: Instruction
I_SWAP_FULL_STACKS = \_ st=:{mem={lhs,rhs}} w = ({st&mem.lhs=rhs,mem.rhs=lhs}, w)

// no-op
I_NO_OP :: Instruction
I_NO_OP = \ _ st w = (st, w)

// number stuff
I_ADD :: Instruction
I_ADD = abort "I_ADD  not implemented"
I_SUBTRACT :: Instruction
I_SUBTRACT = abort "I_SUBTRACT  not implemented"
I_DIVIDE :: Instruction
I_DIVIDE = abort "I_DIVIDE  not implemented"
I_MULTIPLY :: Instruction
I_MULTIPLY = abort "I_MULTIPLY  not implemented"
I_RECIPROCAL :: Instruction
I_RECIPROCAL = abort "I_RECIPROCAL  not implemented"
I_SQUARE_ROOT :: Instruction
I_SQUARE_ROOT = abort "I_SQUARE_ROOT  not implemented"
I_MODULUS :: Instruction
I_MODULUS = abort "I_MODULUS  not implemented"
I_NEGATE :: Instruction
I_NEGATE = abort "I_NEGATE  not implemented"
I_OR :: Instruction
I_OR = abort "I_OR  not implemented"
I_AND :: Instruction
I_AND = abort "I_AND  not implemented"
I_XOR :: Instruction
I_XOR = abort "I_XOR  not implemented"
I_NOT :: Instruction
I_NOT = abort "I_NOT  not implemented"
I_SHIFT_LEFT :: Instruction
I_SHIFT_LEFT = abort "I_SHIFT_LEFT  not implemented"
I_SHIFT_RIGHT :: Instruction
I_SHIFT_RIGHT = abort "I_SHIFT_RIGHT  not implemented"
I_ARC_SINE :: Instruction
I_ARC_SINE = abort "I_ARC_SINE  not implemented"
I_SINE :: Instruction
I_SINE = abort "I_SINE  not implemented"
I_ARC_COSINE :: Instruction
I_ARC_COSINE = abort "I_ARC_COSINE  not implemented"
I_COSINE :: Instruction
I_COSINE = abort "I_COSINE  not implemented"
I_ARC_TANGENT :: Instruction
I_ARC_TANGENT = abort "I_ARC_TANGENT  not implemented"
I_TANGENT :: Instruction
I_TANGENT = abort "I_TANGENT  not implemented"
I_TO_BINARY :: Instruction
I_TO_BINARY = abort "I_TO_BINARY  not implemented"
I_FROM_BINARY :: Instruction
I_FROM_BINARY = abort "I_FROM_BINARY  not implemented"
I_ABSOLUTE :: Instruction
I_ABSOLUTE = abort "I_ABSOLUTE  not implemented"
I_PRIMES :: Instruction
I_PRIMES = abort "I_PRIMES  not implemented"
I_IS_PRIME :: Instruction
I_IS_PRIME = abort "I_IS_PRIME  not implemented"
I_RANDOM :: Instruction
I_RANDOM = abort "I_RANDOM  not implemented"
I_LOGARITHM :: Instruction
I_LOGARITHM = abort "I_LOGARITHM  not implemented"
I_EXPONENTIATE :: Instruction
I_EXPONENTIATE = abort "I_EXPONENTIATE  not implemented"
I_ROUND :: Instruction
I_ROUND = abort "I_ROUND  not implemented"
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
I_REPEAT = \_ st=:{mem={lhs,rhs}} w = ({st&mem.lhs=appH (maybe id (vectorizeLeft (S_repeat o toInt)) (peek rhs)) lhs}, w)
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
	= \_ st=:{mem={lhs,rhs}} w = let
		takeWhileNum num stk = if(toBool num) stk zero
	in ({st&mem.lhs=appH (appS ((maybe id (appV takeWhileNum S_takeWhile)) (peek rhs))) lhs}, w)
I_DROP :: Instruction
I_DROP = abort "I_DROP  not implemented"
I_DROP_WHILE :: Instruction
I_DROP_WHILE = abort "I_DROP_WHILE  not implemented"
I_SORT :: Instruction
I_SORT = \ _ st w = ({st&mem.lhs=appH (appS S_sort) st.mem.lhs}, w)
I_IS_SORTED :: Instruction
I_IS_SORTED = abort "I_IS_SORTED  not implemented"
I_MINIMUM :: Instruction
I_MINIMUM = abort "I_MINIMUM  not implemented"
I_MAXIMUM :: Instruction
I_MAXIMUM = abort "I_MAXIMUM  not implemented"
I_LENGTH :: Instruction
I_LENGTH = \ _ st w = ({st&mem.lhs=appH (appS S_length) st.mem.lhs}, w)
I_IS_LIST :: Instruction
I_IS_LIST = abort "I_IS_LIST  not implemented"
I_REMOVE_DUPLICATES :: Instruction
I_REMOVE_DUPLICATES = \_ st=:{mem={lhs}} w = ({st&mem.lhs=appH (appS S_removeDup) lhs}, w)
I_HAS_DUPLICATES :: Instruction
I_HAS_DUPLICATES = abort "I_HAS_DUPLICATES  not implemented"
I_HEAD :: Instruction
I_HEAD = op
where
	op _ st=:{mem={lhs}} w
		= case pop lhs of
			(Just (Stk h), t) = ({st&mem.lhs=maybe lhs (\e = push e t) (peek h)}, w)
			_ = (st, w)
			
I_TAIL :: Instruction
I_TAIL = op
where
	op _ st=:{mem={lhs}} w
		= case pop lhs of
			(Just (Stk h), t) = ({st&mem.lhs=push ((toValue o snd o pop) h) t}, w)
			_ = (st, w)
		
I_GROUP :: Instruction
I_GROUP = abort "I_GROUP  not implemented"
I_REVERSE :: Instruction
I_REVERSE = abort "I_REVERSE  not implemented"
I_IS_PALINDROME :: Instruction
I_IS_PALINDROME = abort "I_IS_PALINDROME  not implemented"
I_FLATTEN :: Instruction
I_FLATTEN = abort "I_FLATTEN  not implemented"
I_UNFLATTEN :: Instruction
I_UNFLATTEN = abort "I_UNFLATTEN  not implemented"
I_CONCATENATE :: Instruction
I_CONCATENATE = op
where
	op _ st=:{mem={lhs,rhs}} w
		= case (peek lhs, peek rhs) of
			(Nothing, Nothing) = (st, w)
			(l, r) = let fn = maybe zero (appV toStack id)
			in ({st&mem.rhs=push (toValue ((fn l) +++ (fn r))) rhs}, w)
I_FILTER :: Instruction
I_FILTER = abort "I_FILTER  not implemented"



// string stuff
I_TO_STRING :: Instruction
I_TO_STRING = abort "I_TO_STRING  not implemented"
I_REGEX :: Instruction
I_REGEX = abort "I_REGEX  not implemented"

// other stuff
I_EQUAL_TO :: Instruction
I_EQUAL_TO = abort "I_EQUAL_TO  not implemented"
I_LESS_THAN :: Instruction
I_LESS_THAN = abort "I_LESS_THAN  not implemented"
I_MORE_THAN :: Instruction
I_MORE_THAN = abort "I_MORE_THAN  not implemented"
I_HASH :: Instruction
I_HASH = abort "I_HASH  not implemented"
I_EVAL :: Instruction
I_EVAL = abort "I_EVAL  not implemented"

