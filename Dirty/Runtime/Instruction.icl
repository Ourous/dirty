implementation module Dirty.Runtime.Instruction

import Dirty.Backend.Stack, Dirty.Backend.Value
from Dirty.Runtime import ::Memory(..), ::State(..)
from Dirty.Frontend.Arguments import ::RuntimeFlags(..)
from Dirty.Types import ::Point(..), ::Region(..), ::Direction(..)
//import Dirty.Types
import Data.Matrix, Data.Func, Data.Maybe
import StdEnv, StdDebug

:: Instruction :== RuntimeFlags -> Operation

finalizeProgram :: RuntimeFlags (Matrix Instruction) -> Matrix Operation
finalizeProgram f m = hyperstrict {{i f \\ i <-: j} \\ j <-: m}

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

// tab swapping
I_HORIZONTAL_TAB_SWAP :: (Vector Point) -> Instruction
I_HORIZONTAL_TAB_SWAP vec = op
where
	op _ st=:{rng=[num:rng]} w
		= ({st&pos=vec.[num rem(size vec)],rng=rng}, w)
	
I_VERTICAL_TAB_SWAP :: (Vector Point) -> Instruction
I_VERTICAL_TAB_SWAP vec = op
where
	op _ st=:{rng=[num:rng]} w
		= ({st&pos=vec.[num rem(size vec)],rng=rng}, w)

// conditional gotos
I_MAYBE_GOTO_NORTH :: Point -> Instruction
I_MAYBE_GOTO_NORTH pos = const undef
	
I_MAYBE_GOTO_EAST :: Point -> Instruction
I_MAYBE_GOTO_EAST pos = const undef

I_MAYBE_GOTO_SOUTH :: Point -> Instruction
I_MAYBE_GOTO_SOUTH pos = const undef

I_MAYBE_GOTO_WEST :: Point -> Instruction
I_MAYBE_GOTO_WEST pos = const undef

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
I_MAYBE_LOOP_NORTH pos = const undef
	
I_MAYBE_LOOP_EAST :: Point -> Instruction
I_MAYBE_LOOP_EAST pos = const undef

I_MAYBE_LOOP_SOUTH :: Point -> Instruction
I_MAYBE_LOOP_SOUTH pos = const undef

I_MAYBE_LOOP_WEST :: Point -> Instruction
I_MAYBE_LOOP_WEST pos = const undef

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
I_TERMINATE = op
where
	op _ st w
		= ({st&end=True}, w)

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