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
		= ({st&mem.lhs=prepend n_val mem.lhs,pos=n_end}, w)
	op _ st=:{dir=East,mem} w
		= ({st&mem.lhs=prepend e_val mem.lhs,pos=e_end}, w)
	op _ st=:{dir=South,mem} w
		= ({st&mem.lhs=prepend s_val mem.lhs,pos=s_end}, w)
	op _ st=:{dir=West,mem} w
		= ({st&mem.lhs=prepend w_val mem.lhs,pos=w_end}, w)

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
I_TERMINATE = undef
	
	
	
	
	
	
	
	
	
	
	
	