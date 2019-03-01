implementation module Dirty.Runtime.Instruction

import Dirty.Backend.Stack, Dirty.Backend.Value
from Dirty.Runtime import ::Memory(..), ::State(..)
from Dirty.Frontend.Arguments import ::RuntimeFlags(..)
from Dirty.Types import ::Point(..), ::Region(..), ::Direction(..)
//import Dirty.Types
import Data.Matrix, Data.Func, Data.Maybe
import StdEnv, StdDebug

:: Instruction :== RuntimeFlags -> Operation

NO_FLAGS op :== \_ = op

finalizeProgram :: RuntimeFlags (Matrix Instruction) -> Matrix Operation
finalizeProgram f m = hyperstrict {{i f \\ i <-: j} \\ j <-: m}

// tab swapping
I_HORIZONTAL_TAB_SWAP :: (Vector Point) -> Instruction
I_HORIZONTAL_TAB_SWAP vec = NO_FLAGS op
where
	op st=:{rng=[num:rng]} w
		= ({st&pos=vec.[(abs num)rem(size vec)],rng=rng}, w)
	
I_VERTICAL_TAB_SWAP :: (Vector Point) -> Instruction
I_VERTICAL_TAB_SWAP vec = NO_FLAGS op
where
	op st=:{rng=[num:rng]} w
		= ({st&pos=vec.[(abs num)rem(size vec)],rng=rng}, w)

// conditional gotos
I_MAYBE_GOTO_TOP :: Point -> Instruction
I_MAYBE_GOTO_TOP pos = NO_FLAGS undef
	
I_MAYBE_GOTO_RIGHT :: Point -> Instruction
I_MAYBE_GOTO_RIGHT pos = NO_FLAGS undef

I_MAYBE_GOTO_BOTTOM :: Point -> Instruction
I_MAYBE_GOTO_BOTTOM pos = NO_FLAGS undef

I_MAYBE_GOTO_LEFT :: Point -> Instruction
I_MAYBE_GOTO_LEFT pos = NO_FLAGS undef

// constant gotos
I_ALWAYS_GOTO_TOP :: Point -> Instruction
I_ALWAYS_GOTO_TOP pos = NO_FLAGS op
where
	op st=:{dir=North} w
		= ({st&pos=pos}, w)
	op st w
		= (st, w)

I_ALWAYS_GOTO_RIGHT :: Point -> Instruction
I_ALWAYS_GOTO_RIGHT pos = NO_FLAGS op
where
	op st=:{dir=East} w
		= ({st&pos=pos}, w)
	op st w
		= (st, w)

I_ALWAYS_GOTO_BOTTOM :: Point -> Instruction
I_ALWAYS_GOTO_BOTTOM pos = NO_FLAGS op
where
	op st=:{dir=South} w
		= ({st&pos=pos}, w)
	op st w
		= (st, w)

I_ALWAYS_GOTO_LEFT :: Point -> Instruction
I_ALWAYS_GOTO_LEFT pos = NO_FLAGS op
where
	op st=:{dir=West} w
		= ({st&pos=pos}, w)
	op st w
		= (st, w)

// conditional loops
I_MAYBE_LOOP_TOP :: Point -> Instruction
I_MAYBE_LOOP_TOP pos = NO_FLAGS undef
	
I_MAYBE_LOOP_RIGHT :: Point -> Instruction
I_MAYBE_LOOP_RIGHT pos = NO_FLAGS undef

I_MAYBE_LOOP_BOTTOM :: Point -> Instruction
I_MAYBE_LOOP_BOTTOM pos = NO_FLAGS undef

I_MAYBE_LOOP_LEFT :: Point -> Instruction
I_MAYBE_LOOP_LEFT pos = NO_FLAGS undef

// constant loops
I_ALWAYS_LOOP_TOP :: Point -> Instruction
I_ALWAYS_LOOP_TOP pos = NO_FLAGS op
where
	op st=:{dir=North} w
		= ({st&pos=pos}, w)
	op st w
		= (st, w)

I_ALWAYS_LOOP_RIGHT :: Point -> Instruction
I_ALWAYS_LOOP_RIGHT pos = NO_FLAGS op
where
	op st=:{dir=East} w
		= ({st&pos=pos}, w)
	op st w
		= (st, w)

I_ALWAYS_LOOP_BOTTOM :: Point -> Instruction
I_ALWAYS_LOOP_BOTTOM pos = NO_FLAGS op
where
	op st=:{dir=South} w
		= ({st&pos=pos}, w)
	op st w
		= (st, w)

I_ALWAYS_LOOP_LEFT :: Point -> Instruction
I_ALWAYS_LOOP_LEFT pos = NO_FLAGS op
where
	op st=:{dir=West} w
		= ({st&pos=pos}, w)
	op st w
		= (st, w)

// restarts
I_RESTART_RANDOM :: (Vector Point) -> Instruction
I_RESTART_RANDOM vec = NO_FLAGS op
where
	op st=:{rng=[num:rng]} w
		= ({st&ini=Nothing,pos=vec.[(abs num)rem(size vec)],rng=rng}, w)

I_RESTART_LAST :: Instruction
I_RESTART_LAST = op
where
	op {warnings} st=:{ini=Nothing} w
		= if(warnings) (trace_n "Warning: restart used without start location") id (st, w)

// terminate
I_TERMINATE :: Instruction
I_TERMINATE = undef