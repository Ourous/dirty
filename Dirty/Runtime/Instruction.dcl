definition module Dirty.Runtime.Instruction

// this module has no idea what characters map to the instructions

from Dirty.Runtime import ::Memory, ::State
from Dirty.Frontend.Arguments import ::RuntimeFlags
from Data.Matrix import ::Matrix, ::Vector
from Dirty.Types import ::Point

:: Operation :== State *World -> *(State, *World)

:: Instruction (:== RuntimeFlags -> Operation)

// applies runtime behaviour changes to the program
finalizeProgram :: RuntimeFlags (Matrix Instruction) -> Matrix Operation

// the rest is really ugly

// complex instructions

// tab swapping
I_HORIZONTAL_TAB_SWAP :: (Vector Point) -> Instruction
I_VERTICAL_TAB_SWAP :: (Vector Point) -> Instruction

// conditional gotos
I_MAYBE_GOTO_TOP :: Point -> Instruction
I_MAYBE_GOTO_RIGHT :: Point -> Instruction
I_MAYBE_GOTO_BOTTOM :: Point -> Instruction
I_MAYBE_GOTO_LEFT :: Point -> Instruction

// constant gotos
I_ALWAYS_GOTO_TOP :: Point -> Instruction
I_ALWAYS_GOTO_RIGHT :: Point -> Instruction
I_ALWAYS_GOTO_BOTTOM :: Point -> Instruction
I_ALWAYS_GOTO_LEFT :: Point -> Instruction

// conditional loops
I_MAYBE_LOOP_TOP :: Point -> Instruction
I_MAYBE_LOOP_RIGHT :: Point -> Instruction
I_MAYBE_LOOP_BOTTOM :: Point -> Instruction
I_MAYBE_LOOP_LEFT :: Point -> Instruction

// constant loops
I_ALWAYS_LOOP_TOP :: Point -> Instruction
I_ALWAYS_LOOP_RIGHT :: Point -> Instruction
I_ALWAYS_LOOP_BOTTOM :: Point -> Instruction
I_ALWAYS_LOOP_LEFT :: Point -> Instruction

// restarts
I_RESTART_RANDOM :: (Vector Point) -> Instruction
I_RESTART_LAST :: Instruction

// simple instructions

// terminate
I_TERMINATE :: Instruction