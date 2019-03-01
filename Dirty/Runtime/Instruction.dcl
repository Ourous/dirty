definition module Dirty.Runtime.Instruction

// this module has no idea what characters map to the instructions

from Dirty.Backend.Value import ::Value
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

// region literals
I_LITERAL_REGION :: (Value, Point) (Value, Point) (Value, Point) (Value, Point) -> Instruction

// tab swapping
I_HORIZONTAL_TAB_SWAP :: (Vector Point) -> Instruction
I_VERTICAL_TAB_SWAP :: (Vector Point) -> Instruction

// conditional gotos
I_MAYBE_GOTO_NORTH :: Point -> Instruction
I_MAYBE_GOTO_EAST :: Point -> Instruction
I_MAYBE_GOTO_SOUTH :: Point -> Instruction
I_MAYBE_GOTO_WEST :: Point -> Instruction

// constant gotos
I_ALWAYS_GOTO_NORTH :: Point -> Instruction
I_ALWAYS_GOTO_EAST :: Point -> Instruction
I_ALWAYS_GOTO_SOUTH :: Point -> Instruction
I_ALWAYS_GOTO_WEST :: Point -> Instruction

// conditional loops
I_MAYBE_LOOP_NORTH :: Point -> Instruction
I_MAYBE_LOOP_EAST :: Point -> Instruction
I_MAYBE_LOOP_SOUTH :: Point -> Instruction
I_MAYBE_LOOP_WEST :: Point -> Instruction

// constant loops
I_ALWAYS_LOOP_NORTH :: Point -> Instruction
I_ALWAYS_LOOP_EAST :: Point -> Instruction
I_ALWAYS_LOOP_SOUTH :: Point -> Instruction
I_ALWAYS_LOOP_WEST :: Point -> Instruction

// start
I_START_NORTH :: Point -> Instruction
I_START_EAST :: Point -> Instruction
I_START_SOUTH :: Point -> Instruction
I_START_WEST :: Point -> Instruction
I_START_RANDOM :: Point -> Instruction

// restarts
I_RESTART_RANDOM :: (Vector Point) -> Instruction
I_RESTART_LAST :: Instruction

// simple instructions

// terminate
I_TERMINATE :: Instruction

