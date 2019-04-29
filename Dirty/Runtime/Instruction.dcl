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
I_LITERAL_SINGLE :: Value -> Instruction

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

// reflections
I_REFLECT_VERTICAL :: Instruction
I_REFLECT_HORIZONTAL :: Instruction
I_REFLECT_IDENTITY :: Instruction
I_REFLECT_INVERSE :: Instruction

// skips
I_MAYBE_SKIP_NEXT :: Instruction
I_ALWAYS_SKIP_NEXT :: Instruction

// jumps
I_MAYBE_JUMP_SE :: Instruction
I_MAYBE_JUMP_SW :: Instruction
I_MAYBE_JUMP_NW :: Instruction
I_MAYBE_JUMP_NE :: Instruction

I_ALWAYS_JUMP_SE :: Instruction
I_ALWAYS_JUMP_SW :: Instruction
I_ALWAYS_JUMP_NW :: Instruction
I_ALWAYS_JUMP_NE :: Instruction

// direction swaps
I_MAYBE_MOVE_NORTH :: Instruction
I_MAYBE_MOVE_EAST :: Instruction
I_MAYBE_MOVE_SOUTH :: Instruction
I_MAYBE_MOVE_WEST :: Instruction
I_MAYBE_MOVE_RANDOM :: Instruction

I_ALWAYS_MOVE_NORTH :: Instruction
I_ALWAYS_MOVE_EAST :: Instruction
I_ALWAYS_MOVE_SOUTH :: Instruction
I_ALWAYS_MOVE_WEST :: Instruction
I_ALWAYS_MOVE_RANDOM :: Instruction

// reposition
I_REPOSITION :: Instruction

// rotations
I_TURN_ANTICLOCKWISE :: Instruction
I_TURN_CLOCKWISE :: Instruction

// io
I_WRITE_SHORT :: Instruction
I_WRITE_LONG :: Instruction
I_WRITE_FILE :: Instruction
I_READ_SHORT :: Instruction
I_READ_LONG :: Instruction
I_READ_FILE :: Instruction
I_DELETE_FILE :: Instruction
I_GET_TIME :: Instruction
I_GET_ENV_VAR :: Instruction
I_SYSTEM_COMMAND :: Instruction
I_BEEP :: Instruction
I_CLEAR_CONSOLE :: Instruction

// memory stuff
I_STORE_LEFT :: Instruction
I_RECALL_LEFT :: Instruction
I_STORE_RIGHT :: Instruction
I_RECALL_RIGHT :: Instruction
I_DUPLICATE_TOP :: Instruction
I_TOP_RIGHT_TO_LEFT :: Instruction
I_TOP_LEFT_TO_RIGHT :: Instruction
I_SWAP_STACK_TOPS :: Instruction
I_PREPEND_RIGHT_TO_LEFT :: Instruction
I_PREPEND_LEFT_TO_RIGHT :: Instruction
I_SWAP_FULL_STACKS :: Instruction
I_WIPE_ARG :: Instruction
I_POP_ARG :: Instruction
I_WIPE_OUT :: Instruction
I_POP_OUT :: Instruction
I_SWAP_ARG_TOP :: Instruction
I_ENLIST_FULL_ARG :: Instruction
I_ENLIST_TOP_ARG :: Instruction
I_EXPLODE_TOP_ARG :: Instruction

// no-op
I_NO_OP :: Instruction

// number stuff
I_ADD :: Instruction
I_SUBTRACT :: Instruction
I_DIVIDE :: Instruction
I_MULTIPLY :: Instruction
I_RECIPROCAL :: Instruction
I_SQUARE_ROOT :: Instruction
I_MODULUS :: Instruction
I_NEGATE :: Instruction
I_OR :: Instruction
I_AND :: Instruction
I_XOR :: Instruction
I_NOT :: Instruction
I_SHIFT_LEFT :: Instruction
I_SHIFT_RIGHT :: Instruction
I_ARC_SINE :: Instruction
I_SINE :: Instruction
I_ARC_COSINE :: Instruction
I_COSINE :: Instruction
I_ARC_TANGENT :: Instruction
I_TANGENT :: Instruction
I_TO_BINARY :: Instruction
I_FROM_BINARY :: Instruction
I_ABSOLUTE :: Instruction
I_PRIMES :: Instruction
I_IS_PRIME :: Instruction
I_RANDOM :: Instruction
I_LOGARITHM :: Instruction
I_EXPONENTIATE :: Instruction
I_ROUND :: Instruction
I_IS_INTEGER :: Instruction
I_IS_NUMBER :: Instruction
I_IS_RATIONAL :: Instruction
I_REAL_PART :: Instruction
I_IMAGINARY_PART :: Instruction
I_JOIN_COMPLEX :: Instruction
I_SPLIT_COMPLEX :: Instruction
I_CONJUGATE :: Instruction


// list stuff
I_SUM :: Instruction
I_PRODUCT :: Instruction
I_CROSS_PRODUCT :: Instruction
I_DOT_PRODUCT :: Instruction
I_DIAGONALIZE :: Instruction
I_UNIFORM_COLLAPSE :: Instruction
I_UNIFORM_EXPAND :: Instruction
I_REPEAT :: Instruction
I_COUNT_REPEATS :: Instruction
I_PAIR :: Instruction
I_SPLIT :: Instruction
I_JOIN :: Instruction
I_TAKE :: Instruction
I_TAKE_WHILE :: Instruction
I_DROP :: Instruction
I_DROP_WHILE :: Instruction
I_SORT :: Instruction
I_IS_SORTED :: Instruction
I_MINIMUM :: Instruction
I_MAXIMUM :: Instruction
I_LENGTH :: Instruction
I_IS_LIST :: Instruction
I_REMOVE_DUPLICATES :: Instruction
I_HAS_DUPLICATES :: Instruction
I_HEAD :: Instruction
I_TAIL :: Instruction
I_GROUP :: Instruction
I_REVERSE :: Instruction
I_IS_PALINDROME :: Instruction
I_FLATTEN :: Instruction
I_UNFLATTEN :: Instruction
I_CONCATENATE :: Instruction
I_FILTER :: Instruction



// string stuff
I_TO_STRING :: Instruction
I_REGEX :: Instruction

// other stuff
I_EQUAL_TO :: Instruction
I_LESS_THAN :: Instruction
I_MORE_THAN :: Instruction
I_HASH :: Instruction
I_EVAL :: Instruction
