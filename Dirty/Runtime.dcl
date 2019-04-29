definition module Dirty.Runtime

from Dirty.Runtime.Instruction import ::Instruction, ::Operation
from Dirty.Backend.Stack import ::Stack
from Dirty.Backend.Value import ::Value
from Dirty.Frontend.Arguments import ::RuntimeFlags
from Dirty.Types import ::Point, ::Direction, ::Region
from Data.Maybe import ::Maybe
from Data.Matrix import ::Matrix, ::Vector
from System.IO import ::IO

:: Memory = {
	arg :: Stack,
	out :: Stack,
	tmp :: Maybe Value
	}

:: State = {
	end :: Bool,
	ini :: Maybe Point,
	mem :: Memory,
	dir :: Direction,
	pos :: Point,
	rng :: [Int]
	}
	
initialize :: RuntimeFlags (Matrix Instruction) (Vector Point) Stack -> Int -> *World -> *World 
