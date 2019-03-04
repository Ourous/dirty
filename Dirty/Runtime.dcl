definition module Dirty.Runtime

from Dirty.Runtime.Instruction import ::Instruction
from Dirty.Backend.Stack import ::Stack
from Dirty.Backend.Value import ::Value
from Dirty.Types import ::Point, ::Direction, ::Region
from Data.Maybe import ::Maybe
from System.IO import ::IO

:: Memory = {
	lhs :: Stack,
	rhs :: Stack,
	tmp :: Value
	}

:: State = {
	end :: Bool,
	ini :: Maybe Point,
	mem :: Memory,
	dir :: Direction,
	pos :: Point,
	reg :: Region,
	rng :: [Int]
	}
	
//evaluate :: (Matrix Instruction) Stack -> IO ()