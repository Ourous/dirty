definition module Dirty.Frontend.Parser

// this module has no idea what the instructions do, or about runtime flags

from Dirty.Frontend.Arguments import ::Flags
from Dirty.Runtime.Instruction import ::Instruction
from Dirty.Runtime import ::State
from Data.Matrix import ::Matrix, ::Vector
from Dirty.Types import ::Point

parseFile :: (Matrix Char) -> (Matrix Instruction, Vector Point)