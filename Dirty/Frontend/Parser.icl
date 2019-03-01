implementation module Dirty.Frontend.Parser

import Dirty.Frontend.Arguments
import Dirty.Runtime.Instruction
from Dirty.Types import ::Point(..)
import Data.Matrix
import StdEnv

parse :: (Matrix Char) -> (Matrix Instruction, Vector Point)
parse m = ({{parse0 \\ c <-: r & x <- [0..]} \\ r <-: m & y <- [0..]}, undef)
where
	parse0 = undef