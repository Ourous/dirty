implementation module Dirty.Runtime

import Dirty.Backend.Stack, Dirty.Backend.Value
import Dirty.Frontend.Parser
import Dirty.Runtime.Instruction
import Dirty.Types
import System.IO

initialize :: RuntimeFlags (Matrix Instruction) Stack -> (*World -> *World)
initialize flags source lhs = IO runtime (finalizeProgram flags source)