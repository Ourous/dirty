implementation module Dirty.Runtime

import Dirty.Backend.Stack, Dirty.Backend.Value
import Dirty.Frontend
import Dirty.Runtime.Instruction
import Dirty.Types
import Math.Random
import System.IO
import StdEnv, StdDebug, Text
import Data.Maybe, Data.Matrix

instance toString Direction where
	toString North = "North"
	toString South = "South"
	toString East = "East"
	toString West = "West"

initialize :: RuntimeFlags (Matrix Instruction) (Vector Point) Stack -> Int -> *World -> *World 
initialize flags instrs starts arg = \seed = let
		[num:rng] = genRandInt seed
		start = starts.[num rem (size starts)]
	in program {
		mem={arg=arg,out=zero,tmp=Nothing},
		end=False,
		ini=Nothing,
		dir=East,
		pos=if(wrapping) start {x=0,y=0},
		rng=rng
		}
where
	source = finalizeProgram flags instrs
	wrapping = size starts > 0
	rs = rows source
	cs = cols source
	program :: State !*World -> *World
	program state=:{end=True} w
		| flags.debug
			= trace_n ("Terminated with state:\n T: "<+maybe ['_'] (repr False) state.mem.tmp<+"\n A: "<+repr False state.mem.arg<+"\n O: "<+repr False state.mem.out) w
		| otherwise
			= w
	program state w
		# w = if(flags.debug) (trace_n ("Located at ("<+state.pos.x<+","<+state.pos.y<+"), going "<+state.dir)) id w
		# (state=:{pos,dir}, w) = source.[state.pos.y, state.pos.x] state w
		# pos = case dir of
			North = {pos&y=pos.y-1}
			East = {pos&x=pos.x+1}
			South = {pos&y=pos.y+1}
			West = {pos&x=pos.x-1}
		# state = {state&pos={pos&x=(pos.x+cs)rem(cs),y=(pos.y+rs)rem(rs)},end=state.end||((not wrapping)&&(pos.x<0||pos.x>=cs||pos.y<0||pos.y>=rs))}
		= program state w