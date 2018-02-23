implementation module utilities

import StdEnv, StdLib

rotate :: a u:[b] -> v:[b] | Enum a, [u <= v]
rotate _ [] = []
rotate n list
	| n > zero
		= rotate (dec n) ((tl list) ++ [hd list])
	| n < zero
		= rotate (inc n) [last list:init list]
	| otherwise
		= list