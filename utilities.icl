implementation module utilities

import StdEnv, StdLib

rotateList :: a u:[b] -> v:[b] | Enum a, [u <= v]
rotateList _ [] = []
rotateList n list
	| n > zero
		= rotateList (dec n) ((tl list) ++ [hd list])
	| n < zero
		= rotateList (inc n) [last list:init list]
	| otherwise
		= list