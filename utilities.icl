implementation module utilities

import types, StdEnv, StdLib

instance == Direction where
	(==) East East = True
	(==) West West = True
	(==) North North = True
	(==) South South = True
	(==) NorthEast NorthEast = True
	(==) NorthWest NorthWest = True
	(==) SouthWest SouthWest = True
	(==) SouthEast SouthEast = True
	(==) _ _ = False

rotateList :: a u:[b] -> v:[b] | Enum a, [u <= v]
rotateList _ [] = []
rotateList n list
	| n > zero
		= rotateList (dec n) ((tl list) ++ [hd list])
	| n < zero
		= rotateList (inc n) [last list:init list]
	| otherwise
		= list