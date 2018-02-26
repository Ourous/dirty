definition module utilities

import types, StdClass

instance == Direction

instance == StackID

rotateList :: a u:[b] -> v:[b] | Enum a, [u <= v]

// :: [Number] -> Bool
TO_BOOL stack
	:== case stack of
		[] = False
		[Zero:_] = False
		[NaN:_] = False
		_ = True
		
// :: Element -> Bool
IS_DELIM element
	:== case element of
		Delimiter = True
		_ = False