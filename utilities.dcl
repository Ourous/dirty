definition module utilities

import types, StdClass

instance == Direction

instance == StackID

rotateList :: a u:[b] -> v:[b] | Enum a, [u <= v]

//isLastDelim :: [Element] -> Bool

//setAllFalse :: [Element] -> [Element]

//setWhenLast :: [Element] -> [Element]

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
		(Delim _) = True
		_ = False
		
// :: Element -> Bool
ACTIVE_CURSOR element
	:== case element of
		(Delim cur) = cur
		_ = False

// :: [Element] -> Bool
IS_LAST_DELIM :== isLastDelim
where
	isLastDelim [Delim cur:_] = cur
	isLastDelim [_:tail] = isLastDelim tail
		
// :: [Element] -> [Element]
SET_ALL_FALSE :== setAllFalse
where
	setAllFalse [] = []
	setAllFalse [Delim _:tail] = [Delim False:setAllFalse tail]
	setAllFalse [head:tail] = [head:setAllFalse tail]
	
// :: [Element] -> [Element]
NEW_WHEN_LAST :== newWhenLast
where
	newWhenLast elements = let
			(cur, tail) = setLastFalse elements
		in [(Delim cur):tail]
	setLastFalse [Delim cur:tail] = (cur, [Delim False:tail])
	setLastFalse [head:tail] = let
			(cur, otherLast) = setLastFalse tail
		in (cur, [head:otherLast])