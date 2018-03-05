definition module utilities

import types, StdOverloaded

from StdClass import class Enum, class IncDec

instance == Direction

instance == StackID

rotateList :: a u:[b] -> v:[b] | Enum a, [u <= v]

//isLastDelim :: [Element] -> Bool

//setAllFalse :: [Element] -> [Element]

//setWhenLast :: [Element] -> [Element]

SAFE_HEAD list
	:== case list of
		[] = []
		[head:_] = [head]

SAFE_TAIL list
	:== case list of
		[] = []
		[_:tail] = tail

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
		
// :: ([Number] -> [Number]) -> (Element -> Element)
APPLY_IF_ELEM func
	:== \elem -> case elem of
		(El arg) = (El (func arg))
		else = else
		
// :: Element -> Bool
ACTIVE_CURSOR element
	:== case element of
		(Delim cur) = cur
		_ = False
		
DROP_IF_DELIM stack :== case stack of
	[Delim _:tail] = tail
	_ = stack
		
MERGE_DELIMS :== mergeDelims
where
	mergeDelims [] = []
	mergeDelims [Delim False, Delim False:tail] = mergeDelims [Delim False:tail]
	mergeDelims [Delim _, Delim _:tail] = mergeDelims [Delim True:tail]
	mergeDelims [head:tail] = [head:mergeDelims tail]
	
ENSURE_ACTIVE :== ensureActive
where
	ensureActive main
		| areAnyActive main
			= ensureOneActive main
		| otherwise
			= SET_FIRST_DELIM main
	areAnyActive [] = False
	areAnyActive [Delim True:_] = True
	areAnyActive [_:tail] = areAnyActive tail
	ensureOneActive [] = []
	ensureOneActive [Delim True:tail] = SET_ALL_FALSE tail
	ensureOneActive [head:tail] = [head:ensureOneActive tail]

// :: [Element] -> Bool
IS_FIRST_DELIM :== isLastDelim
where
	isLastDelim [Delim cur:_] = cur
	isLastDelim [_:tail] = isLastDelim tail
		
// :: [Element] -> [Element]
SET_ALL_FALSE :== setAllFalse
where
	setAllFalse [] = []
	setAllFalse [Delim _:tail] = [Delim False:setAllFalse tail]
	setAllFalse [head:tail] = [head:setAllFalse tail]
	
// checks if the delimiter should move forward
// :: [Element] -> [Element]
SET_NEW_DELIM :== setNewDelim
where
	setNewDelim [Delim _:tail]
		= [Delim True:SET_ALL_FALSE tail]
	setNewDelim elements = let
			(cur, tail) = setLastFalse elements
		in [(Delim cur):tail]
	setLastFalse [Delim cur:tail] = (cur, [Delim False:tail])
	setLastFalse [head:tail] = let
			(cur, otherLast) = setLastFalse tail
		in (cur, [head:otherLast])
		
SET_FIRST_DELIM :== setFirstDelim
where
	setFirstDelim [] = []
	setFirstDelim [Delim _:tail] = [Delim True:SET_ALL_FALSE tail]
	setFirstDelim [head:tail] = [head:setFirstDelim tail]
	
NEW_FIRST_DELIM :== (\e -> SET_FIRST_DELIM (SET_NEW_DELIM e))