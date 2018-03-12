definition module utilities

import types, StdOverloaded

from StdClass import class Enum, class IncDec

instance == Direction

instance == StackID

rotateList :: a u:[b] -> v:[b] | Enum a, [u <= v]

mergeDelims :: !Memory -> Memory

//isLastDelim :: [Element] -> Bool

//setAllFalse :: [Element] -> [Element]

//setWhenLast :: [Element] -> [Element]

IS_IMAG num :== case num of
	(Im _) = True
	_ = False

IS_REAL num :== case num of
	(Re _) = True
	Zero = True
	_ = False
	
IS_CPLX num :== case num of
	(Cx _) = True
	_ = False

SAFE_HEAD list
	:== case list of
		[] = []
		[head:_] = [head]

SAFE_TAIL list
	:== case list of
		[] = []
		[_:tail] = tail

// :: [Number] -> Bool
TO_BOOL {stack}
	:== case stack of
		[!] = False
		[!Zero:_] = False
		[!NaN:_] = False
		_ = True
		
// :: Element -> Bool
IS_DELIM element
	:== case element of
		(Delim _) = True
		_ = False
		
IS_ELEM element
	:== case element of
		(El _) = True
		_ = False
		
// :: ([Number] -> [Number]) -> (Element -> Element)
APPLY_ELEM func
	:== \elem -> case elem of
		(El arg) = (El (func arg))
		else = else
		
APPLY_DELIM func
	:== \elem -> case elem of
		(Delim arg) = (Delim (func arg))
		else = else
		
DELIM_FUNC default func
	:== \elem -> case elem of
		(Delim val) = func val
		_ = default
		
ELEM_FUNC default func
	:== \elem -> case elem of
		(El val) = func val
		_ = default