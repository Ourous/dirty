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

// :: Element -> Bool
TO_BOOL arg
	:== case arg of
		Nothing = False
		(Just {head}) = toBool head
		