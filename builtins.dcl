definition module builtins

import types

/*
// "boolean" functions, returning -1 for True and 0 for False
isLessThan :: Number Number -> Number
isGreaterThan :: Number Number -> Number
isEqualTo :: Number Number -> Number
isLessOrEqual :: Number Number -> Number
isGreaterOrEqual :: Number Number -> Number
isNotEqual :: Number Number -> Number
isIdentical :: [Number] [Number] -> Number
isNotIdentical :: [Number] [Number] -> Number
isImproperSubsetOf :: [Number] [Number] -> Number
isImproperSupersetOf :: [Number] [Number] -> Number
isProperSubsetOf :: [Number] [Number] -> Number
isProperSupersetOf :: [Number] [Number] -> Number
isNotSubsetOf :: [Number] [Number] -> Number
isNotSupersetOf :: [Number] [Number] -> Number

isUppercase :: Number -> Number
isLoweracse :: Number -> Number

isFiniteReal :: Number -> Number
isFiniteNumber :: Number -> Number
isInfinity :: Number -> Number

isSorted :: [Number] -> Number

areAnyTrue :: [Number] -> Number
areAllTrue :: [Number] -> Number
*/

// exported macros with effective type signature

// :: [Number] -> Bool
TO_BOOL stack
	:== case stack of
		[] = False
		[Zero:_] = False
		[NaN:_] = False
		_ = True
