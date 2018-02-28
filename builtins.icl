implementation module builtins

import types, arithmetic, utilities, StdEnv, StdLib

// "boolean" functions
isLessThan :: Number Number -> Number
isLessThan lhs rhs = fromBool (lhs < rhs)
isGreaterThan :: Number Number -> Number
isGreaterThan lhs rhs = fromBool (lhs > rhs)
isEqualTo :: Number Number -> Number
isEqualTo lhs rhs = fromBool (lhs == rhs)
isLessOrEqual :: Number Number -> Number
isLessOrEqual lhs rhs = fromBool (lhs <= rhs)
isGreaterOrEqual :: Number Number -> Number
isGreaterOrEqual lhs rhs = fromBool (lhs >= rhs)
isNotEqual :: Number Number -> Number
isNotEqual lhs rhs = fromBool (lhs <> rhs)
isIdentical :: [Number] [Number] -> Number
isIdentical lhs rhs = fromBool (lhs == rhs)

isElementOf :: Number [Number] -> Number
isElementOf lhs rhs = fromBool (isMember lhs rhs)
isImproperSubsetOf :: [Number] [Number] -> Number
isImproperSubsetOf lhs rhs = fromBool (all (\e -> [0 \\ i <- lhs | i == e] <= [0 \\ i <- rhs | i == e]) lhs)
isProperSubsetOf :: [Number] [Number] -> Number
isProperSubsetOf lhs rhs = fromBool (all (\e -> [0 \\ i <- lhs | i == e] <= [0 \\ i <- rhs | i == e]) lhs && any (\e -> [0 \\ i <- lhs | i == e] < [0 \\ i <- rhs | i == e]) lhs)
isNotSubsetOf :: [Number] [Number] -> Number
isNotSubsetOf lhs rhs = fromBool (any (\e -> [0 \\ i <- lhs | i == e] > [0 \\ i <- rhs | i == e]) lhs)

// stack manipulations
stackReverse :: StackID Memory -> Memory
stackReverse Left memory=:{left}
	= {memory&left=reverse left}
stackReverse Right memory=:{right}
	= {memory&right=reverse right}
stackReverse Middle memory=:{main=[El mid:other]}
	= {memory&main=[El (reverse mid):other]}
stackReverse Both memory=:{left, right}
	= {memory&left=reverse left, right=reverse right}
stackReverse Primary memory=:{left, main=[El mid:other], right}
	= {memory&left=reverse left, main=[El (reverse mid):other], right=reverse right}
stackReverse Base memory=:{main}
	= let
		(base, other) = span (not o ACTIVE_CURSOR) main
	in {memory&main=reverse base ++ other}
stackReverse All memory=:{main}
	= {memory&main=reverseEach main}
where
	reverseEach [] = []
	reverseEach [El head:tail]
		= [El (reverse head):reverseEach tail]
	reverseEach [head:tail]
		= [head:reverseEach tail]
//stackRotate :: StackID Memory -> Memory
//stackDelete :: StackID Memory -> Memory
//stackDrop :: StackID Memory -> Memory