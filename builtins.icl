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
		(base, other) = span (not o IS_DELIM) main
	in {memory&main=reverse base ++ other}
stackReverse All memory=:{left, main, right}
	= {memory&left=reverse left, main=reverseEach main, right=reverse right}
where
	reverseEach [] = []
	reverseEach [El head:tail]
		= [El (reverse head):reverseEach tail]
	reverseEach [Delimiter:tail]
		= [Delimiter:reverseEach tail]
//stackRotate :: StackID Memory -> Memory
//stackDelete :: StackID Memory -> Memory
//stackDrop :: StackID Memory -> Memory