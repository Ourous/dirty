implementation module builtins

import types, arithmetic, utilities, StdEnv, StdLib, unicode

// "boolean" functions
isLessThan :: !Number !Number -> Number
isLessThan lhs rhs = fromBool (lhs < rhs)
isGreaterThan :: !Number !Number -> Number
isGreaterThan lhs rhs = fromBool (lhs > rhs)
isEqualTo :: !Number !Number -> Number
isEqualTo lhs rhs = fromBool (lhs == rhs)
isLessOrEqual :: !Number !Number -> Number
isLessOrEqual lhs rhs = fromBool (lhs <= rhs)
isGreaterOrEqual :: !Number !Number -> Number
isGreaterOrEqual lhs rhs = fromBool (lhs >= rhs)
isNotEqual :: !Number !Number -> Number
isNotEqual lhs rhs = fromBool (lhs <> rhs)
isIdentical :: [Number] [Number] -> Number
isIdentical lhs rhs = fromBool (lhs == rhs)

isElementOf :: !Number [Number] -> Number
isElementOf lhs rhs = fromBool (isMember lhs rhs)
isImproperSubsetOf :: [Number] [Number] -> Number
isImproperSubsetOf lhs rhs = fromBool (all (\e -> [0 \\ i <- lhs | i == e] <= [0 \\ i <- rhs | i == e]) lhs)
isProperSubsetOf :: [Number] [Number] -> Number
isProperSubsetOf lhs rhs = fromBool (all (\e -> [0 \\ i <- lhs | i == e] <= [0 \\ i <- rhs | i == e]) lhs && any (\e -> [0 \\ i <- lhs | i == e] < [0 \\ i <- rhs | i == e]) lhs)
isNotSubsetOf :: [Number] [Number] -> Number
isNotSubsetOf lhs rhs = fromBool (any (\e -> [0 \\ i <- lhs | i == e] > [0 \\ i <- rhs | i == e]) lhs)

isUppercase :: !Number -> Number
isUppercase arg = fromBool (isUpperUChar (toInt arg))
isLowercase :: !Number -> Number
isLowercase arg = (fromBool o isLowerUChar o toInt) arg

isFiniteReal :: !Number -> Number
isFiniteReal arg = fromBool case arg of
	Zero = True
	(Re (Fin _)) = True
	_ = False
isFiniteNumber :: !Number -> Number
isFiniteNumber arg = fromBool case arg of
	Zero = True
	(Re (Fin _)) = True
	(Im (Fin _)) = True
	(Cx (Fin _)) = True
	_ = False
isInfinite :: !Number -> Number
isInfinite arg = fromBool case arg of
	(Re (Inf _)) = True
	(Im (Inf _)) = True
	(Cx (Inf _)) = True
	_ = False

isPrime :: !Number -> Number
isPrime arg
	| numCeiling arg <> numFloor arg
		= fromBool False
	| otherwise
		= fromBool (all (\e -> arg mod e <> Zero) [one + one..arg - one])

isSorted :: [Number] -> Number
isSorted arg = fromBool (isSorted` arg)
where
	isSorted` [h1:tail=:[h2:_]] = h1 <= h2 && isSorted` tail
	isSorted` _ = True
	
areAnyTrue :: [Number] -> Number
areAnyTrue arg = fromBool (any toBool arg)
areAllTrue :: [Number] -> Number
areAllTrue arg = fromBool (all toBool arg)

// stack manipulations
stackReverse :: !StackID !Memory -> Memory
stackReverse Left memory=:{left}
	= {memory&left=reverse left}
stackReverse Right memory=:{right}
	= {memory&right=reverse right}
stackReverse Middle memory=:{main=[El mid:other]}
	= {memory&main=[El (reverse mid):other]}
stackReverse Both memory=:{left, right}
	= {memory&left=reverse left, right=reverse right}
stackReverse Primary memory=:{main}
	= let (base, other) = span (not o ACTIVE_CURSOR) main
	in {memory&main=reverseEach base ++ other}
where
	reverseEach [] = []
	reverseEach [El head:tail]
		= [El (reverse head):reverseEach tail]
	reverseEach [head:tail]
		= [head:reverseEach tail]
stackReverse Base memory=:{main}
	= let (base, other) = span (not o ACTIVE_CURSOR) main
	in {memory&main=reverse base ++ other}
stackReverse All memory=:{left, right, main}
	= {memory&left=reverse left,right=reverse right,main=reverseEach main}
where
	reverseEach [] = []
	reverseEach [El head:tail]
		= [El (reverse head):reverseEach tail]
	reverseEach [head:tail]
		= [head:reverseEach tail]
		
stackRotate :: !StackID !Memory -> Memory
stackRotate _ memory=:{main=[El []:_]} = memory
stackRotate Left memory=:{left, main=[El [top:mid]:other]}
	= {memory&left=rotateList(toInt top)left,main=[El mid:other]}
stackRotate Right memory=:{right, main=[El [top:mid]:other]}
	= {memory&right=rotateList(toInt top)right,main=[El mid:other]}
stackRotate Both memory=:{left, right, main=[El [top:mid]:other]}
	= let rotate = rotateList (toInt top)
	in {memory&left=rotate left,right=rotate right,main=[El mid:other]}
stackRotate Primary memory=:{main=[El [top:mid]:other]}
	= let rotate = rotateList (toInt top)
	in {memory&main=[El(rotate mid):map(APPLY_IF_ELEM rotate)other]}
stackRotate Base memory=:{main=[El [top:mid]:other]} = let
		rotate = rotateList (toInt top)
		(base, other) = span (not o ACTIVE_CURSOR) [El mid:other]
	in {memory&main=rotate base ++ other}
stackRotate All memory=:{left, right, main=[El [top:mid]:other]}
	= let rotate = rotateList (toInt top)
	in {memory&left=rotate left,right=rotate right,main=[El(rotate mid):map(APPLY_IF_ELEM rotate)other]}

stackDelete :: !StackID !Memory -> Memory
stackDelete Left memory = {memory&left=[]}
stackDelete Right memory = {memory&right=[]}
stackDelete Both memory = {memory&left=[],right=[]}
stackDelete Base memory=:{main}
	= let (base, other) = span (not o ACTIVE_CURSOR) main
	in {memory&main=SET_FIRST_DELIM (SAFE_TAIL other)}
stackDelete Main memory = {memory&main=[]}
stackDelete All memory = {memory&left=[],right=[],main=[]}

stackDrop :: !StackID !Memory -> Memory
stackDrop _ memory=:{main=[El []:_]} = memory
stackDrop Left memory=:{left, main=[El [top:mid]:other]}
	= let val = toInt top
	in {memory&left=if(val>0) (take(~val)) (drop val) left,main=[El mid:other]}
stackDrop Right memory=:{right, main=[El [top:mid]:other]}
	= let val = toInt top
	in {memory&right=if(val>0) (take(~val)) (drop val) right,main=[El mid:other]}
	