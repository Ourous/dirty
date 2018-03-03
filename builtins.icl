implementation module builtins

import types, atomics, arithmetic, utilities, StdEnv, StdLib, unicode

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

// coalescing operators
logicEquiv :: !Number -> Number
logicEquiv arg = fromBool (toBool arg)
logicNegate :: !Number -> Number
logicNegate arg = fromBool (not (toBool arg))

// remaining math ops
primeFactors :: !Number -> [Number]
primeFactors NaN = []
primeFactors arg
	| abs arg < one + one || numCeiling arg <> numFloor arg
		= []
	| otherwise
		# factors = [n \\ n <- [one+one..abs arg-one] | arg mod n == Zero && (toBool o isPrime) n]
		= factors ++ (primeFactors (arg/(prod factors)))
conjugate :: !Number -> Number
conjugate (Im (Fin val)) = (Im (Fin (~val)))
conjugate (Cx (Fin val=:{im})) = (Cx (Fin {val&im=(~im)}))
conjugate (Im (Inf Positive)) = (Im (Inf Negative))
conjugate (Im (Inf Negative)) = (Im (Inf Positive))
conjugate arg = arg
justReal :: !Number -> Number
justReal (Im _) = Zero
justReal (Cx (Fin {re})) = (Re (Fin re))
justReal (Cx (Inf Directed)) = NaN
justReal NaN = NaN
justReal arg = arg
justImag :: !Number -> Number
justImag (Re _) = Zero
justImag (Cx (Fin {im})) = (Im (Fin im))
justImag (Cx (Inf Directed)) = NaN
justImag NaN = NaN
justImag arg = arg
reciprocal :: !Number -> Number
reciprocal arg = one / arg
imagUnit :: !Number -> Number
imagUnit arg = arg * (Im (Fin one))
numPermute :: !Number !Number -> Number
numPermute lhs rhs = prod [numFloor(lhs - rhs)..lhs]
numCombin :: !Number !Number -> Number
numCombin lhs rhs = (numPermute lhs rhs) / prod [one..rhs]
logarithm :: !Number !Number -> Number
logarithm lhs rhs = (ln rhs) / (ln lhs)

// vectorized ops
vectorPlus :: [Number] [Number] -> [Number]
vectorPlus lhs rhs = zipWith (+) lhs rhs
vectorMinus :: [Number] [Number] -> [Number]
vectorMinus lhs rhs = zipWith (-) lhs rhs
vectorTimes :: [Number] [Number] -> [Number]
vectorTimes lhs rhs = zipWith (*) lhs rhs
vectorNegate :: [Number] -> [Number]
vectorNegate arg = map (~) arg
vectorAND :: [Number] [Number] -> [Number]
vectorAND lhs rhs = zipWith bitAND lhs rhs
vectorOR :: [Number] [Number] -> [Number]
vectorOR lhs rhs = zipWith bitOR lhs rhs
vectorElementOf :: [Number] [Number] -> [Number]
vectorElementOf lhs rhs = map (\e -> isElementOf e rhs) lhs
vectorLessThan :: [Number] [Number] -> [Number]
vectorLessThan lhs rhs = zipWith isLessThan lhs rhs
vectorGreaterThan :: [Number] [Number] -> [Number]
vectorGreaterThan lhs rhs = zipWith isGreaterThan lhs rhs
vectorIsEqual :: [Number] [Number] -> [Number]
vectorIsEqual lhs rhs = zipWith isEqualTo lhs rhs
vectorLessOrEqual :: [Number] [Number] -> [Number]
vectorLessOrEqual lhs rhs = zipWith isLessOrEqual lhs rhs
vectorGreaterOrEqual :: [Number] [Number] -> [Number]
vectorGreaterOrEqual lhs rhs = zipWith isGreaterOrEqual lhs rhs

// miscelaneous operators
toUppercase :: !Number -> Number
toUppercase arg = fromInt (toUpperUChar (toInt arg))
toLowercase :: !Number -> Number
toLowercase arg = fromInt (toLowerUChar (toInt arg))
splitOnNewlines :: [Number] -> [[Number]]
splitOnNewlines [] = []
splitOnNewlines arg
	# (head, tail) = span (\e -> toInt e <> 10) arg
	| isEmpty tail
		= [head]
	| otherwise
		= [head:splitOnNewlines (tl tail)]

// "set" operators
setMinimum :: [Number] -> Number
setMinimum [] = NaN
setMinimum [head:tail] = foldl (min) head tail
setMaximum :: [Number] -> Number
setMaximum [] = NaN
setMaximum [head:tail] = foldl (max) head tail
setFilter :: [Number] [Number] -> [Number]
setFilter lhs rhs = [el \\ el <- lhs & cond <- rhs | toBool cond]
antiFilter :: [Number] [Number] -> [Number]
antiFilter lhs rhs = [el \\ el <- lhs & cond <- rhs | (not o toBool) cond]

// special cases
matrixProduct :: !Memory -> Memory
matrixProduct memory=:{left, right} = let
		matrix = [El [lhs * rhs \\ rhs <- right] \\ lhs <- left]
	in {memory&main=matrix++SET_NEW_DELIM memory.main}
joinWithNewlines :: !Memory -> Memory
joinWithNewlines memory=:{main} = let
		(base, other) = span (not o ACTIVE_CURSOR) main
		safeBase = [el \\ (El el) <- base]
		joined = foldl (\a b -> a ++ [fromInt 10] ++ b) [] safeBase
	in {memory&main=[El joined:other]}
stacksFromCursor :: !Memory -> Memory
stacksFromCursor memory=:{main=[El mid:other]} = let
		base = takeWhile (not o ACTIVE_CURSOR) memory.main
		stacks = sum [1 \\ (El _) <- base]
	in {memory&main=[El[fromInt stacks:mid]:other]}
transposeFromCursor :: !Memory -> Memory
transposeFromCursor memory=:{main} = let
		(base, other) = span (not o ACTIVE_CURSOR) main
		safeBase = [el \\ (El el) <- base]
		transposed = [(El el) \\ el <- transpose safeBase]
	in {memory&main=transposed ++ other}
stackJoin :: !Memory -> Memory
stackJoin memory=:{main} = let
		(base, other) = span (not o ACTIVE_CURSOR) main
		grouped = groupBy (\a b -> IS_DELIM a == IS_DELIM b) base
		flattened = [El (flatten [el \\ (El el) <- part]) \\ part <- grouped | case part of [Delim _] = False; _ = True]
	in {memory&main=flattened ++ other}
stackUnjoin :: !Memory -> Memory
stackUnjoin memory=:{main=[El mid:other]} = let
		singles = [El [el] \\ el <- mid]
	in {memory&main=singles ++ SET_NEW_DELIM other}

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
stackRotate Middle memory=:{main=[El [top:mid]:other]}
	= let rotate = rotateList (toInt top)
	in {memory&main=[El(rotate mid):other]}
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
stackDelete Middle memory=:{main=[El mid:other]}
	= {memory&main=other}
stackDelete Both memory = {memory&left=[],right=[]}
stackDelete Base memory=:{main}
	= let (base, other) = span (not o ACTIVE_CURSOR) main
	in {memory&main=SET_FIRST_DELIM (SAFE_TAIL other)}
stackDelete Main memory = {memory&main=[]}
stackDelete All memory = {memory&left=[],right=[],main=[]}

stackDrop :: !StackID !Memory -> Memory
stackDrop _ memory=:{main=[El []:_]} = memory
stackDrop Left memory=:{left, main=[El [top:mid]:other]} = let
		val = toInt top
		fn = if(val<=0) (take(~val)) (drop val)
	in {memory&left=fn left,main=[El mid:other]}
stackDrop Right memory=:{right, main=[El [top:mid]:other]} = let
		val = toInt top
		fn = if(val<=0) (take(~val)) (drop val)
	in {memory&right=fn right,main=[El mid:other]}
stackDrop Middle memory=:{main=[El [top:mid]:other]} = let
		val = toInt top
		fn = if(val<=0) (take(~val)) (drop val)
	in {memory&main=[El(fn mid):other]}
stackDrop Both memory=:{left, right, main=[El [top:mid]:other]} = let
		val = toInt top
		fn = if(val<=0) (take(~val)) (drop val)
	in {memory&left=fn left,right=fn right,main=[El mid:other]}
stackDrop Base memory=:{main=[El [top:mid]:other]} = let
		val = toInt top
		fn = if(val<=0) (take(~val)) (drop val)
		(base, other) = span (not o ACTIVE_CURSOR) [El mid:other]
	in {memory&main=fn base ++ other}
	
cycleTops :: !Rotation !Memory -> Memory
cycleTops Clockwise memory=:{left, main=[El mid:other], right}
	= {memory&left=SAFE_HEAD right++SAFE_TAIL left,right=SAFE_HEAD mid++SAFE_TAIL right,main=[El(SAFE_HEAD left++SAFE_TAIL mid):other]}
cycleTops Anticlockwise memory=:{left, main=[El mid:other], right}
	= {memory&left=SAFE_HEAD mid++SAFE_TAIL left,right=SAFE_HEAD left++SAFE_TAIL right,main=[El(SAFE_HEAD right++SAFE_TAIL mid):other]}
	
cycleStacks :: !Rotation !Memory -> Memory
cycleStacks Clockwise memory=:{left, main=[El mid:other], right}
	= {memory&left=right,right=mid,main=[El left:other]}
cycleStacks Anticlockwise memory=:{left, main=[El mid:other], right}
	= {memory&left=mid,right=left,main=[El right:other]}
	
unpackLeftRight :: !Memory -> Memory
unpackLeftRight memory=:{left, main=[El[lhs,rhs:mid]:other], right}
	= {memory&left=[lhs:left],right=[rhs:right],main=[El mid:other]}
unpackLeftRight memory=:{left, main=[El[lhs]:other]}
	= {memory&left=[lhs:left],main=[El []:other]}
unpackLeftRight memory = memory

unpackRightLeft :: !Memory -> Memory
unpackRightLeft memory=:{left, main=[El[rhs,lhs:mid]:other], right}
	= {memory&left=[lhs:left],right=[rhs:right],main=[El mid:other]}
unpackRightLeft memory=:{right, main=[El[rhs]:other]}
	= {memory&right=[rhs:right],main=[El []:other]}
unpackRightLeft memory = memory

swapLeftRight :: !Memory -> Memory
swapLeftRight memory=:{left, right}
	= {memory&left=right,right=left}
	
swapTop :: !Axes !Memory -> Memory
swapTop Horizontal memory=:{left, right}
	= {memory&left=SAFE_HEAD right++SAFE_TAIL left,right=SAFE_HEAD left++SAFE_TAIL right}
swapTop Vertical memory=:{main=[El [top:mid=:[_:_]]:other]}
	= {memory&main=[El ([last mid:init mid]++[top]):other]}
swapTop Identity memory=:{main=[El mid:other], right}
	= {memory&right=SAFE_HEAD mid++SAFE_TAIL right,main=[El(SAFE_HEAD right++SAFE_TAIL mid):other]}
swapTop Inverse memory=:{left, main=[El mid:other]}
	= {memory&left=SAFE_HEAD mid++SAFE_TAIL left,main=[El(SAFE_HEAD left++SAFE_TAIL mid):other]}
	
moveTop :: !Direction !Memory -> Memory
moveTop East memory=:{left, right=[top:right]}
	= {memory&left=[top:left],right=right}
moveTop West memory=:{left=[top:left], right}
	= {memory&left=left,right=[top:right]}
moveTop South memory=:{main=[El [top,next:mid]:other]}
	= {memory&main=[El [next,top:mid]:other]}
moveTop NorthWest memory=:{left, main=[El [top:mid]:other]}
	= {memory&left=[top:left],main=[El mid:other]}
moveTop NorthEast memory=:{right, main=[El [top:mid]:other]}
	= {memory&right=[top:right],main=[El mid:other]}
moveTop SouthWest memory=:{right=[top:right], main=[El mid:other]}
	= {memory&right=right,main=[El[top:mid]:other]}
moveTop SouthEast memory=:{left=[top:left], main=[El mid:other]}
	= {memory&left=left,main=[El[top:mid]:other]}
	
copyTop :: !Direction !Memory -> Memory
copyTop East memory=:{left, right=[top:_]}
	= {memory&left=[top:left]}
copyTop West memory=:{left=[top:_], right}
	= {memory&right=[top:right]}
copyTop North memory=:{main=[El [top:mid]:other]}
	= {memory&main=[El [top,top:mid]:other]}
copyTop NorthWest memory=:{left, main=[El [top:_]:_]}
	= {memory&left=[top:left]}
copyTop NorthEast memory=:{right, main=[El [top:_]:_]}
	= {memory&right=[top:right]}
copyTop SouthWest memory=:{right=[top:_], main=[El mid:other]}
	= {memory&main=[El[top:mid]:other]}
copyTop SouthEast memory=:{left=[top:_], main=[El mid:other]}
	= {memory&main=[El[top:mid]:other]}
	
copyBoth :: !Axes !Memory -> Memory
copyBoth Horizontal memory=:{left=[lhs:_], right=[rhs:_]}
	= {memory&left=[rhs:memory.left],right=[lhs:memory.right]}
copyBoth Vertical memory=:{main=[El (mid=:[_:_]):other]}
	= {memory&main=[El([last mid:mid]++[hd mid]):other]}
	
moveAll :: !Direction !Memory -> Memory
moveAll NorthWest memory=:{left, main=[El mid:other]}
	= {memory&left=mid++left,main=other}
moveAll NorthEast memory=:{right, main=[El mid:other]}
	= {memory&right=mid++right,main=other}
moveAll SouthWest memory=:{right, main}
	= {memory&right=[],main=[El right:SET_NEW_DELIM main]}
moveAll SouthEast memory=:{left, main}
	= {memory&left=[],main=[El left:SET_NEW_DELIM main]}