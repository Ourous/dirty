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
		= fromBool (all (\e -> arg mod e <> Zero) [inc one..dec arg])

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
	| abs arg < inc one || numCeiling arg <> numFloor arg
		= []
	| otherwise
		# factors = [n \\ n <- [inc one..arg] | arg mod n == Zero && (toBool o isPrime) n]
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
numPermute lhs rhs = prod [lhs-rhs..lhs]
numCombin :: !Number !Number -> Number
numCombin lhs rhs = (numPermute lhs rhs) / prod [one..rhs]
logarithm :: !Number !Number -> Number
logarithm lhs rhs = (ln rhs) / (ln lhs)
numProduct :: [Number] -> Number
numProduct [] = Zero
numProduct arg = foldl (*) one arg
numSum :: [Number] -> Number
numSum arg = foldl (+) Zero arg

// vectorized ops
vectorPlus :: [Number] [Number] -> [Number]
vectorPlus lhs rhs = zipWith (+) lhs rhs
vectorTimes :: [Number] [Number] -> [Number]
vectorTimes lhs rhs = zipWith (*) lhs rhs
vectorNegate :: [Number] -> [Number]
vectorNegate arg = map (~) arg
vectorAND :: [Number] [Number] -> [Number]
vectorAND lhs rhs = zipWith bitAND lhs rhs
vectorOR :: [Number] [Number] -> [Number]
vectorOR lhs rhs = zipWith bitOR lhs rhs
vectorIsEqual :: [Number] [Number] -> [Number]
vectorIsEqual lhs rhs = zipWith isEqualTo lhs rhs
vectorElementOf :: [Number] [Number] -> [Number]
vectorElementOf lhs rhs = map (\e -> isElementOf e rhs) lhs
vectorLessThan :: [Number] [Number] -> [Number]
vectorLessThan lhs rhs = zipWith isLessThan lhs rhs
vectorGreaterThan :: [Number] [Number] -> [Number]
vectorGreaterThan lhs rhs = zipWith isGreaterThan lhs rhs
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
fromLeftStepRight :: !Number !Number -> [Number]
fromLeftStepRight lhs rhs = [lhs, lhs + rhs..]
fromZeroToMiddle :: !Number -> [Number]
fromZeroToMiddle arg
	| arg < Zero
		= [Zero, ~one..arg]
	| arg > Zero
		= [Zero, one..arg]
	| otherwise
		= [arg]
fromMiddleAvoidZero :: !Number -> [Number]
fromMiddleAvoidZero arg
	| arg < Zero	
		= [arg, dec arg..]
	| arg > Zero
		= [arg, inc arg..]
	| otherwise
		= [arg]
fromLeftTimesRight :: !Number !Number -> [Number]
fromLeftTimesRight lhs rhs = yieldTimesRight lhs
where yieldTimesRight arg = [arg:yieldTimesRight(arg*rhs)]
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
groupMiddle :: [Number] -> [[Number]]
groupMiddle arg = group arg
dupesMiddle :: [Number] -> [Number]
dupesMiddle arg = [el \\ el <- arg | sum [1 \\ e <- arg | e == el] > 1]
setIntersection :: [Number] [Number] -> [Number]
setIntersection lhs rhs = removeDup (filter (\el -> isMember el rhs) lhs)
setUnion :: [Number] [Number] -> [Number]
setUnion lhs rhs = removeDup (lhs ++ rhs)
setExclusion :: [Number] [Number] -> [Number]
setExclusion lhs rhs = removeDup ((filter (not o \el -> isMember el rhs) lhs) ++ (filter (not o \el -> isMember el lhs) rhs))


// special cases
complexSplit :: !Memory -> Memory
complexSplit memory=:{left, right, main=[El [top:mid]:other]}
	= {memory&left=[justReal top:left],right=[justImag top:left],main=[El mid:other]}
complexSplit memory = memory
matrixProduct :: !Memory -> Memory
matrixProduct memory=:{left, right} = let
		matrix = [El [lhs * rhs \\ rhs <- right] \\ lhs <- left]
	in {memory&main=matrix++SET_NEW_DELIM memory.main}
joinWithNewlines :: !Memory -> Memory
joinWithNewlines memory=:{main}
	# (base, other) = span (not o ACTIVE_CURSOR) main
	= let
		safeBase = [el \\ (El el) <- base]
		joined = foldl (\a b -> a ++ [fromInt 10] ++ b) [] safeBase
	in {memory&main=[El joined:other]}
stacksFromCursor :: !Memory -> Memory
stacksFromCursor memory=:{main=[El mid:other]} = let
		base = takeWhile (not o ACTIVE_CURSOR) memory.main
		stacks = sum [1 \\ (El _) <- base]
	in {memory&main=[El[fromInt stacks:mid]:other]}
transposeFromCursor :: !Memory -> Memory
transposeFromCursor memory=:{main}
	# (base, other) = span (not o ACTIVE_CURSOR) main
	= let
		safeBase = [el \\ (El el) <- base]
		transposed = [(El el) \\ el <- transpose safeBase]
	in {memory&main=transposed ++ other}
stackJoin :: !Memory -> Memory
stackJoin memory=:{main}
	# (base, other) = span (not o ACTIVE_CURSOR) main
	= let
		grouped = groupBy (\a b -> IS_DELIM a == IS_DELIM b) base
		flattened = [El (flatten [el \\ (El el) <- part]) \\ part <- grouped | case part of [Delim _] = False; _ = True]
	in {memory&main=flattened ++ other}
stackUnjoin :: !Memory -> Memory
stackUnjoin memory=:{main=[El mid:other]} = let
		singles = [El [el] \\ el <- mid]
	in {memory&main=singles ++ SET_NEW_DELIM other}
removeDupBase :: !Memory -> Memory
removeDupBase memory=:{main}
	# (base, other) = span (not o ACTIVE_CURSOR) main
	= let 
		safeBase = [el \\ (El el) <- base]
		deduped = [El el \\ el <- removeDup safeBase]
	in {memory&main=deduped ++ other}
repeatTopMiddle :: !Memory -> Memory
repeatTopMiddle memory=:{main=[El []:_]} = memory
repeatTopMiddle memory=:{main=[El [top:mid]:other]}
	= {memory&main=[El(repeat top):SET_NEW_DELIM[El mid:other]]}
repeatFullMiddle :: !Memory -> Memory
repeatFullMiddle memory=:{main=[El mid:other]}
	= {memory&main=(repeat (El mid))++SET_NEW_DELIM other}
sortBaseline :: !Memory -> Memory
sortBaseline memory=:{main}
	# (base, other) = span (not o ACTIVE_CURSOR) main
	= let
		safeBase = [el \\ (El el) <- base]
		sorted = [El el \\ el <- sort safeBase]
	in {memory&main=sorted ++ other}
	
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
	# (base, other) = span (not o ACTIVE_CURSOR) main
	= {memory&main=reverseEach base ++ other}
where
	reverseEach [] = []
	reverseEach [El head:tail]
		= [El (reverse head):reverseEach tail]
	reverseEach [head:tail]
		= [head:reverseEach tail]
stackReverse Base memory=:{main}
	# (base, other) = span (not o ACTIVE_CURSOR) main
	= {memory&main=reverse base ++ other}
		
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
stackRotate Base memory=:{main=[El [top:mid]:other]}
	# (base, other) = span (not o ACTIVE_CURSOR) [El mid:other]
	= let rotate = rotateList (toInt top)
	in {memory&main=MERGE_DELIMS(rotate base ++ other)}

stackDelete :: !StackID !Memory -> Memory
stackDelete Left memory = {memory&left=[]}
stackDelete Right memory = {memory&right=[]}
stackDelete Middle memory=:{main=[El mid:other]}
	= {memory&main=other}
stackDelete Both memory = {memory&left=[],right=[]}
stackDelete Base memory=:{main}
	# (base, other) = span (not o ACTIVE_CURSOR) main
	= {memory&main=SET_FIRST_DELIM (SAFE_TAIL other)}
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
stackDrop Base memory=:{main=[El [top:mid]:other]}
	# (base, other) = span (not o ACTIVE_CURSOR) [El mid:other]
	= let
		val = toInt top
		fn = if(val<=0) (take(~val)) (drop val)
	in {memory&main=MERGE_DELIMS(fn base ++ other)}
	
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
swapTop _ memory = memory	

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
moveTop _ memory = memory
	
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
copyTop _ memory = memory
	
copyBoth :: !Axes !Memory -> Memory
copyBoth Horizontal memory=:{left=[lhs:_], right=[rhs:_]}
	= {memory&left=[rhs:memory.left],right=[lhs:memory.right]}
copyBoth Vertical memory=:{main=[El (mid=:[_:_]):other]}
	= {memory&main=[El([last mid:mid]++[hd mid]):other]}
copyBoth _ memory = memory
	
moveAll :: !Direction !Memory -> Memory
moveAll NorthWest memory=:{left, main=[El mid:other]}
	= {memory&left=mid++left,main=other}
moveAll NorthEast memory=:{right, main=[El mid:other]}
	= {memory&right=mid++right,main=other}
moveAll SouthWest memory=:{right, main}
	= {memory&right=[],main=[El right:SET_NEW_DELIM main]}
moveAll SouthEast memory=:{left, main}
	= {memory&left=[],main=[El left:SET_NEW_DELIM main]}
	
replicateBase :: !Memory -> Memory
replicateBase memory=:{main}
	# (base, other) = span (not o ACTIVE_CURSOR) main
	= {memory&main=base++NEW_FIRST_DELIM main}
	
replicateMiddle :: !Memory -> Memory
replicateMiddle memory=:{main=[El mid:other]}
	= {memory&main=[El mid:SET_NEW_DELIM[El mid:other]]}
	
replicateTop :: !Memory -> Memory
replicateTop memory=:{main=[El mid:other]}
	= {memory&main=[El(SAFE_HEAD mid):SET_NEW_DELIM[El mid:other]]}
	
dupesBase :: !Memory -> Memory
dupesBase memory=:{main}
	# (base, other) = span (not o ACTIVE_CURSOR) main
	= let
		safeBase = [el \\ (El el) <- base]
		deduplicated = [El el \\ el <- safeBase | sum [1 \\ e <- safeBase | e == el] > 1]
	in {memory&main=deduplicated ++ other}
	
shiftCursorDownwards :: !Memory -> Memory
shiftCursorDownwards memory=:{main}
	# (base, [cur:other]) = span (not o ACTIVE_CURSOR) main
	| isEmpty other
		= memory
	| otherwise
		= {memory&main=base ++ [Delim False:SET_FIRST_DELIM other]}
		
shiftCursorUpwards :: !Memory -> Memory
shiftCursorUpwards memory=:{main}
	# (base, [cur:other]) = span (not o ACTIVE_CURSOR) main
	| any IS_DELIM base
		= {memory&main=setLastDelim base ++ [Delim False:other]}
	| otherwise
		= {memory&main=[hd base:SET_NEW_DELIM(tl base)]++[Delim False:other]}
where
	setLastDelim [] = []
	setLastDelim [Delim False:tail]
		| any IS_DELIM tail
			= [Delim False:setLastDelim tail]
		| otherwise
			= [Delim True:tail]
	setLastDelim [head:tail] = [head:setLastDelim tail]
	
moveCursorForwards :: !Memory -> Memory
moveCursorForwards memory=:{main}
	# (base, [cur:other]) = span (not o ACTIVE_CURSOR) main
	| isEmpty other
		= memory
	| otherwise
		= {memory&main=MERGE_DELIMS(init base ++ [Delim True, last base:other])}
	
moveCursorBackwards :: !Memory -> Memory
moveCursorBackwards memory=:{main}
	# (base, [cur:other]) = span (not o ACTIVE_CURSOR) main
	| isEmpty other
		= {memory&main=MERGE_DELIMS(init base ++ [Delim True, last base, Delim False])}
	| otherwise
		= {memory&main=MERGE_DELIMS(base ++ [hd other, Delim True:tl other])}
		
remember :: !Memory -> Memory
remember memory=:{main=[El [top:mid]:other]}
	= {memory&main=[El mid:other],note=top}

recall :: !Memory -> Memory
recall memory=:{main=[El mid:other], note}
	= {memory&main=[El[note:mid]:other]}