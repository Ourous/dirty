implementation module builtins

import types, atomics, arithmetic, utilities, StdEnv, StdLib, unicode, stacks

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
isIdentical :: !(Stack Number) !(Stack Number) -> Number
isIdentical lhs rhs = fromBool (lhs == rhs)

isElementOf :: !Number !(Stack Number) -> Number
isElementOf lhs rhs = fromBool (areAny ((==) lhs) rhs)//fromBool (isMember lhs rhs)
isImproperSubsetOf :: !(Stack Number) !(Stack Number) -> Number
isImproperSubsetOf lhs rhs = fromBool (areAll (\e -> occurrences ((==) e) lhs <= occurrences ((==) e) rhs) lhs)
//isImproperSubsetOf lhs rhs = fromBool (all (\e -> [0 \\ i <- lhs | i == e] <= [0 \\ i <- rhs | i == e]) lhs)
isProperSubsetOf :: !(Stack Number) !(Stack Number) -> Number
isProperSubsetOf lhs rhs = fromBool (areAll (\e -> occurrences ((==) e) lhs <= occurrences ((==) e) rhs) lhs && areAny (\e -> occurrences ((==) e) lhs < occurrences ((==) e) rhs) lhs)
//isProperSubsetOf lhs rhs = fromBool (all (\e -> [0 \\ i <- lhs | i == e] <= [0 \\ i <- rhs | i == e]) lhs && any (\e -> [0 \\ i <- lhs | i == e] < [0 \\ i <- rhs | i == e]) lhs)
isNotSubsetOf :: !(Stack Number) !(Stack Number) -> Number
isNotSubsetOf lhs rhs = fromBool (areAny (\e -> occurrences ((==) e) lhs > occurrences ((==) e) rhs) lhs)
//isNotSubsetOf lhs rhs = fromBool (any (\e -> [0 \\ i <- lhs | i == e] > [0 \\ i <- rhs | i == e]) lhs)

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

isSorted :: !(Stack Number) -> Number
isSorted {bounded=False} = Zero
isSorted {stack} = fromBool (isSorted` stack)
where
	isSorted` [!h1:tail=:[!h2:_]] = h1 <= h2 && isSorted` tail
	isSorted` _ = True
	
areAnyTrue :: !(Stack Number) -> Number
areAnyTrue arg = fromBool (areAny toBool arg)
areAllTrue :: !(Stack Number) -> Number
areAllTrue arg = fromBool (areAll toBool arg)

// coalescing operators
logicEquiv :: !Number -> Number
logicEquiv arg = fromBool (toBool arg)
logicNegate :: !Number -> Number
logicNegate arg = fromBool (not (toBool arg))

// remaining math ops
primeFactors :: !Number -> (Stack Number)
primeFactors _ = zero
/*
primeFactors NaN = []
primeFactors arg
	| abs arg < inc one || numCeiling arg <> numFloor arg
		= []
	| otherwise
		# factors = [n \\ n <- [inc one..arg] | arg mod n == Zero && (toBool o isPrime) n]
		= factors ++ (primeFactors (arg/(prod factors)))
*/
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
numProduct :: !(Stack Number) -> Number
numProduct {stack=[!]} = Zero
numProduct {bounded=False} = NaN
numProduct arg = reduce (*) one arg//foldl (*) one arg
numSum :: !(Stack Number) -> Number
numSum {bounded=False} = NaN
numSum arg = reduce (+) Zero arg//foldl (+) Zero arg

// vectorized ops
vectorPlus :: !(Stack Number) !(Stack Number) -> (Stack Number)
vectorPlus lhs rhs = withEach (+) lhs rhs
vectorTimes :: !(Stack Number) !(Stack Number) -> (Stack Number)
vectorTimes lhs rhs = withEach (*) lhs rhs
vectorNegate :: !(Stack Number) -> (Stack Number)
vectorNegate arg = forEach (~) arg
vectorAND :: !(Stack Number) !(Stack Number) -> (Stack Number)
vectorAND lhs rhs = withEach bitAND lhs rhs
vectorOR :: !(Stack Number) !(Stack Number) -> (Stack Number)
vectorOR lhs rhs = withEach bitOR lhs rhs
vectorIsEqual :: !(Stack Number) !(Stack Number) -> (Stack Number)
vectorIsEqual lhs rhs = withEach isEqualTo lhs rhs
vectorElementOf :: !(Stack Number) !(Stack Number) -> (Stack Number)
vectorElementOf lhs rhs = forEach (\e -> isElementOf e rhs) lhs
vectorLessThan :: !(Stack Number) !(Stack Number) -> (Stack Number)
vectorLessThan lhs rhs = withEach isLessThan lhs rhs
vectorGreaterThan :: !(Stack Number) !(Stack Number) -> (Stack Number)
vectorGreaterThan lhs rhs = withEach isGreaterThan lhs rhs
vectorLessOrEqual :: !(Stack Number) !(Stack Number) -> (Stack Number)
vectorLessOrEqual lhs rhs = withEach isLessOrEqual lhs rhs
vectorGreaterOrEqual :: !(Stack Number) !(Stack Number) -> (Stack Number)
vectorGreaterOrEqual lhs rhs = withEach isGreaterOrEqual lhs rhs

// miscelaneous operators
toUppercase :: !Number -> Number
toUppercase arg = fromInt (toUpperUChar (toInt arg))
toLowercase :: !Number -> Number
toLowercase arg = fromInt (toLowerUChar (toInt arg))
splitOnNewlines :: !(Stack Number) -> (Stack Element)
splitOnNewlines arg
	# (head, tail) = splitWhen (\e -> toInt e == 10) arg
	| tail == zero
		= fromSingle (El head)
	| otherwise
		= fromSingle (El head) + splitOnNewlines (tailOf tail)

// "set" operators
fromLeftStepRight :: !Number !Number -> (Stack Number)
fromLeftStepRight lhs rhs = fromStrictList [!lhs, lhs + rhs..] False
fromOneToMiddle :: !Number -> (Stack Number)
fromOneToMiddle arg
	| IS_CPLX arg
		= fromSingle arg
	# unit = if(IS_IMAG arg) imagUnit id one
	| arg < Zero
		= fromStrictList [!Zero - unit, Zero - unit - unit..arg] True
	| arg > Zero
		= fromStrictList [!unit, unit + unit..arg] True
	| otherwise
		= fromSingle arg
fromMiddleToZero :: !Number -> (Stack Number)
fromMiddleToZero arg
	| IS_CPLX arg
		= fromSingle arg
	# unit = if(IS_IMAG arg) imagUnit id one
	| arg < Zero
		= fromStrictList [!arg, arg + unit..Zero] True
	| arg > Zero
		= fromStrictList [!arg, arg - unit..Zero] True
	| otherwise
		= fromSingle arg
fromLeftTimesRight :: !Number !Number -> (Stack Number)
fromLeftTimesRight lhs rhs = fromStrictList (yieldTimesRight lhs) False
where yieldTimesRight arg = [!arg:yieldTimesRight(arg*rhs)]
setMinimum :: !(Stack Number) -> Number
setMinimum {stack=[!]} = NaN
setMinimum arg = reduce (min) (headOf arg) (tailOf arg)
setMaximum :: !(Stack Number) -> Number
setMaximum {stack=[!]} = NaN
setMaximum arg = reduce (max) (headOf arg) (tailOf arg)
setFilter :: !(Stack Number) !(Stack Number) -> (Stack Number)
setFilter lhs rhs = filterOn (toBool) lhs rhs
antiFilter :: !(Stack Number) !(Stack Number) -> (Stack Number)
antiFilter lhs rhs = filterOn (not o toBool) lhs rhs// [el \\ el <- lhs.stack & cond <- rhs.stack | (not o toBool) cond]
dupesMiddle :: !(Stack Number) -> (Stack Number)
dupesMiddle arg = filterBy (\e -> occurrences ((==) e) arg > 1) arg//[el \\ el <- arg | sum [1 \\ e <- arg | e == el] > 1]
groupMiddle :: !(Stack Number) -> (Stack Element)
groupMiddle arg=:{bounded}
	# list = toList arg
	# groups = map (\e -> (El (fromList e False))) (group list) // TODO: all complete groups are bounded, find a way to implement that
	= fromList groups bounded
setIntersection :: !(Stack Number) !(Stack Number) -> (Stack Number)
setIntersection lhs rhs = uniques (filterBy (\e -> areAny ((==) e) rhs) lhs)//removeDup (filter (\el -> isMember el rhs) lhs)
setUnion :: !(Stack Number) !(Stack Number) -> (Stack Number)
setUnion lhs rhs = abort "TBI"//removeDup (lhs ++ rhs)
setExclusion :: !(Stack Number) !(Stack Number) -> (Stack Number)
setExclusion lhs rhs = abort "TBI"//removeDup ((filter (not o \el -> isMember el rhs) lhs) ++ (filter (not o \el -> isMember el lhs) rhs))


// special cases
complexSplit :: !Memory -> Memory
complexSplit memory=:{left, right, main=main`=:{stack=[!El mid`=:{stack=[!top:mid]}:other]}}
	= {memory&left=fromSingle (justReal top) + left,right=fromSingle (justImag top) + right,main={main`&stack=[!El {mid`&stack=mid}:other]}}
complexSplit memory = memory

matrixProduct :: !Memory -> Memory // returns multiple
matrixProduct _ = abort "TBI"/*
matrixProduct memory=:{cursor, delims, left, right, main} = abort "TBI"// let
//		matrix = [El [lhs * rhs \\ rhs <- right] \\ lhs <- left]
//	in {memory&cursor=delims,delims=inc delims,main=matrix++[Delim delims:memory.main]}
*/
joinWithNewlines :: !Memory -> Memory
joinWithNewlines _ = abort "TBI"

stacksFromCursor :: !Memory -> Memory
stacksFromCursor memory=:{cursor,main=main`=:{stack=[!El mid`:other]}} = let
		(base, _) = splitWhen (DELIM_FUNC False ((==)cursor)) memory.main
		stacks = occurrences (IS_ELEM) base
	in {memory&main={main`&stack=[!El (fromSingle (fromInt stacks) + mid`):other]}}

transposeFromCursor :: !Memory -> Memory
transposeFromCursor _ = abort "TBI"/*
transposeFromCursor memory=:{cursor,main}
	# (base, other) = span (DELIM_FUNC True ((<>)cursor)) main
	= let
		safeBase = [el \\ (El el) <- base]
		transposed = [(El el) \\ el <- transpose safeBase]
	in {memory&main=transposed ++ other}
*/
stackJoin :: !Memory -> Memory
stackJoin memory=:{cursor,main}
	# (base, other) = splitWhen (DELIM_FUNC False ((==)cursor)) main
	= let
		grouped = groupBy (\a b -> IS_DELIM a == IS_DELIM b) (toList base)
		flattened = [!El (fromList (flatten [toList el \\ (El el) <- part]) (all (\e -> case e of (El el) = el.bounded; _ = False) part)) \\ part <- grouped | case part of [Delim _] = False; _ = True]
	in {memory&main=fromStrictList flattened base.bounded + other}

stackUnjoin :: !Memory -> Memory
stackUnjoin memory=:{cursor,delims,main=main`=:{stack=[!El mid`:other]}} = let
		singles = fromStrictList [!El (fromSingle el) \\ el <- toList mid`] mid`.bounded
	in mergeDelims {memory&cursor=delims,delims=inc delims,main=(if(case singles.stack of [!] = True; _ = False) (fromSingle (El zero)) singles) + (fromSingle (Delim delims)) + (fromStrictList other main`.bounded)}

removeDupBase :: !Memory -> Memory
removeDupBase _ = abort "TBI"/*
removeDupBase memory=:{cursor,main}
	# (base, other) = span (DELIM_FUNC True ((<>)cursor)) main
	= let 
		safeBase = [el \\ (El el) <- base]
		deduped = [El el \\ el <- removeDup safeBase]
	in {memory&main=deduped ++ other}
*/
repeatTopMiddle :: !Memory -> Memory
repeatTopMiddle memory=:{delims,main=main`=:{stack=[!El mid`=:{stack=[!top:mid]}:other]}}
	= {memory&delims=inc delims,main={main`&stack=[!El (fromList (repeat top) False),Delim delims,El (fromStrictList mid mid`.bounded):other]}}
repeatTopMiddle memory = memory
	
repeatFullMiddle :: !Memory -> Memory
repeatFullMiddle _ = abort "TBI"/*
repeatFullMiddle memory=:{cursor, delims, main=[El mid:other]} // handle the infinite-ness
	# memory = mergeDelims {memory&cursor=delims,delims=inc delims,main=[El mid,Delim delims:other]}
	= {memory&main=(repeat (El mid))++memory.main}
*/
sortBaseline :: !Memory -> Memory
sortBaseline _ = abort "TBI"/*
sortBaseline memory=:{cursor,main}
	# (base, other) = span (DELIM_FUNC True ((<>)cursor)) main
	= let
		safeBase = [el \\ (El el) <- base]
		sorted = [El el \\ el <- sort safeBase]
	in {memory&main=sorted ++ other}
	
// stack manipulations
*/
stackReverse :: !StackID !Memory -> Memory
stackReverse Left memory=:{left}
	= {memory&left=reversed left}
stackReverse Right memory=:{right}
	= {memory&right=reversed right}
stackReverse Middle memory=:{main=main`=:{stack=[!El mid`:other]}}
	= {memory&main={main`&stack=[!El (reversed mid`):other]}}
stackReverse Both memory=:{left, right}
	= {memory&left=reversed left, right=reversed right}
stackReverse Primary memory=:{cursor,main}
	# (base, other) = splitWhen (DELIM_FUNC False ((==)cursor)) main
	= {memory&main=forEach (APPLY_ELEM reversed) base + other}
stackReverse Base memory=:{cursor,main}
	# (base, other) = splitWhen (DELIM_FUNC False ((==)cursor)) main
	= mergeDelims {memory&main=reversed base + other}
		

stackRotate :: !StackID !Memory -> Memory
stackRotate _ memory=:{main={stack=[!El {stack=[!]}:_]}} = memory
stackRotate Left memory=:{left, main=main`=:{stack=[!El mid`=:{stack=[!top:mid]}:other]}}
	= {memory&left=rotated (toInt top) left,main={main`&stack=[!El {mid`&stack=mid}:other]}}
stackRotate Right memory=:{right, main=main`=:{stack=[!El mid`=:{stack=[!top:mid]}:other]}}
	= {memory&right=rotated (toInt top) right,main={main`&stack=[!El {mid`&stack=mid}:other]}}
stackRotate Both memory=:{left, right, main=main`=:{stack=[!El mid`=:{stack=[!top:mid]}:other]}}
	= let rotate = rotated (toInt top)
	in {memory&left=rotate left,right=rotate right,main={main`&stack=[!El {mid`&stack=mid}:other]}}
stackRotate Middle memory=:{main=main`=:{stack=[!El mid`:other]}}
	= let rotate = rotated (toInt (headOf mid`))
	in {memory&main={main`&stack=[!El (rotate (tailOf mid`)):other]}}
stackRotate Primary memory=:{cursor, main=main`=:{stack=[!El mid`=:{stack=[!top:_]}:other]}}
	# (base, other) = splitWhen (DELIM_FUNC False ((==)cursor)) {main`&stack=[!El (tailOf mid`):other]}
	= let rotate = rotated (toInt top)
	in {memory&main=forEach (APPLY_ELEM rotate) base + other}
stackRotate Base memory=:{cursor,main=main`=:{stack=[!El mid`=:{stack=[!top:_]}:other]}}
	# (base, other) = splitWhen (DELIM_FUNC False ((==)cursor)) {main`&stack=[!El (tailOf mid`):other]}
	= let rotate = rotated (toInt top)
	in mergeDelims {memory&main=rotate base + other}


stackDelete :: !StackID !Memory -> Memory
stackDelete Left memory = {memory&left=zero}
stackDelete Right memory = {memory&right=zero}
stackDelete Middle memory=:{main}
	= {memory&main=tailOf main}
stackDelete Both memory = {memory&left=zero,right=zero}
stackDelete Base memory=:{cursor,main}
	# (base, other) = splitWhen (DELIM_FUNC False ((==)cursor)) main
	= mergeDelims {memory&main=other}
stackDelete Main memory = {memory&main=zero}
stackDelete All memory = {memory&left=zero,right=zero,main=zero}


stackDrop :: !StackID !Memory -> Memory
stackDrop _ _ = abort "TBI"/*
stackDrop _ memory=:{main=[El []:_]} = memory
stackDrop Left memory=:{left, main=[El [top:mid]:other]} = let
		val = toInt top
		fn = if(val<0) (take(~val)) (drop val)
	in {memory&left=fn left,main=[El mid:other]}
stackDrop Right memory=:{right, main=[El [top:mid]:other]} = let
		val = toInt top
		fn = if(val<0) (take(~val)) (drop val)
	in {memory&right=fn right,main=[El mid:other]}
stackDrop Middle memory=:{main=[El [top:mid]:other]} = let
		val = toInt top
		fn = if(val<0) (take(~val)) (drop val)
	in {memory&main=[El(fn mid):other]}
stackDrop Both memory=:{left, right, main=[El [top:mid]:other]} = let
		val = toInt top
		fn = if(val<0) (take(~val)) (drop val)
	in {memory&left=fn left,right=fn right,main=[El mid:other]}
stackDrop Base memory=:{cursor,main=[El [top:mid]:other]}
	# (base, other) = span (DELIM_FUNC True ((<>)cursor)) [El mid:other]
	= let
		val = toInt top
		fn = if(val<0) (take(~val)) (drop val)
	in mergeDelims {memory&main=fn base ++ other}
	
*/
cycleTops :: !Rotation !Memory -> Memory
cycleTops _ _ = abort "TBI"/*
cycleTops Clockwise memory=:{left, main=[El mid:other], right}
	= {memory&left=SAFE_HEAD right++SAFE_TAIL left,right=SAFE_HEAD mid++SAFE_TAIL right,main=[El(SAFE_HEAD left++SAFE_TAIL mid):other]}
cycleTops Anticlockwise memory=:{left, main=[El mid:other], right}
	= {memory&left=SAFE_HEAD mid++SAFE_TAIL left,right=SAFE_HEAD left++SAFE_TAIL right,main=[El(SAFE_HEAD right++SAFE_TAIL mid):other]}
	
*/
cycleStacks :: !Rotation !Memory -> Memory
cycleStacks _ _ = abort "TBI"/*
cycleStacks Clockwise memory=:{left, main=[El mid:other], right}
	= {memory&left=right,right=mid,main=[El left:other]}
cycleStacks Anticlockwise memory=:{left, main=[El mid:other], right}
	= {memory&left=mid,right=left,main=[El right:other]}
	
*/
unpackLeftRight :: !Memory -> Memory
unpackLeftRight _ = abort "TBI"/*
unpackLeftRight memory=:{left, main=[El[lhs,rhs:mid]:other], right}
	= {memory&left=[lhs:left],right=[rhs:right],main=[El mid:other]}
unpackLeftRight memory=:{left, main=[El[lhs]:other]}
	= {memory&left=[lhs:left],main=[El []:other]}
unpackLeftRight memory = memory

*/
unpackRightLeft :: !Memory -> Memory
unpackRightLeft _ = abort "TBI"/*
unpackRightLeft memory=:{left, main=[El[rhs,lhs:mid]:other], right}
	= {memory&left=[lhs:left],right=[rhs:right],main=[El mid:other]}
unpackRightLeft memory=:{right, main=[El[rhs]:other]}
	= {memory&right=[rhs:right],main=[El []:other]}
unpackRightLeft memory = memory

*/
swapLeftRight :: !Memory -> Memory
swapLeftRight _ = abort "TBI"/*
swapLeftRight memory=:{left, right}
	= {memory&left=right,right=left}
	
*/
swapTop :: !Axes !Memory -> Memory
swapTop _ _ = abort "TBI"/*
swapTop Horizontal memory=:{left, right}
	= {memory&left=SAFE_HEAD right++SAFE_TAIL left,right=SAFE_HEAD left++SAFE_TAIL right}
swapTop Vertical memory=:{main=[El [top:mid=:[_:_]]:other]}
	= {memory&main=[El ([last mid:init mid]++[top]):other]}
swapTop Identity memory=:{main=[El mid:other], right}
	= {memory&right=SAFE_HEAD mid++SAFE_TAIL right,main=[El(SAFE_HEAD right++SAFE_TAIL mid):other]}
swapTop Inverse memory=:{left, main=[El mid:other]}
	= {memory&left=SAFE_HEAD mid++SAFE_TAIL left,main=[El(SAFE_HEAD left++SAFE_TAIL mid):other]}
swapTop _ memory = memory	

*/
moveTop :: !Direction !Memory -> Memory
moveTop _ _ = abort "TBI"/*
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
	
*/
copyTop :: !Direction !Memory -> Memory
copyTop _ _ = abort "TBI"/*
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
	
*/
copyBoth :: !Axes !Memory -> Memory
copyBoth _ _ = abort "TBI"/*
copyBoth Horizontal memory=:{left=[lhs:_], right=[rhs:_]}
	= {memory&left=[rhs:memory.left],right=[lhs:memory.right]}
copyBoth Vertical memory=:{main=[El (mid=:[_:_]):other]}
	= {memory&main=[El([last mid:mid]++[hd mid]):other]}
copyBoth _ memory = memory
*/
moveAll :: !Direction !Memory -> Memory
moveAll NorthWest memory=:{left, main=main`=:{stack=[!El mid`:other]}}
	= {memory&left=mid` + left,main={main`&stack=other}}
moveAll NorthEast memory=:{right, main=main`=:{stack=[!El mid`:other]}}
	= {memory&right=mid` + right,main={main`&stack=other}}
moveAll SouthWest memory=:{delims, right, main}
	= {memory&delims=inc delims,right=zero,main=fromStrictList [!El right,Delim delims] True + main}
moveAll SouthEast memory=:{delims, left, main}
	= {memory&delims=inc delims,left=zero,main=fromStrictList [!El left, Delim delims] True + main}

replicateBase :: !Memory -> Memory
replicateBase _ = abort "TBI"/*
replicateBase memory=:{cursor,main}
	# (base, other) = span (DELIM_FUNC True ((<>)cursor)) main
	= mergeDelims {memory&cursor= -1,main=base++[Delim -1: main]} // do not touch, this is magic
	
*/
replicateMiddle :: !Memory -> Memory
replicateMiddle _ = abort "TBI"/*
replicateMiddle memory=:{delims,main=[El mid:other]}
	= {memory&delims=inc delims,main=[El mid,Delim delims,El mid:other]}
	
*/
replicateTop :: !Memory -> Memory
replicateTop _ = abort "TBI"/*
replicateTop memory=:{delims,main=[El mid:other]}
	= {memory&delims=inc delims,main=[El(SAFE_HEAD mid),Delim delims,El mid:other]}
	
*/
dupesBase :: !Memory -> Memory
dupesBase _ = abort "TBI"/*
dupesBase memory=:{cursor,main}
	# (base, other) = span (DELIM_FUNC True ((<>)cursor)) main
	= let
		safeBase = [el \\ (El el) <- base]
		deduplicated = [El el \\ el <- safeBase | sum [1 \\ e <- safeBase | e == el] > 1]
	in {memory&main=deduplicated ++ other}
	
*/
shiftCursorDownwards :: !Memory -> Memory
shiftCursorDownwards memory=:{cursor=0, delims} = {memory&cursor=dec delims}
shiftCursorDownwards memory=:{cursor} = {memory&cursor=dec cursor}

shiftCursorUpwards :: !Memory -> Memory
shiftCursorUpwards memory=:{cursor, delims}
	| inc cursor == delims
		= {memory&cursor=0}
	| otherwise
		= {memory&cursor=inc cursor}

moveCursorForwards :: !Memory -> Memory
moveCursorForwards _ = abort "TBI"/*
moveCursorForwards memory=:{delims,cursor,main}
	# (base, [cur:other]) = span (DELIM_FUNC True ((<>)cursor)) main
	| isEmpty other
		= mergeDelims {memory&cursor= -1,main=init base ++ [Delim -1, last base, Delim 0]}
		//= mergeDelims {memory&cursor=1,delims=inc delims,main=(map (APPLY_DELIM inc) (init base)) ++ [Delim -1, last base, Delim 0]}
	| otherwise
		= mergeDelims {memory&main=(init base ++ [cur, last base:other])}
	
*/
moveCursorBackwards :: !Memory -> Memory
moveCursorBackwards _ = abort "TBI"/*
moveCursorBackwards memory=:{delims,cursor,main}
	# (base, [cur:other]) = span (DELIM_FUNC True ((<>)cursor)) main
	| isEmpty other
		= mergeDelims {memory&cursor= -1,main=[hd main,Delim -1:tl main]}
		//= mergeDelims {memory&cursor=delims,delims=inc delims,main=[hd main,Delim delims:tl main]}
	| otherwise
		= mergeDelims {memory&main=(base ++ [hd other, cur:tl other])}

*/
remember :: !Memory -> Memory
remember memory=:{main=main`=:{stack=[!El mid`=:{stack=[!top:mid]}:other]}}
	= {memory&main={main`&stack=[!El {mid`&stack=mid}:other]},note=top}

recall :: !Memory -> Memory
recall memory=:{main=main`=:{stack=[!El mid`:other]}, note}
	= {memory&main={main`&stack=[!El (fromSingle note + mid`):other]}}