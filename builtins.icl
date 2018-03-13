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
isElementOf lhs rhs = fromBool (S_any ((==) lhs) rhs)//fromBool (isMember lhs rhs)
isImproperSubsetOf :: !(Stack Number) !(Stack Number) -> Number
isImproperSubsetOf lhs rhs = fromBool (S_all (\e -> S_occurrences ((==) e) lhs <= S_occurrences ((==) e) rhs) lhs)
//isImproperSubsetOf lhs rhs = fromBool (all (\e -> [0 \\ i <- lhs | i == e] <= [0 \\ i <- rhs | i == e]) lhs)
isProperSubsetOf :: !(Stack Number) !(Stack Number) -> Number
isProperSubsetOf lhs rhs = fromBool (S_all (\e -> S_occurrences ((==) e) lhs <= S_occurrences ((==) e) rhs) lhs && S_any (\e -> S_occurrences ((==) e) lhs < S_occurrences ((==) e) rhs) lhs)
//isProperSubsetOf lhs rhs = fromBool (all (\e -> [0 \\ i <- lhs | i == e] <= [0 \\ i <- rhs | i == e]) lhs && any (\e -> [0 \\ i <- lhs | i == e] < [0 \\ i <- rhs | i == e]) lhs)
isNotSubsetOf :: !(Stack Number) !(Stack Number) -> Number
isNotSubsetOf lhs rhs = fromBool (S_any (\e -> S_occurrences ((==) e) lhs > S_occurrences ((==) e) rhs) lhs)
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
areAnyTrue arg = fromBool (S_any toBool arg)
areAllTrue :: !(Stack Number) -> Number
areAllTrue arg = fromBool (S_all toBool arg)

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
numProduct arg = S_reduce (*) one arg//foldl (*) one arg
numSum :: !(Stack Number) -> Number
numSum {bounded=False} = NaN
numSum arg = S_reduce (+) Zero arg//foldl (+) Zero arg

// vectorized ops
vectorPlus :: !(Stack Number) !(Stack Number) -> (Stack Number)
vectorPlus lhs rhs = S_zipWith (+) lhs rhs
vectorTimes :: !(Stack Number) !(Stack Number) -> (Stack Number)
vectorTimes lhs rhs = S_zipWith (*) lhs rhs
vectorNegate :: !(Stack Number) -> (Stack Number)
vectorNegate arg = S_map (~) arg
vectorAND :: !(Stack Number) !(Stack Number) -> (Stack Number)
vectorAND lhs rhs = S_zipWith bitAND lhs rhs
vectorOR :: !(Stack Number) !(Stack Number) -> (Stack Number)
vectorOR lhs rhs = S_zipWith bitOR lhs rhs
vectorIsEqual :: !(Stack Number) !(Stack Number) -> (Stack Number)
vectorIsEqual lhs rhs = S_zipWith isEqualTo lhs rhs
vectorElementOf :: !(Stack Number) !(Stack Number) -> (Stack Number)
vectorElementOf lhs rhs = S_map (\e -> isElementOf e rhs) lhs
vectorLessThan :: !(Stack Number) !(Stack Number) -> (Stack Number)
vectorLessThan lhs rhs = S_zipWith isLessThan lhs rhs
vectorGreaterThan :: !(Stack Number) !(Stack Number) -> (Stack Number)
vectorGreaterThan lhs rhs = S_zipWith isGreaterThan lhs rhs
vectorLessOrEqual :: !(Stack Number) !(Stack Number) -> (Stack Number)
vectorLessOrEqual lhs rhs = S_zipWith isLessOrEqual lhs rhs
vectorGreaterOrEqual :: !(Stack Number) !(Stack Number) -> (Stack Number)
vectorGreaterOrEqual lhs rhs = S_zipWith isGreaterOrEqual lhs rhs

// miscelaneous operators
toUppercase :: !Number -> Number
toUppercase arg = fromInt (toUpperUChar (toInt arg))
toLowercase :: !Number -> Number
toLowercase arg = fromInt (toLowerUChar (toInt arg))
splitOnNewlines :: !(Stack Number) -> (Stack Element)
splitOnNewlines arg
	# (head, tail) = S_span (\e -> toInt e == 10) arg
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
	#! unit = if(IS_IMAG arg) imagUnit id one
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
	#! unit = if(IS_IMAG arg) imagUnit id one
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
setMinimum arg = S_reduce (min) (headOf arg) (tailOf arg)
setMaximum :: !(Stack Number) -> Number
setMaximum {stack=[!]} = NaN
setMaximum arg = S_reduce (max) (headOf arg) (tailOf arg)
setFilter :: !(Stack Number) !(Stack Number) -> (Stack Number)
setFilter lhs rhs = S_filterOn (toBool) lhs rhs
antiFilter :: !(Stack Number) !(Stack Number) -> (Stack Number)
antiFilter lhs rhs = S_filterOn (not o toBool) lhs rhs// [el \\ el <- lhs.stack & cond <- rhs.stack | (not o toBool) cond]
dupesMiddle :: !(Stack Number) -> (Stack Number)
dupesMiddle arg = S_filterBy (\e -> S_occurrences ((==) e) arg > 1) arg//[el \\ el <- arg | sum [1 \\ e <- arg | e == el] > 1]
groupMiddle :: !(Stack Number) -> (Stack Element)
groupMiddle arg=:{bounded}
	# list = toList arg
	# groups = map (\e -> (El (fromList e False))) (group list) // TODO: all complete groups are bounded, find a way to implement that
	= fromList groups bounded
setIntersection :: !(Stack Number) !(Stack Number) -> (Stack Number)
setIntersection lhs rhs = S_uniques (S_filterBy (\e -> S_any ((==) e) rhs) lhs)//removeDup (filter (\el -> isMember el rhs) lhs)
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
		(base, _) = S_span (DELIM_FUNC False ((==)cursor)) memory.main
		stacks = S_occurrences (IS_ELEM) base
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
	# (base, other) = S_span (DELIM_FUNC False ((==)cursor)) main
	= let
		grouped = groupBy (\a b -> IS_DELIM a == IS_DELIM b) (toList base)
		filtered = filter (all IS_ELEM) grouped
		joined = map (foldl (\(El a) (El b) -> (El (b + a))) (El zero)) filtered
	in {memory&main=fromList joined base.bounded + other}

stackUnjoin :: !Memory -> Memory
stackUnjoin memory=:{cursor, delims, main}
	# (El mid, other) = decons main
	# singles = case mid.stack of
		[!] = fromSingle (El zero)
		_ = S_map (\el -> (El (fromSingle el))) mid
	= {memory&cursor=delims,delims=inc delims,main=singles + recons (Delim delims, other)}
	
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
	= {memory&left=S_reverse left}
stackReverse Right memory=:{right}
	= {memory&right=S_reverse right}
stackReverse Middle memory=:{main=main`=:{stack=[!El mid`:other]}}
	= {memory&main={main`&stack=[!El (S_reverse mid`):other]}}
stackReverse Both memory=:{left, right}
	= {memory&left=S_reverse left, right=S_reverse right}
stackReverse Primary memory=:{cursor,main}
	# (base, other) = S_span (DELIM_FUNC False ((==)cursor)) main
	= {memory&main=S_map (APPLY_ELEM S_reverse) base + other}
stackReverse Base memory=:{cursor,main}
	# (base, other) = S_span (DELIM_FUNC False ((==)cursor)) main
	= mergeDelims {memory&main=S_reverse base + other}

stackRotate :: !StackID !Memory -> Memory
stackRotate _ memory=:{main={stack=[!El {stack=[!]}:_]}} = memory
stackRotate Left memory=:{left, main=main`=:{stack=[!El mid`=:{stack=[!top:mid]}:other]}}
	= {memory&left=S_rotate (toInt top) left,main={main`&stack=[!El {mid`&stack=mid}:other]}}
stackRotate Right memory=:{right, main=main`=:{stack=[!El mid`=:{stack=[!top:mid]}:other]}}
	= {memory&right=S_rotate (toInt top) right,main={main`&stack=[!El {mid`&stack=mid}:other]}}
stackRotate Both memory=:{left, right, main=main`=:{stack=[!El mid`=:{stack=[!top:mid]}:other]}}
	= let rotate = S_rotate (toInt top)
	in {memory&left=rotate left,right=rotate right,main={main`&stack=[!El {mid`&stack=mid}:other]}}
stackRotate Middle memory=:{main=main`=:{stack=[!El mid`:other]}}
	= let rotate = S_rotate (toInt (headOf mid`))
	in {memory&main={main`&stack=[!El (rotate (tailOf mid`)):other]}}
stackRotate Primary memory=:{cursor, main=main`=:{stack=[!El mid`=:{stack=[!top:_]}:other]}}
	# (base, other) = S_span (DELIM_FUNC False ((==)cursor)) {main`&stack=[!El (tailOf mid`):other]}
	= let rotate = S_rotate (toInt top)
	in {memory&main=S_map (APPLY_ELEM rotate) base + other}
stackRotate Base memory=:{cursor,main=main`=:{stack=[!El mid`=:{stack=[!top:_]}:other]}}
	# (base, other) = S_span (DELIM_FUNC False ((==)cursor)) {main`&stack=[!El (tailOf mid`):other]}
	= let rotate = S_rotate (toInt top)
	in mergeDelims {memory&main=rotate base + other}


stackDelete :: !StackID !Memory -> Memory
stackDelete Left memory = {memory&left=zero}
stackDelete Right memory = {memory&right=zero}
stackDelete Middle memory=:{main}
	= {memory&main=tailOf main}
stackDelete Both memory = {memory&left=zero,right=zero}
stackDelete Base memory=:{cursor,main}
	# (base, other) = S_span (DELIM_FUNC False ((==)cursor)) main
	= mergeDelims {memory&main=other}
stackDelete Main memory = {memory&main=zero}
stackDelete All memory = {memory&left=zero,right=zero,main=zero}

stackDrop :: !StackID !Memory -> Memory
stackDrop _ memory=:{main={stack=[!El {stack=[!]}:_]}} = memory
stackDrop Left memory=:{left, main=main`=:{stack=[!El mid`=:{stack=[!top:mid]}:other]}} = let
		val = toInt top
		fn = if(val<0) (S_take(~val)) (S_drop val)
	in {memory&left=fn left,main={main`&stack=[!El {mid`&stack=mid}:other]}}
stackDrop Right memory=:{right, main=main`=:{stack=[!El mid`=:{stack=[!top:mid]}:other]}}  = let
		val = toInt top
		fn = if(val<0) (S_take(~val)) (S_drop val)
	in {memory&right=fn right,main={main`&stack=[!El {mid`&stack=mid}:other]}}
stackDrop Middle memory=:{main=main`=:{stack=[!El mid`=:{stack=[!top:mid]}:other]}} = let
		val = toInt top
		fn = if(val<0) (S_take(~val)) (S_drop val)
	in {memory&main={main`&stack=[!El (fn {mid`&stack=mid}):other]}}
stackDrop Both memory=:{left, right, main=main`=:{stack=[!El mid`=:{stack=[!top:mid]}:other]}} = let
		val = toInt top
		fn = if(val<0) (S_take(~val)) (S_drop val)
	in {memory&left=fn left,right=fn right,main={main`&stack=[!El {mid`&stack=mid}:other]}}
stackDrop Base memory=:{cursor, main=main`=:{stack=[!El mid`=:{stack=[!top:mid]}:other]}}
	# (base, other) = S_span (DELIM_FUNC False ((==)cursor)) {main`&stack=[!El {mid`&stack=mid}:other]}
	= let
		val = toInt top
		fn = if(val<0) (S_take(~val)) (S_drop val)
	in mergeDelims {memory&main=fn base + other}
	
cycleTops :: !Rotation !Memory -> Memory
//cycleTops Clockwise memory=:{left, main=[El mid:other], right}
//	= {memory&left=SAFE_HEAD right++SAFE_TAIL left,right=SAFE_HEAD mid++SAFE_TAIL right,main=[El(SAFE_HEAD left++SAFE_TAIL mid):other]}
cycleTops Anticlockwise memory=:{left, main=main`=:{stack=[!El mid`:other]}, right}
	= {memory&left=safeHead mid` + safeTail left,right=safeHead left + safeTail right,main={main`&stack=[!El (safeHead right + safeTail mid`):other]}}

cycleStacks :: !Rotation !Memory -> Memory
//cycleStacks Clockwise memory=:{left, main=[El mid:other], right}
//	= {memory&left=right,right=mid,main=[El left:other]}
cycleStacks Anticlockwise memory=:{left, main=main`=:{stack=[!El mid`:other]}, right}
	= {memory&left=mid`,right=left,main={main`&stack=[!El right:other]}}

unpackLeftRight :: !Memory -> Memory
unpackLeftRight memory=:{left, main=main`=:{stack=[!El mid`=:{stack=[!lhs,rhs:mid]}:other]}, right}
	= {memory&left=fromSingle lhs + left,right=fromSingle rhs + right,main={main`&stack=[!El {mid`&stack=mid}:other]}}
unpackLeftRight memory=:{left, main=main`=:{stack=[!El {stack=[!lhs]}:other]}}
	= {memory&left=fromSingle lhs + left,main={main`&stack=[!El zero:other]}}
unpackLeftRight memory = memory

unpackRightLeft :: !Memory -> Memory
unpackRightLeft memory=:{left, main=main`=:{stack=[!El mid`=:{stack=[!rhs,lhs:mid]}:other]}, right}
	= {memory&left=fromSingle lhs + left,right=fromSingle rhs + right,main={main`&stack=[!El {mid`&stack=mid}:other]}}
unpackRightLeft memory=:{right, main=main`=:{stack=[!El {stack=[!rhs]}:other]}}
	= {memory&right=fromSingle rhs + right,main={main`&stack=[!El zero:other]}}
unpackRightLeft memory = memory

swapLeftRight :: !Memory -> Memory
swapLeftRight memory=:{left, right}
	= {memory&left=right,right=left}

swapTop :: !Axes !Memory -> Memory
swapTop Horizontal memory=:{left, right}
	= {memory&left=safeHead right + safeTail left,right=safeHead left + safeTail right}
swapTop Vertical memory=:{main=main`=:{stack=[!El mid`:other]}}
	= {memory&main={main`&stack=[!El (safeLast mid` + safeInit mid`):other]}}
swapTop Identity memory=:{main=main`=:{stack=[!El mid`:other]}, right}
	= {memory&right=safeHead mid` + safeTail right,main={main`&stack=[!El(safeHead right + safeTail mid`):other]}}
swapTop Inverse memory=:{main=main`=:{stack=[!El mid`:other]}, left}
	= {memory&left=safeHead mid` + safeTail left,main={main`&stack=[!El(safeHead left + safeTail mid`):other]}}
swapTop _ memory = memory	

moveTop :: !Direction !Memory -> Memory
moveTop East memory=:{left, right}
	= {memory&left=safeTail left,right=safeHead left + right}
moveTop West memory=:{left, right}
	= {memory&left=safeHead right + left,right=safeTail right}
moveTop South memory=:{main={stack=[!El {stack=[!_,_:_]}:_]}}
	# (El mid, other) = decons memory.main
	# (top, next, mid) = decon2 mid
	= {memory&main=recons (El (recon2 (next, top, mid)), other)}
moveTop NorthWest memory=:{main={stack=[!El {stack=[!_:_]}:_]}}
	# (El mid, other) = decons memory.main
	# (top, mid) = decons mid
	= {memory&left=recons (top, memory.left),main=recons (El mid, other)}
moveTop NorthEast memory=:{main={stack=[!El {stack=[!_:_]}:_]}}
	# (El mid, other) = decons memory.main
	# (top, mid) = decons mid
	= {memory&right=recons (top, memory.right),main=recons (El mid, other)}
moveTop SouthWest memory=:{right={stack=[!_:_]}, main={stack=[!El _:_]}}
	# (El mid, other) = decons memory.main
	# (top, right) = decons memory.right
	= {memory&right=right,main=recons (El (recons (top, mid)), other)}
moveTop SouthEast memory=:{left={stack=[!_:_]}, main={stack=[!El _:_]}}
	# (El mid, other) = decons memory.main
	# (top, left) = decons memory.left
	= {memory&left=left,main=recons (El (recons (top, mid)), other)}
moveTop _ memory = memory

copyTop :: !Direction !Memory -> Memory
copyTop East memory
	= {memory&left=safeHead memory.right + memory.left}
copyTop West memory
	= {memory&right=safeHead memory.left + memory.right}
copyTop North memory=:{main={stack=[!El {stack=[!_:_]}:_]}}
	# (El mid, other) = decons memory.main
	# (top, mid) = decons mid
	= {memory&main=recons (El (recon2 (top, top, mid)), other)}
copyTop NorthWest memory=:{main={stack=[!El {stack=[!top:_]}:_]}}
	= {memory&left=recons (top, memory.left)}
copyTop NorthEast memory=:{main={stack=[!El {stack=[!top:_]}:_]}}
	= {memory&right=recons (top, memory.right)}
copyTop SouthWest memory
	# (El mid, other) = decons memory.main
	= {memory&main=recons (El (safeHead memory.right + mid), other)}
copyTop SouthEast memory
	# (El mid, other) = decons memory.main
	= {memory&main=recons (El (safeHead memory.left + mid), other)}
copyTop _ memory = memory

copyBoth :: !Axes !Memory -> Memory
copyBoth Horizontal memory
	= {memory&left=safeHead memory.right + memory.left,right=safeHead memory.left + memory.right}
copyBoth Vertical memory=:{main={stack=[!El _:_]}}
	# (El mid, other) = decons memory.main
	= {memory&main=recons (El (safeLast mid + mid + safeHead mid), other)}
copyBoth _ memory = memory

moveAll :: !Direction !Memory -> Memory
moveAll NorthWest memory=:{left, main}
	# (El mid, other) = decons main
	= {memory&left=mid + left,main=other}
moveAll NorthEast memory=:{right, main}
	# (El mid, other) = decons main
	= {memory&right=mid + right,main=other}
moveAll SouthWest memory=:{delims, right, main}
	= {memory&delims=inc delims,right=zero,main=recon2 (El right, Delim delims, main)}
moveAll SouthEast memory=:{delims, left, main}
	= {memory&delims=inc delims,left=zero,main=recon2 (El left, Delim delims, main)}

replicateBase :: !Memory -> Memory
replicateBase memory=:{cursor,main}
	# (base, _) = S_span (DELIM_FUNC False ((==)cursor)) main
	= mergeDelims {memory&cursor= -1,main=base + recons (Delim -1, main)} // do not touch, this is magic

replicateMiddle :: !Memory -> Memory
replicateMiddle memory=:{delims}
	= {memory&delims=inc delims,main=recon2 (headOf memory.main, Delim delims, memory.main)}

replicateTop :: !Memory -> Memory
replicateTop memory=:{delims,main={stack=[!El {stack=[!top:_]}:_]}}
	= {memory&delims=inc delims,main=recon2 (El (fromSingle top), Delim delims, memory.main)}
replicateTop memory = memory

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
moveCursorForwards memory=:{delims,cursor,main}
	# (base, other) = S_span (DELIM_FUNC False ((==)cursor)) main
	# (cur, other) = decons other
	= mergeDelims case other.stack of
		[!]	= {memory&cursor= -1,main=initOf base + fromStrictList [!Delim -1, lastOf base, Delim 0] True}
		_ = {memory&main=initOf base + recon2 (cur, lastOf base, other)}

moveCursorBackwards :: !Memory -> Memory
moveCursorBackwards memory=:{delims,cursor,main}
	# (base, other) = S_span (DELIM_FUNC False ((==)cursor)) main
	# (cur, other) = decons other
	= mergeDelims case other.stack of
		[!] = {memory&cursor= -1,main=recon2 (headOf main, Delim -1, tailOf main)}
		_ = {memory&main=base + recon2 (headOf other, cur, tailOf other)}


remember :: !Memory -> Memory
remember memory=:{main=main`=:{stack=[!El mid`=:{stack=[!top:mid]}:other]}}
	= {memory&main={main`&stack=[!El {mid`&stack=mid}:other]},note=top}

recall :: !Memory -> Memory
recall memory=:{main=main`=:{stack=[!El mid`:other]}, note}
	= {memory&main={main`&stack=[!El (fromSingle note + mid`):other]}}