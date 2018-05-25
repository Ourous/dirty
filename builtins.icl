implementation module builtins

import types, atomics, arithmetic, utilities, StdEnv, StdLib, unicode, stacks, Data.List

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
isIdentical :: !(MStack Number) !(MStack Number) -> Number
isIdentical lhs rhs = fromBool (lhs == rhs)

isElementOf :: !Number !(MStack Number) -> Number
isElementOf _ Nothing = NUM_FALSE
isElementOf _ (Just {finite=False}) = NUM_TRUE
isElementOf lhs (Just {head,init,tail}) = fromBool (lhs == head || (IsMember lhs init) || (IsMember lhs tail))//fromBool (S_any ((==) lhs) rhs)//fromBool (isMember lhs rhs)
isImproperSubsetOf :: !(MStack Number) !(MStack Number) -> Number
isImproperSubsetOf Nothing _ = NUM_TRUE
isImproperSubsetOf (Just {finite=True}) (Just {finite=False}) = NUM_TRUE
isImproperSubsetOf (Just {finite=False}) _ = NUM_FALSE
isImproperSubsetOf (Just lhs) (Just rhs) = fromBool (S_all (\e -> S_occurrences ((==) e) lhs <= S_occurrences ((==) e) rhs) lhs)
//isImproperSubsetOf lhs rhs = fromBool (all (\e -> [0 \\ i <- lhs | i == e] <= [0 \\ i <- rhs | i == e]) lhs)
isProperSubsetOf :: !(MStack Number) !(MStack Number) -> Number
isProperSubsetOf Nothing Nothing = NUM_FALSE
isProperSubsetOf Nothing _ = NUM_TRUE
isProperSubsetOf (Just {finite=True}) (Just {finite=False}) = NUM_TRUE
isProperSubsetOf (Just {finite=False}) _ = NUM_FALSE
isProperSubsetOf (Just lhs) (Just rhs) = fromBool (S_all (\e -> S_occurrences ((==) e) lhs <= S_occurrences ((==) e) rhs) lhs && S_any (\e -> S_occurrences ((==) e) lhs < S_occurrences ((==) e) rhs) lhs)
//isProperSubsetOf lhs rhs = fromBool (all (\e -> [0 \\ i <- lhs | i == e] <= [0 \\ i <- rhs | i == e]) lhs && any (\e -> [0 \\ i <- lhs | i == e] < [0 \\ i <- rhs | i == e]) lhs)
isNotSubsetOf :: !(MStack Number) !(MStack Number) -> Number
isNotSubsetOf Nothing _ = NUM_FALSE
isNotSubsetOf (Just {finite=True}) (Just {finite=False}) = NUM_FALSE
isNotSubsetOf (Just {finite=False}) _ = NUM_TRUE
isNotSubsetOf (Just lhs) (Just rhs) = fromBool (S_any (\e -> S_occurrences ((==) e) lhs > S_occurrences ((==) e) rhs) lhs)
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
		= NUM_FALSE
	| otherwise
		= fromBool (all (\e -> arg mod e <> Zero) [inc one..dec arg])

isSorted :: !(MStack Number) -> Number
isSorted Nothing = NUM_TRUE
isSorted (Just {finite=False}) = NUM_FALSE
isSorted (Just arg) = fromBool (isSorted` (toStrictList arg))
where
	isSorted` [!h1:tail=:[!h2:_]] = h1 <= h2 && isSorted` tail
	isSorted` _ = True
	
areAnyTrue :: !(MStack Number) -> Number
areAnyTrue Nothing = NUM_FALSE
areAnyTrue (Just arg) = fromBool (S_any toBool arg)
areAllTrue :: !(MStack Number) -> Number
areAllTrue Nothing = NUM_TRUE
areAllTrue (Just arg) = fromBool (S_all toBool arg)

// coalescing operators
logicEquiv :: !Number -> Number
logicEquiv arg = fromBool (toBool arg)
logicNegate :: !Number -> Number
logicNegate arg = fromBool (not (toBool arg))

// remaining math ops
primeFactors :: !Number -> (MStack Number)
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
numProduct :: !(MStack Number) -> Number
numProduct Nothing = Zero
numProduct (Just {finite=False}) = NaN
numProduct (Just arg) = S_collapse (*) one arg
numSum :: !(MStack Number) -> Number
numSum Nothing = Zero
numSum (Just {finite=False}) = NaN
numSum (Just arg) = S_collapse (+) zero arg//foldl (+) Zero arg
numAverage :: !(MStack Number) -> Number
numAverage Nothing = Zero
numAverage (Just {finite=False}) = NaN
numAverage (Just arg) = let len = S_length arg in S_reduce (+) Zero (S_map (\e -> e / len) arg)
convToBase :: !Number !Number -> (MStack Number)
convToBase lhs rhs = convToBase` zero lhs
where
	convToBase` acc NaN = acc
	convToBase` acc Zero = acc
	convToBase` acc val
		#! digit = val mod rhs
		#! acc = recons (digit, acc)
		#! val = (val - digit) / rhs
		= convToBase` (Just acc) val
convFromBase :: !(MStack Number) !Number -> Number
convFromBase (Just {finite=False}) _ = NaN
convFromBase _ Zero = NaN
convFromBase _ NaN = NaN
convFromBase (Just lhs) rhs
	= convFromBase` (S_length lhs - one) (toStrictList lhs)
where
	convFromBase` _ [!] = Zero
	convFromBase` place [!l:lhs]
		#! val = l * rhs ^ place
		= val + convFromBase` (dec place) lhs

// vectorized ops
vectorPlus :: !(MStack Number) !(MStack Number) -> (MStack Number)
vectorPlus (Just lhs) (Just rhs) = Just (S_zipWith (+) lhs rhs)
vectorPlus _ _ = Nothing
vectorTimes :: !(MStack Number) !(MStack Number) -> (MStack Number)
vectorTimes (Just lhs) (Just rhs) = Just (S_zipWith (*) lhs rhs)
vectorTimes _ _ = Nothing
vectorNegate :: !(MStack Number) -> (MStack Number)
vectorNegate (Just arg) = Just (S_map (~) arg)
vectorNegate _ = Nothing
vectorAND :: !(MStack Number) !(MStack Number) -> (MStack Number)
vectorAND (Just lhs) (Just rhs) = Just (S_zipWith bitAND lhs rhs)
vectorAND _ _ = Nothing
vectorOR :: !(MStack Number) !(MStack Number) -> (MStack Number)
vectorOR (Just lhs) (Just rhs) = Just (S_zipWith bitOR lhs rhs)
vectorOR _ _ = Nothing
vectorIsEqual :: !(MStack Number) !(MStack Number) -> (MStack Number)
vectorIsEqual (Just lhs) (Just rhs) = Just (S_zipWith isEqualTo lhs rhs)
vectorIsEqual _ _ = Nothing
vectorElementOf :: !(MStack Number) !(MStack Number) -> (MStack Number)
vectorElementOf (Just lhs) rhs = Just (S_map (\e -> isElementOf e rhs) lhs)
vecotrElementOf _ _ = Nothing
vectorLessThan :: !(MStack Number) !(MStack Number) -> (MStack Number)
vectorLessThan (Just lhs) (Just rhs) = Just (S_zipWith isLessThan lhs rhs)
vectorLessThan _ _ = Nothing
vectorGreaterThan :: !(MStack Number) !(MStack Number) -> (MStack Number)
vectorGreaterThan (Just lhs) (Just rhs) = Just (S_zipWith isGreaterThan lhs rhs)
vectorGreaterThan _ _ = Nothing
vectorLessOrEqual :: !(MStack Number) !(MStack Number) -> (MStack Number)
vectorLessOrEqual (Just lhs) (Just rhs) = Just (S_zipWith isLessOrEqual lhs rhs)
vectorLessOrEqual _ _ = Nothing
vectorGreaterOrEqual :: !(MStack Number) !(MStack Number) -> (MStack Number)
vectorGreaterOrEqual (Just lhs) (Just rhs) = Just (S_zipWith isGreaterOrEqual lhs rhs)
vectorGreaterOrEqual _ _ = Nothing

// miscelaneous operators
toUppercase :: !Number -> Number
toUppercase arg = fromInt (toUpperUChar (toInt arg))
toLowercase :: !Number -> Number
toLowercase arg = fromInt (toLowerUChar (toInt arg))
splitOnNewlines :: !(MStack Number) -> (Stack (MStack Number))
splitOnNewlines Nothing = zero
splitOnNewlines (Just arg)
	# (head, tail) = S_span (\e -> toInt e <> 10) arg
	| isNothing tail
		= fromSingle (head)
	| otherwise
		= recons (head, Just (splitOnNewlines (tailOf (fromJust tail))))

// "set" operators
fromLeftStepRight :: !Number !Number -> (MStack Number)
fromLeftStepRight lhs rhs = Just {head=lhs,init=[!lhs+rhs,lhs+rhs+rhs..],tail=Repeat NaN,finite=False}
fromOneToMiddle :: !Number -> (MStack Number)
fromOneToMiddle arg
	| IS_CPLX arg
		= Just (fromSingle arg)
	#! unit = if(IS_IMAG arg) imagUnit id one
	| arg < Zero
		= Just (fromStrictList [!Zero - unit, Zero - unit - unit..arg] True)
	| arg > Zero
		= Just (fromStrictList [!unit, unit + unit..arg] True)
	| otherwise
		= Just (fromSingle arg)
fromMiddleToZero :: !Number -> (MStack Number)
fromMiddleToZero arg
	| IS_CPLX arg
		= Just (fromSingle arg)
	#! unit = if(IS_IMAG arg) imagUnit id one
	| arg < Zero
		= Just (fromStrictList [!arg, arg + unit..Zero] True)
	| arg > Zero
		= Just (fromStrictList [!arg, arg - unit..Zero] True)
	| otherwise
		= Just (fromSingle arg)
fromLeftTimesRight :: !Number !Number -> (MStack Number)
fromLeftTimesRight lhs rhs = Just {head=lhs,init=(yieldTimesRight (lhs*rhs)),tail=Repeat NaN,finite=False}
where yieldTimesRight arg = [!arg:yieldTimesRight(arg*rhs)]
setMinimum :: !(MStack Number) -> Number
setMinimum Nothing = NaN
setMinimum (Just arg) = S_collapse min arg.head arg
setMaximum :: !(MStack Number) -> Number
setMaximum Nothing = NaN
setMaximum (Just arg) = S_collapse max arg.head arg
setFilter :: !(MStack Number) !(MStack Number) -> (MStack Number)
setFilter (Just lhs) (Just rhs) = S_filterOn (toBool) lhs rhs
setFilter _ _ = Nothing
antiFilter :: !(MStack Number) !(MStack Number) -> (MStack Number)
antiFilter (Just lhs) (Just rhs) = S_filterOn (not o toBool) lhs rhs// [el \\ el <- lhs.stack & cond <- rhs.stack | (not o toBool) cond]
antiFilter _ _ = Nothing
dupesMiddle :: !(MStack Number) -> (MStack Number)
dupesMiddle Nothing = Nothing
dupesMiddle (Just arg) = S_filterBy (\e -> S_occurrences ((==) e) arg > 1) arg//[el \\ el <- arg | sum [1 \\ e <- arg | e == el] > 1]
groupMiddle :: !(MStack Number) -> (Stack (MStack Number))
groupMiddle Nothing = zero
groupMiddle (Just arg=:{finite})
	# list = toList arg
	# groups = map (\e -> (Just (fromList e False))) (group list) // TODO: all complete groups are finite, find a way to implement that
	= fromList groups finite
setIntersection :: !(MStack Number) !(MStack Number) -> (MStack Number)
setIntersection (Just lhs) (Just rhs) = mapMaybe S_uniques (S_filterBy (\e -> S_any ((==) e) rhs) lhs)
setIntersection _ _ = Nothing
	
setExclusion :: !(MStack Number) !(MStack Number) -> (MStack Number)
setExclusion (Just lhs) (Just rhs) = mapMaybe S_uniques ((S_filterBy (\el -> S_all ((<>)el) rhs) lhs) +++ (S_filterBy (\el -> S_all ((<>)el) lhs) rhs))
numContigSubsets :: !(MStack Number) !(MStack Number) -> Number
numContigSubsets Nothing Nothing = one
numContigSubsets _ Nothing = Zero
numContigSubsets (Just {finite=False}) (Just {finite=False}) = NaN
numContigSubsets (Just {finite=False}) _ = Zero
numContigSubsets _ (Just {finite=False}) = (Re (Inf Positive))
numContigSubsets Nothing (Just rhs) = S_length rhs
numContigSubsets (Just lhs) (Just rhs)
	= fromInt (numContig (toStrictList lhs) (toStrictList rhs))
where
	equateAll [!] _ = True
	equateAll _ [!] = False
	equateAll [!l:lhs] [!r:rhs] = l == r && equateAll lhs rhs
	numContig _ [!] = 0
	numContig lhs [!r:rhs]
		#! val = if(equateAll lhs [!r:rhs]) 1 0
		= val + numContig lhs rhs
splitContig :: !(MStack Number) !(MStack Number) -> (Stack (MStack Number))
splitContig (Just {finite=False}) rhs = fromSingle rhs
splitContig Nothing (Just rhs) = S_map (\e -> Just (fromSingle e)) rhs
splitContig (Just lhs) (Just rhs)
	= splitset {zero&finite=rhs.finite} zero (toStrictList lhs) (toStrictList rhs)
where
	equateAll [!] rhs = (True, rhs)
	equateAll _ [!] = (False, [!])
	equateAll [!l:lhs] [!r:rhs]
		# (eq, st) = equateAll lhs rhs
		| l == r && eq
			= (eq, st)
		| otherwise
			= (False, [!r:rhs])
	splitset acc head _ [!] = {acc&finite=True} +++  fromSingle (Just {head&finite=True})
	splitset acc head lhs [!r:rhs]
		# (eq, st) = equateAll lhs [!r:rhs]
		| eq
			= splitset (acc +++ fromSingle (Just {head&finite=True})) zero lhs st
		| otherwise
			= splitset acc (head +++ fromSingle r) lhs rhs
contigSubsets :: !(MStack Number) -> (Stack (MStack Number))
contigSubsets Nothing = fromSingle Nothing
contigSubsets (Just arg) // TODO this but less hacky
	# list = toList arg
	# subsets = [Just (fromList subset True) \\ subset <- subsequences list]
	= fromList subsets True

// special cases
complexSplit :: !Memory -> Memory
complexSplit memory=:{left, right, above={head={head=Just _}}}
	# (main, above) = decons memory.above
	# (Just mid, main) = decons main
	# (top, mid) = decons mid
	= {memory&left=Just (recons (justReal top, left)),right=Just (recons (justImag top, right)),above.head.head=mid}
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
stacksFromCursor memory=:{above}
	= {memory&above.head.head=Just (recons (stacks, above.head.head))}
where
	lengths :: (Stack  Number)
	lengths = S_map ((S_collapse (+) zero) o S_map (S_length o fallback)) above
	stacks :: Number
	stacks = S_collapse (+) zero lengths

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
stackJoin _ = abort "TODO: joining with proper partitioning"

stackUnjoin :: !Memory -> Memory
stackUnjoin memory=:{above}
	# main = S_map fromSingle above.head
	= {memory&above=main+++(fallback (tailOf above))}
	
	
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
repeatTopMiddle memory=:{above={head={head=Just mid}}}
	# (top, mid) = decons mid
	# rep = {head=top,init=Repeat top,tail=Repeat top,finite=False}
	# above = let main = memory.above.head in {memory.above&head={main&head=Just rep,init=[!mid:main.init]}}
	= {memory&above=above}
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
	= {memory&left=mapMaybe S_reverse left}
stackReverse Right memory=:{right}
	= {memory&right=mapMaybe S_reverse right}
stackReverse Middle memory=:{above={head={head=mid}}}
	= {memory&above.head.head=mapMaybe S_reverse mid}
stackReverse Both memory=:{left, right}
	= {memory&left=mapMaybe S_reverse left, right=mapMaybe S_reverse right}
stackReverse Primary memory=:{above}
	= {memory&above=S_map (S_map (mapMaybe S_reverse)) above}
stackReverse Base memory=:{above}
	= {memory&above=S_map S_reverse above}

stackRotate :: !StackID !Memory -> Memory
stackRotate _ memory=:{above={head={head=Nothing}}} = memory
stackRotate Left memory=:{left, above}
	# (top, mid) = decons (fromJust above.head.head)
	= {memory&left=mapMaybe (S_rotate (toInt top)) left,above.head.head=mid}
stackRotate Right memory=:{right, above}
	# (top, mid) = decons (fromJust above.head.head)
	= {memory&right=mapMaybe (S_rotate (toInt top)) right,above.head.head=mid}
stackRotate Both memory=:{left, right, above}
	# (top, mid) = decons (fromJust above.head.head)
	= let rotate = mapMaybe (S_rotate (toInt top))
	in {memory&left=rotate left,right=rotate right,above.head.head=mid}
stackRotate Middle memory=:{above}
	# (top, mid) = decons (fromJust above.head.head)
	= let rotate = mapMaybe (S_rotate (toInt top))
	in {memory&above.head.head=rotate mid}
stackRotate Primary memory=:{above}
	# (top, mid) = decons (fromJust above.head.head)
	= let rotate = mapMaybe (S_rotate (toInt top))
	in {memory&above=S_map (S_map rotate) {above&head.head=mid}}
stackRotate Base memory=:{above}
	# (top, mid) = decons (fromJust above.head.head)
	= let rotate = S_rotate (toInt top)
	in {memory&above=S_map rotate {above&head.head=mid}}

stackDelete :: !StackID !Memory -> Memory
stackDelete Left memory = {memory&left=zero}
stackDelete Right memory = {memory&right=zero}
stackDelete Middle memory=:{above}
	# (main, above) = decons above
	# main = tailOf main
	| isNothing main
		= {memory&above=fallback above}
	| otherwise
		= {memory&above=recons (fallback main, above)}
stackDelete Both memory = {memory&left=zero,right=zero}
stackDelete Base memory=:{above, below}
	| isNothing below
		= {memory&above=zero}
	| otherwise
		# (main, below) = decons (fromJust below)
		= {memory&above=fromSingle main,below=below}
stackDelete Main memory = {memory&above=zero,below=zero}
stackDelete Every memory = {memory&left=zero,right=zero,above=zero,below=zero}

stackDrop :: !StackID !Memory -> Memory
stackDrop _ memory=:{above={head={head=Nothing}}} = memory
stackDrop stackID memory=:{above={head={head=Just mid}}}
	# (top, mid) = decons mid
	# fn = let val = toInt top in if(val<0) (S_take(~val)) (S_drop val)
	# memory & above.head.head = mid
	= case stackID of
		Left = {memory&left=foldMaybe fn memory.left}
		Right = {memory&right=foldMaybe fn memory.right}
		Middle = {memory&above.head.head=foldMaybe fn memory.above.head.head}
		Both = {memory&left=foldMaybe fn memory.left,right=foldMaybe fn memory.right}
		Base = abort "Behaviour for command `stackDrop Base _` undecided"
	
cycleTops :: !Rotation !Memory -> Memory
cycleTops Anticlockwise memory=:{left, above={head={head=mid}}, right}
	# (top, mid) = safeDecons mid
	# (lhs, left) = safeDecons left
	# (rhs, right) = safeDecons right
	= {memory&left=rhs +++ left,right=top +++ right,above.head.head=lhs +++ mid}
where
	safeDecons Nothing = (Nothing, Nothing)
	safeDecons (Just arg)
		= let (val, other) = decons arg
		in (Just (fromSingle val), other)

cycleStacks :: !Rotation !Memory -> Memory
cycleStacks Anticlockwise memory=:{left, right, above}
	= {memory&left=right,right=above.head.head,above.head.head=left}

unpackLeftRight :: !Memory -> Memory
unpackLeftRight memory=:{above={head={head=Nothing}}} = memory
unpackLeftRight memory=:{right, left, above={head={head=Just mid}}}
	# (lhs, mid) = decons mid
	| isNothing mid
		= {memory&left=Just(recons(lhs,left)),above.head.head=Nothing}
	# (rhs, mid) = decons (fromJust mid)
	| otherwise
		= {memory&right=Just(recons(rhs,right)),left=Just(recons(lhs,left)),above.head.head=mid}

unpackRightLeft :: !Memory -> Memory
unpackRightLeft memory=:{above={head={head=Nothing}}} = memory
unpackRightLeft memory=:{right, left, above={head={head=Just mid}}}
	# (rhs, mid) = decons mid
	| isNothing mid
		= {memory&right=Just(recons(rhs,right)),above.head.head=Nothing}
	# (lhs, mid) = decons (fromJust mid)
	| otherwise
		= {memory&right=Just(recons(rhs,right)),left=Just(recons(lhs,left)),above.head.head=mid}

swapLeftRight :: !Memory -> Memory
swapLeftRight memory=:{left, right}
	= {memory&left=right,right=left}

swapTop :: !Axes !Memory -> Memory
swapTop Horizontal memory=:{left, right} = let
		(left`, right`) = S_swap left right
	in {memory&left=left`, right=right`}
swapTop Vertical _  = abort "behaviour for `swapTop Vertical _` is TBD"
//swapTop Vertical memory=:{main=main`=:{stack=[!El mid`:other]}}
//	= {memory&main={main`&stack=[!El (safeLast mid` + safeInit mid`):other]}}
swapTop Identity memory=:{right, above} = let
		(right`, mid`) = S_swap right above.head.head
	in {memory&right=right`,above.head.head=mid`}
swapTop Inverse memory=:{left, above} = let
		(left`, mid`) = S_swap left above.head.head
	in {memory&left=left`,above.head.head=mid`}
swapTop _ memory = memory	

moveTop :: !Direction !Memory -> Memory
moveTop East memory=:{left=(Just left), right}
	# (lhs, left) = decons left
	= {memory&left=left,right=Just (recons (lhs, right))}
moveTop West memory=:{left, right=(Just right)}
	# (rhs, right) = decons right
	= {memory&left=Just (recons (rhs, left)),right=right}
moveTop South memory=:{above={head={head=Just _}}}
	# mid = sanitize (fromJust memory.above.head.head)
	| IsEmpty mid.init
		= {memory&above.head.head=Just mid}
	| otherwise
		= {memory&above.head.head=Just{mid&head=Hd mid.init,init=[!mid.head:Tl mid.init]}}
moveTop NorthWest memory=:{left, above={head={head=Just mid}}}
	# (top, mid) = decons mid
	= {memory&left=Just (recons (top, left)),above.head.head=mid}
moveTop NorthEast memory=:{right, above={head={head=Just mid}}}
	# (top, mid) = decons mid
	= {memory&right=Just (recons (top, right)),above.head.head=mid}
moveTop SouthWest memory=:{right=(Just right), above}
	# (rhs, right) = decons right
	= {memory&right=right,above.head.head=Just (recons (rhs, above.head.head))}
moveTop SouthEast memory=:{left=(Just left), above}
	# (lhs, left) = decons left
	= {memory&left=left,above.head.head=Just (recons (lhs, above.head.head))}
moveTop _ memory = memory

copyTop :: !Direction !Memory -> Memory
copyTop East memory=:{left=(Just {head}), right}
	= {memory&right=Just (recons (head, right))}
copyTop West memory=:{left, right=(Just {head})}
	= {memory&left=Just (recons (head, left))}
copyTop North memory=:{above={head={head=Just{head}}}}
	= {memory&above.head.head=Just (recons (head, memory.above.head.head))}
copyTop NorthWest memory=:{left, above={head={head=Just{head}}}}
	= {memory&left=Just (recons (head, left))}
copyTop NorthEast memory=:{right, above={head={head=Just{head}}}}
	= {memory&right=Just (recons (head, right))}
copyTop SouthWest memory=:{right=Just{head}, above}
	= {memory&above.head.head=Just (recons (head, above.head.head))}
copyTop SouthEast memory=:{left=Just{head}, above}
	= {memory&above.head.head=Just (recons (head, above.head.head))}
copyTop _ memory = memory

copyBoth :: !Axes !Memory -> Memory
copyBoth Horizontal memory=:{left=(Just left), right=(Just right)}
	= {memory&left=Just (recons (right.head, Just left)),right=Just (recons (left.head, Just right))}
copyBoth Horizontal memory=:{left=Nothing, right=(Just right)}
	= {memory&left=Just (fromSingle right.head)}
copyBoth Horizontal memory=:{left=(Just left), right=Nothing}
	= {memory&right=Just (fromSingle left.head)}
copyBoth Vertical memory=:{above={head={head=Just mid}}}
	# mid = sanitize mid
	| IsEmpty mid.tail
		= {memory&above.head.head=Just {mid&tail=[!mid.head]}}
	| otherwise
		= {memory&above.head.head=Just {mid&head=Hd mid.tail,init=[!mid.head:mid.init],tail=[!mid.head:mid.tail]}}
copyBoth _ memory = memory

moveAll :: !Direction !Memory -> Memory
moveAll NorthWest memory=:{left, above}
	# (main, above) = decons above
	# (mid, main) = decons main
	# left = mid +++ left
	| isNothing main
		= {memory&left=left,above=fallback above}
	| otherwise
		= {memory&left=left,above=recons (fallback main, above)}
moveAll NorthEast memory=:{right, above}
	# (main, above) = decons above
	# (mid, main) = decons main
	# right = mid +++ right
	| isNothing main
		= {memory&right=right,above=fallback above}
	| otherwise
		= {memory&right=right,above=recons (fallback main, above)}
moveAll SouthWest memory=:{right, above}
	= {memory&right=zero,above.head=fromSingle right +++ above.head}
moveAll SouthEast memory=:{left, above}
	= {memory&left=zero,above.head=fromSingle left +++ above.head}

replicateBase :: !Memory -> Memory
replicateBase memory=:{above, below=Nothing} = {memory&below=Just above}
replicateBase memory=:{above, below=(Just below)} = {memory&below=Just (above+++below)}

replicateMiddle :: !Memory -> Memory
replicateMiddle memory=:{above}
	= {memory&above.head = recons (above.head.head, Just above.head)}

replicateTop :: !Memory -> Memory
replicateTop memory=:{above={head={head=Just mid}}}
	= {memory&above.head.head=Just (recons (mid.head, Just mid))}
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
shiftCursorDownwards memory=:{above, below=Nothing}
	# (main, above) = decons above
	= {memory&above=fromSingle main, below=above}
shiftCursorDownwards memory=:{above, below=(Just below)}
	# (base, below) = decons below
	= {memory&above={above&tail=[!base:above.tail]},below=below}

shiftCursorUpwards :: !Memory -> Memory
shiftCursorUpwards memory=:{above, below}
	# (base, above) = (lastOf above, initOf above)
	| isNothing above
		= {memory&above=fallback (above+++below),below=zero}
	| otherwise
		= {memory&above=fromJust above,below=Just (recons (base, below))}

moveCursorForwards :: !Memory -> Memory
moveCursorForwards memory=:{above={init=[!],tail=[!]}} = memory
moveCursorForwards memory=:{above, below}
	# (above, lastElement) = case above of
		{tail=[!lastElement:tail`]} = ({above&tail=tail`}, lastElement)
		{init} = ({above&init=Init init}, Last init)
	= {memory&above=above,below=Just (recons (lastElement, below))}

moveCursorBackwards :: !Memory -> Memory
moveCursorBackwards memory=:{below=Nothing} = memory
moveCursorBackwards memory=:{above, below=Just below}
	# (top, other) = decons below
	= {memory&above=above +++ (fromSingle top), below=other}

takeStackFrom :: !Memory -> Memory // negative takes from nth below cursor
takeStackFrom _ = abort "takeStackFrom is TBI"

// note modifiers
remember :: !Memory -> Memory
remember memory=:{above={head={head=Just mid}}}
	# (top, mid) = decons mid
	= {memory&note=top,above.head.head=mid}
remember memory = memory

recall :: !Memory -> Memory
recall memory=:{above, note}
	= {memory&above.head.head=Just (recons(note,above.head.head))}