definition module builtins

import types

// "boolean" functions, returning -1 for True and 0 for False
isLessThan :: !Number !Number -> Number
isGreaterThan :: !Number !Number -> Number
isEqualTo :: !Number !Number -> Number
isLessOrEqual :: !Number !Number -> Number
isGreaterOrEqual :: !Number !Number -> Number
isNotEqual :: !Number !Number -> Number
isIdentical :: !(MStack Number) !(MStack Number) -> Number

isElementOf :: !Number !(MStack Number) -> Number

isImproperSubsetOf :: !(MStack Number) !(MStack Number) -> Number
isProperSubsetOf :: !(MStack Number) !(MStack Number) -> Number
isNotSubsetOf :: !(MStack Number) !(MStack Number) -> Number

isUppercase :: !Number -> Number
isLowercase :: !Number -> Number

isFiniteReal :: !Number -> Number
isFiniteNumber :: !Number -> Number
isInfinite :: !Number -> Number

isPrime :: !Number -> Number

isSorted :: !(MStack Number) -> Number

areAnyTrue :: !(MStack Number) -> Number
areAllTrue :: !(MStack Number) -> Number

// coalescing operators
logicEquiv :: !Number -> Number
logicNegate :: !Number -> Number

// remaining math ops
primeFactors :: !Number -> (MStack Number)
conjugate :: !Number -> Number
justReal :: !Number -> Number
justImag :: !Number -> Number
reciprocal :: !Number -> Number
imagUnit :: !Number -> Number
numPermute :: !Number !Number -> Number
numCombin :: !Number !Number -> Number
logarithm :: !Number !Number -> Number
numProduct :: !(MStack Number) -> Number
numSum :: !(MStack Number) -> Number
numAverage :: !(MStack Number) -> Number
convToBase :: !Number !Number -> (MStack Number)
convFromBase :: !(MStack Number) !Number -> Number

// vectorized ops
vectorPlus :: !(MStack Number) !(MStack Number) -> (MStack Number)
vectorTimes :: !(MStack Number) !(MStack Number) -> (MStack Number)
vectorNegate :: !(MStack Number) -> (MStack Number)
vectorAND :: !(MStack Number) !(MStack Number) -> (MStack Number)
vectorOR :: !(MStack Number) !(MStack Number) -> (MStack Number)
vectorIsEqual :: !(MStack Number) !(MStack Number) -> (MStack Number)
vectorElementOf :: !(MStack Number) !(MStack Number) -> (MStack Number)
vectorLessThan :: !(MStack Number) !(MStack Number) -> (MStack Number)
vectorGreaterThan :: !(MStack Number) !(MStack Number) -> (MStack Number)
vectorLessOrEqual :: !(MStack Number) !(MStack Number) -> (MStack Number)
vectorGreaterOrEqual :: !(MStack Number) !(MStack Number) -> (MStack Number)

// miscelaneous operators
toUppercase :: !Number -> Number
toLowercase :: !Number -> Number
splitOnNewlines :: !(MStack Number) -> (Stack (MStack Number))
//MORE TO COME

// "set" operators
fromLeftStepRight :: !Number !Number -> (MStack Number)
fromOneToMiddle :: !Number -> (MStack Number)
fromMiddleToZero :: !Number -> (MStack Number)
fromLeftTimesRight :: !Number !Number -> (MStack Number)
setMinimum :: !(MStack Number) -> Number
setMaximum :: !(MStack Number) -> Number
setFilter :: !(MStack Number) !(MStack Number) -> (MStack Number)
antiFilter :: !(MStack Number) !(MStack Number) -> (MStack Number)
groupMiddle :: !(MStack Number) -> (Stack (MStack Number))
dupesMiddle :: !(MStack Number) -> (MStack Number)
setIntersection :: !(MStack Number) !(MStack Number) -> (MStack Number)
setExclusion :: !(MStack Number) !(MStack Number) -> (MStack Number)
numContigSubsets :: !(MStack Number) !(MStack Number) -> Number
splitContig :: !(MStack Number) !(MStack Number) -> (Stack (MStack Number))
contigSubsets :: !(MStack Number) -> (Stack (MStack Number))
//powerset :: (MStack Number) -> (Stack (MStack Number))

// special cases
complexSplit :: !Memory -> Memory
matrixProduct :: !Memory -> Memory
joinWithNewlines :: !Memory -> Memory
stacksFromCursor :: !Memory -> Memory
transposeFromCursor :: !Memory -> Memory
stackJoin :: !Memory -> Memory
stackUnjoin :: !Memory -> Memory
removeDupBase :: !Memory -> Memory
repeatTopMiddle :: !Memory -> Memory
repeatFullMiddle :: !Memory -> Memory
sortBaseline :: !Memory -> Memory

// stack manipulations
stackReverse :: !StackID !Memory -> Memory
stackRotate :: !StackID !Memory -> Memory
stackDelete :: !StackID !Memory -> Memory
stackDrop :: !StackID !Memory -> Memory
cycleTops :: !Rotation !Memory -> Memory
cycleStacks :: !Rotation !Memory -> Memory
unpackLeftRight :: !Memory -> Memory
unpackRightLeft :: !Memory -> Memory
swapLeftRight :: !Memory -> Memory
swapTop :: !Axes !Memory -> Memory
moveTop :: !Direction !Memory -> Memory
copyTop :: !Direction !Memory -> Memory
copyBoth :: !Axes !Memory -> Memory
moveAll :: !Direction !Memory -> Memory
replicateBase :: !Memory -> Memory
replicateMiddle :: !Memory -> Memory
replicateTop :: !Memory -> Memory
dupesBase :: !Memory -> Memory
shiftCursorDownwards :: !Memory -> Memory
shiftCursorUpwards :: !Memory -> Memory
moveCursorForwards :: !Memory -> Memory
moveCursorBackwards :: !Memory -> Memory
takeStackFrom :: !Memory -> Memory

// note modifiers
remember :: !Memory -> Memory
recall :: !Memory -> Memory