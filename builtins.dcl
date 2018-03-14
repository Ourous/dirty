definition module builtins

import types


// "boolean" functions, returning -1 for True and 0 for False
isLessThan :: !Number !Number -> Number
isGreaterThan :: !Number !Number -> Number
isEqualTo :: !Number !Number -> Number
isLessOrEqual :: !Number !Number -> Number
isGreaterOrEqual :: !Number !Number -> Number
isNotEqual :: !Number !Number -> Number
isIdentical :: !(Stack Number) !(Stack Number) -> Number

isElementOf :: !Number !(Stack Number) -> Number

isImproperSubsetOf :: !(Stack Number) !(Stack Number) -> Number
isProperSubsetOf :: !(Stack Number) !(Stack Number) -> Number
isNotSubsetOf :: !(Stack Number) !(Stack Number) -> Number

isUppercase :: !Number -> Number
isLowercase :: !Number -> Number

isFiniteReal :: !Number -> Number
isFiniteNumber :: !Number -> Number
isInfinite :: !Number -> Number

isPrime :: !Number -> Number

isSorted :: !(Stack Number) -> Number

areAnyTrue :: !(Stack Number) -> Number
areAllTrue :: !(Stack Number) -> Number

// coalescing operators
logicEquiv :: !Number -> Number
logicNegate :: !Number -> Number

// remaining math ops
primeFactors :: !Number -> (Stack Number)
conjugate :: !Number -> Number
justReal :: !Number -> Number
justImag :: !Number -> Number
reciprocal :: !Number -> Number
imagUnit :: !Number -> Number
numPermute :: !Number !Number -> Number
numCombin :: !Number !Number -> Number
logarithm :: !Number !Number -> Number
numProduct :: !(Stack Number) -> Number
numSum :: !(Stack Number) -> Number
numAverage :: !(Stack Number) -> Number
convToBase :: !Number !Number -> (Stack Number)
convFromBase :: !(Stack Number) !Number -> Number

// vectorized ops
vectorPlus :: !(Stack Number) !(Stack Number) -> (Stack Number)
vectorTimes :: !(Stack Number) !(Stack Number) -> (Stack Number)
vectorNegate :: !(Stack Number) -> (Stack Number)
vectorAND :: !(Stack Number) !(Stack Number) -> (Stack Number)
vectorOR :: !(Stack Number) !(Stack Number) -> (Stack Number)
vectorIsEqual :: !(Stack Number) !(Stack Number) -> (Stack Number)
vectorElementOf :: !(Stack Number) !(Stack Number) -> (Stack Number)
vectorLessThan :: !(Stack Number) !(Stack Number) -> (Stack Number)
vectorGreaterThan :: !(Stack Number) !(Stack Number) -> (Stack Number)
vectorLessOrEqual :: !(Stack Number) !(Stack Number) -> (Stack Number)
vectorGreaterOrEqual :: !(Stack Number) !(Stack Number) -> (Stack Number)

// miscelaneous operators
toUppercase :: !Number -> Number
toLowercase :: !Number -> Number
splitOnNewlines :: !(Stack Number) -> (Stack Element)
//MORE TO COME

// "set" operators
fromLeftStepRight :: !Number !Number -> (Stack Number)
fromOneToMiddle :: !Number -> (Stack Number)
fromMiddleToZero :: !Number -> (Stack Number)
fromLeftTimesRight :: !Number !Number -> (Stack Number)
setMinimum :: !(Stack Number) -> Number
setMaximum :: !(Stack Number) -> Number
setFilter :: !(Stack Number) !(Stack Number) -> (Stack Number)
antiFilter :: !(Stack Number) !(Stack Number) -> (Stack Number)
groupMiddle :: !(Stack Number) -> (Stack Element)
dupesMiddle :: !(Stack Number) -> (Stack Number)
setIntersection :: !(Stack Number) !(Stack Number) -> (Stack Number)
setExclusion :: !(Stack Number) !(Stack Number) -> (Stack Number)
numContigSubsets :: !(Stack Number) !(Stack Number) -> Number
splitContig :: !(Stack Number) !(Stack Number) -> (Stack Element)
contigSubsets :: !(Stack Number) -> (Stack Element)
//powerset :: (Stack Number) -> (Stack Element)

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