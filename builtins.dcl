definition module builtins

import types


// "boolean" functions, returning -1 for True and 0 for False
isLessThan :: !Number !Number -> Number
isGreaterThan :: !Number !Number -> Number
isEqualTo :: !Number !Number -> Number
isLessOrEqual :: !Number !Number -> Number
isGreaterOrEqual :: !Number !Number -> Number
isNotEqual :: !Number !Number -> Number
isIdentical :: ![Number] ![Number] -> Number

isElementOf :: !Number ![Number] -> Number

isImproperSubsetOf :: ![Number] ![Number] -> Number
isProperSubsetOf :: ![Number] ![Number] -> Number
isNotSubsetOf :: ![Number] ![Number] -> Number

isUppercase :: !Number -> Number
isLowercase :: !Number -> Number

isFiniteReal :: !Number -> Number
isFiniteNumber :: !Number -> Number
isInfinite :: !Number -> Number

isPrime :: !Number -> Number

isSorted :: ![Number] -> Number

areAnyTrue :: ![Number] -> Number
areAllTrue :: ![Number] -> Number

// coalescing operators
logicEquiv :: !Number -> Number
logicNegate :: !Number -> Number

// remaining math ops
primeFactors :: !Number -> [Number]
conjugate :: !Number -> Number
justReal :: !Number -> Number
justImag :: !Number -> Number
reciprocal :: !Number -> Number
imagUnit :: !Number -> Number
numPermute :: !Number !Number -> Number
numCombin :: !Number !Number -> Number
logarithm :: !Number !Number -> Number
numProduct :: ![Number] -> Number
numSum :: ![Number] -> Number

// vectorized ops
vectorPlus :: ![Number] ![Number] -> [Number]
vectorTimes :: ![Number] ![Number] -> [Number]
vectorNegate :: ![Number] -> [Number]
vectorAND :: ![Number] ![Number] -> [Number]
vectorOR :: ![Number] ![Number] -> [Number]
vectorIsEqual :: ![Number] ![Number] -> [Number]
vectorElementOf :: ![Number] ![Number] -> [Number]
vectorLessThan :: ![Number] ![Number] -> [Number]
vectorGreaterThan :: ![Number] ![Number] -> [Number]
vectorLessOrEqual :: ![Number] ![Number] -> [Number]
vectorGreaterOrEqual :: ![Number] ![Number] -> [Number]

// miscelaneous operators
toUppercase :: !Number -> Number
toLowercase :: !Number -> Number
splitOnNewlines :: ![Number] -> [[Number]]
//MORE TO COME

// "set" operators
fromLeftStepRight :: !Number !Number -> [Number]
fromOneToMiddle :: !Number -> [Number]
fromMiddleToZero :: !Number -> [Number]
fromLeftTimesRight :: !Number !Number -> [Number]
setMinimum :: ![Number] -> Number
setMaximum :: ![Number] -> Number
setFilter :: ![Number] ![Number] -> [Number]
antiFilter :: ![Number] ![Number] -> [Number]
groupMiddle :: ![Number] -> [[Number]]
dupesMiddle :: ![Number] -> [Number]
setIntersection :: ![Number] ![Number] -> [Number]
setUnion :: ![Number] ![Number] -> [Number]
setExclusion :: ![Number] ![Number] -> [Number]
//powerset :: [Number] -> [[Number]]

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

// note modifiers
remember :: !Memory -> Memory
recall :: !Memory -> Memory