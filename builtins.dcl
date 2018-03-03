definition module builtins

import types


// "boolean" functions, returning -1 for True and 0 for False
isLessThan :: !Number !Number -> Number
isGreaterThan :: !Number !Number -> Number
isEqualTo :: !Number !Number -> Number
isLessOrEqual :: !Number !Number -> Number
isGreaterOrEqual :: !Number !Number -> Number
isNotEqual :: !Number !Number -> Number
isIdentical :: [Number] [Number] -> Number

isElementOf :: !Number [Number] -> Number

isImproperSubsetOf :: [Number] [Number] -> Number
isProperSubsetOf :: [Number] [Number] -> Number
isNotSubsetOf :: [Number] [Number] -> Number

isUppercase :: !Number -> Number
isLowercase :: !Number -> Number

isFiniteReal :: !Number -> Number
isFiniteNumber :: !Number -> Number
isInfinite :: !Number -> Number

isPrime :: !Number -> Number

isSorted :: [Number] -> Number

areAnyTrue :: [Number] -> Number
areAllTrue :: [Number] -> Number

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
dotProduct :: [Number] [Number] -> Number
numPermute :: !Number !Number -> Number
numCombin :: !Number !Number -> Number
logarithm :: !Number !Number -> Number

// miscelaneous operators
toUppercase :: !Number -> Number
toLowercase :: !Number -> Number
//MORE TO COME

// "set" operators
setMinimum :: [Number] -> Number
setMaximum :: [Number] -> Number
setFilter :: [Number] [Number] -> [Number]

// special cases
stacksFromCursor :: !Memory -> Memory
transposeFromCursor :: !Memory -> Memory

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