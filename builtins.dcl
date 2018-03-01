definition module builtins

import types


// "boolean" functions, returning -1 for True and 0 for False
isLessThan :: Number Number -> Number
isGreaterThan :: Number Number -> Number
isEqualTo :: Number Number -> Number
isLessOrEqual :: Number Number -> Number
isGreaterOrEqual :: Number Number -> Number
isNotEqual :: Number Number -> Number
isIdentical :: [Number] [Number] -> Number

isElementOf :: Number [Number] -> Number

isImproperSubsetOf :: [Number] [Number] -> Number
isProperSubsetOf :: [Number] [Number] -> Number
isNotSubsetOf :: [Number] [Number] -> Number

isUppercase :: Number -> Number
isLowercase :: Number -> Number

isFiniteReal :: Number -> Number
isFiniteNumber :: Number -> Number
isInfinite :: Number -> Number

isPrime :: Number -> Number

isSorted :: [Number] -> Number

areAnyTrue :: [Number] -> Number
areAllTrue :: [Number] -> Number


// stack manipulations
stackReverse :: StackID Memory -> Memory
stackRotate :: StackID Memory -> Memory
stackDelete :: StackID Memory -> Memory
//stackDrop :: StackID Memory -> Memory
//cycleTops :: Rotation Memory -> Memory
//cycleStacks :: Rotation Memory -> Memory
