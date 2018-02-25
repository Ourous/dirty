implementation module builtins

import types, arithmetic, StdEnv, StdLib

isLessThan :: Number Number -> Number
isLessThan lhs rhs = fromBool (lhs < rhs)
isGreaterThan :: Number Number -> Number
isGreaterThan lhs rhs = fromBool (lhs > rhs)
isEqualTo :: Number Number -> Number
isEqualTo lhs rhs = fromBool (lhs == rhs)
isLessOrEqual :: Number Number -> Number
isLessOrEqual lhs rhs = fromBool (lhs <= rhs)
isGreaterOrEqual :: Number Number -> Number
isGreaterOrEqual lhs rhs = fromBool (lhs >= rhs)
isNotEqual :: Number Number -> Number
isNotEqual lhs rhs = fromBool (lhs <> rhs)
isIdentical :: [Number] [Number] -> Number
isIdentical lhs rhs = fromBool (lhs == rhs)