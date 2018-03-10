definition module stacks

import types, _SystemStrictLists, _SystemEnumStrict
from StdOverloaded import class +, class zero

instance + (Stack t)

instance zero (Stack t)

//fromList :: ![a] !Bool -> (Stack a)
fromList list bounded :== {stack=[!el \\ el <- list], bounded=bounded}
//toList :: !(Stack a) -> [a]
toList {stack} :== toList` stack
where
	toList` [!] = []
	toList` [!head:tail] = [head:toList` tail]
//fromSingle :: !a -> (Stack a)
fromSingle val :== {stack=[!val],bounded=True}

// standard functions

forEach :: !(a -> b) !(Stack a) -> (Stack b)

reduce :: !(a b -> b) !b !(Stack a) -> b