definition module stacks

import types, _SystemStrictLists, _SystemEnumStrict, StdOverloaded

instance + (Stack t)
instance zero (Stack t)
instance == (Stack Number)

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
categorize :: !(a -> Bool) !(Stack a) -> (Stack a, Stack a)
//divide :: !(a -> Bool) !(Stack a) -> (Stack a, Stack a)

occurrences :: !(a -> Bool) !(Stack a) -> Int

areAll :: !(a -> Bool) !(Stack a) -> Bool
areAny :: !(a -> Bool) !(Stack a) -> Bool