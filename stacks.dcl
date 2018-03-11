definition module stacks

import types, _SystemStrictLists, _SystemEnumStrict, StdOverloaded

instance + (Stack t)
instance zero (Stack t)
instance == (Stack Number)

//fromList :: ![a] !Bool -> (Stack a)
fromList list bounded :== fromStrictList [!el \\ el <- list] bounded
fromStrictList list bounded :== {stack=list,bounded=bounded}
//toList :: !(Stack a) -> [a]
toList {stack} :== toList` stack
where
	toList` [!] = []
	toList` [!head:tail] = [head:toList` tail]
//fromSingle :: !a -> (Stack a)
fromSingle val :== {stack=[!val],bounded=True}

tailOf arg=:{stack} :== case stack of
	[!_:tail] = {arg&stack=tail}
	_ = arg
headOf {stack=[!head:_]} :== head
// standard functions

filterBy :: !(a -> Bool) !(Stack a) -> (Stack a)
filterOn :: !(b -> Bool) !(Stack a) !(Stack b) -> (Stack a)
withEach :: !(a a -> b) !(Stack a) !(Stack a) -> (Stack b)
forEach :: !(a -> b) !(Stack a) -> (Stack b)
reduce :: !(a b -> b) !b !(Stack a) -> b
categorize :: !(a -> Bool) !(Stack a) -> (Stack a, Stack a)
splitWhen :: !(a -> Bool) !(Stack a) -> (Stack a, Stack a)

occurrences :: !(a -> Bool) !(Stack a) -> Int

areAll :: !(a -> Bool) !(Stack a) -> Bool
areAny :: !(a -> Bool) !(Stack a) -> Bool