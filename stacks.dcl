definition module stacks

import types, _SystemStrictLists, _SystemEnumStrict, StdOverloaded, StdClass

instance + (Stack t)
instance zero (Stack t)
instance == (Stack Number)

//fromList :: ![a] !Bool -> (Stack a)
fromList list bounded :== fromStrictList [!el \\ el <- list] bounded
fromStrictList list bounded :== {stack=list,bounded=bounded}
toList {stack} :== toList` stack
where
	toList` [!] = []
	toList` [!head:tail] = [head:toList` tail]
//fromSingle :: !a -> (Stack a)
fromSingle val :== {stack=[!val],bounded=True}

decons arg=:{stack=[!head:tail]} :== (head, {arg&stack=tail})
recons (head, arg=:{stack=tail}) :== {arg&stack=[!head:tail]}
safeDecon arg :== (safeHead arg, safeTail arg)
decon2 arg=:{stack=[!head,next:tail]} :== (head, next, {arg&stack=tail})
recon2 (head, next, arg=:{stack=tail}) :== {arg&stack=[!head,next:tail]}
recon3 (head, next, nexter, arg=:{stack=tail}) :== {arg&stack=[!head,next,nexter:tail]}

tailOf arg=:{stack=[!_:tail]} :== {arg&stack=tail}
headOf {stack=[!head:_]} :== head
lastOf :: !(Stack a) -> a
initOf :: !(Stack a) -> (Stack a)
// standard functions
safeTail arg=:{stack} :== case stack of
	[!_:tail] = {arg&stack=tail}
	_ = arg
safeHead arg=:{stack} :== case stack of
	[!head:_] = {stack=[!head],bounded=True}
	_ = zero
safeLast :: !(Stack a) -> (Stack a)
safeInit :: !(Stack a) -> (Stack a)

S_filterBy :: !(a -> Bool) !.(Stack a) -> .(Stack a)
S_filterOn :: !(b -> Bool) !.(Stack a) !.(Stack b) -> .(Stack a)
S_zipWith :: !(a a -> b) !.(Stack a) !.(Stack a) -> .(Stack b)
S_map :: !(a -> b) !.(Stack a) -> .(Stack b)
S_reduce :: !(a .b -> .b) !.b !.(Stack a) -> .b
S_partition :: !(a -> Bool) !.(Stack a) -> *(.(Stack a), .(Stack a))
S_span :: !(a -> Bool) !.(Stack a) ->  *(Stack a, Stack a)
S_uniques :: !.(Stack a) -> .(Stack a) | Eq a
S_reverse :: !.(Stack a) -> .(Stack a)
S_rotate :: !Int !.(Stack a) -> .(Stack a)
S_take :: !Int !.(Stack a) -> .(Stack a)
S_drop :: !Int !.(Stack a) -> .(Stack a)
S_sort :: !.(Stack a) -> .(Stack a) | Ord a

S_occurrences :: !(a -> Bool) !.(Stack a) -> Int

S_all :: !(a -> Bool) !.(Stack a) -> Bool
S_any :: !(a -> Bool) !.(Stack a) -> Bool