definition module stacks

import types, StdOverloadedList, StdOverloaded, StdClass
from Data.Func import hyperstrict
instance + [!t]

instance + (Stack t)
instance zero (Stack t)
instance == (Stack t) | Eq t

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
//lastOf :: !(Stack a) -> a
lastOf {stack} :== last` stack
where
	last` [!last] = last
	last` [!_:tail] = last` tail
//initOf :: !(Stack a) -> (Stack a)
initOf arg=:{stack} :== {arg&stack=init` stack}
where
	init` [!head:tail] = [!head:init` tail]
	init` _ = [!]
// standard functions
safeTail arg=:{stack} :== case stack of
	[!_:tail] = {arg&stack=tail}
	_ = arg
safeHead {stack} :== case stack of
	[!head:_] = {zero&stack=[!head]}
	_ = zero
safeLast :: !(Stack a) -> (Stack a)
safeInit :: !(Stack a) -> (Stack a)

S_filterBy :: !(a -> Bool) !.(Stack a) -> .(Stack a)
S_filterOn :: !(b -> Bool) !.(Stack a) !.(Stack b) -> .(Stack a)
//S_zipWith :: !(a a -> b) !.(Stack a) !.(Stack a) -> .(Stack b)
S_zipWith fn lhs rhs :== {stack=withEach` lhs.stack rhs.stack, bounded=lhs.bounded||rhs.bounded}
where
	withEach` [!l:lhs] [!r:rhs]
		= [!fn l r:withEach` lhs rhs]
	withEach` _ _ = [!]

//S_map :: !(a -> b) !.(Stack a) -> .(Stack b)
S_map fn arg=:{stack} :== {arg&stack=Map fn stack}
//S_reduce :: !(.b a -> .b) !.b !.(Stack a) -> .b
S_reduce fn init {stack} :== reduce` init stack
where
	reduce` acc [!head:tail]
		#! val = fn acc head
		= reduce` (hyperstrict val) tail
	reduce` acc _ = acc
S_partition :: !(a -> Bool) !.(Stack a) -> *(.(Stack a), .(Stack a))
S_split :: !(a -> Bool) !.(Stack a) ->  *(Stack a, Stack a)
S_uniques arg :== {arg&stack=RemoveDup arg.stack}
S_reverse :: !.(Stack a) -> .(Stack a)
S_rotate :: !Int !.(Stack a) -> .(Stack a)
S_take :: !Int !.(Stack a) -> .(Stack a)
S_drop :: !Int !.(Stack a) -> .(Stack a)
S_sort :: !.(Stack a) -> .(Stack a) | Ord a

S_length :== S_reduce (\_ = \b -> inc b) Zero
S_occurrences :: !(a -> Bool) !.(Stack a) -> Int

S_all fn {stack, bounded} :== bounded && All fn stack
S_any fn {stack, bounded} :== bounded || Any fn stack