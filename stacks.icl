implementation module stacks

import types, StdOverloadedList, StdEnv, StdLib, Data.Func, arithmetic, utilities

appendStrict lhs [!] = lhs
appendStrict [!] rhs = rhs
appendStrict [!l:lhs] rhs = [!l:appendStrict lhs rhs]

instance + [!t] where (+) lhs rhs = appendStrict lhs rhs

appendHyper =: hyperstrict o appendStrict

instance + (Stack t) where
	(+) {stack=lhs, bounded=True} {stack=rhs, bounded=True}
		= {stack=appendStrict lhs rhs, bounded=True}
	(+) {stack=lhs} {stack=rhs}
		= {stack=appendStrict lhs rhs, bounded=False}
		
instance zero (Stack t) where
	zero =: {stack=[!], bounded=True}

instance == (Stack t) | Eq t where
	(==) {stack=lhs, bounded=True} {stack=rhs, bounded=True} = lhs == rhs
	(==) _ _ = False
	
instance < (Stack t) | Ord t where
	(<) {bounded=False} {bounded=False} = False
	(<) {stack=lhs} {stack=rhs} = lhs < rhs

safeLast :: !(Stack a) -> (Stack a)
safeLast {stack=[!]} = zero
safeLast arg = fromSingle (lastOf arg)
safeInit :: !(Stack a) -> (Stack a)
safeInit arg = {arg&stack=init` arg.stack}
where
	init` [!head:tail] = [!head:init` tail]
	init` _ = [!]

S_filterBy :: !(a -> Bool) !.(Stack a) -> .(Stack a)
S_filterBy fn {stack, bounded}
	= {stack=S_filterBy` stack, bounded=bounded}
where
	S_filterBy` [!] = [!]
	S_filterBy` [!head:tail]
		| fn head
			= [!head:S_filterBy` tail]
		| otherwise
			= S_filterBy` tail
			
S_filterOn :: !(b -> Bool) !.(Stack a) !.(Stack b) -> .(Stack a)
S_filterOn fn lhs rhs
	= {stack=filterOn` lhs.stack rhs.stack, bounded=lhs.bounded||rhs.bounded}
where
	filterOn` [!] _ = [!]
	filterOn` _ [!] = [!]
	filterOn` [!l:lhs] [!r:rhs]
		| fn r
			= [!l:filterOn` lhs rhs]
		| otherwise
			= filterOn` lhs rhs

//S_zipWith :: !(a a -> b) !.(Stack a) !.(Stack a) -> .(Stack b)



S_partition :: !(a -> Bool) !.(Stack a) -> *(.(Stack a), .(Stack a))
S_partition fn arg=:{stack, bounded}
	# (l, r) = categorize` stack
	= ({stack=l,bounded=bounded}, {stack=r,bounded=bounded})
where
	categorize` [!] = ([!], [!])
	categorize` [!head:tail]
		# (l, r) = categorize` tail
		| fn head
			= ([!head:l], r)
		| otherwise
			= (l, [!head:r])
			
S_split :: !(a -> Bool) !.(Stack a) ->  *(Stack a, Stack a)
S_split fn arg=:{stack, bounded}
	= splitWhen` stack
where
	splitWhen` [!] = (zero, zero)
	splitWhen` [!head:tail]
		| fn head
			= (zero, {stack=[!head:tail],bounded=bounded})
		| otherwise
			# (l, r) = splitWhen` tail
			= (recons (head, l), r)

S_reverse :: !.(Stack a) -> .(Stack a)
S_reverse {stack, bounded}
	= {stack=reversed` [!] stack, bounded=bounded}
where
	reversed` rev [!] = rev
	reversed` rev [!head:tail] = reversed` [!head:rev] tail
	
S_rotate :: !Int !.(Stack a) -> .(Stack a)
S_rotate num arg
	= fromList (rotateList num (toList arg)) arg.bounded

S_take :: !Int !.(Stack a) -> .(Stack a)
S_take num {stack}
	= {zero&stack=take` num stack}
where
	take` _ [!] = [!]
	take` num [!head:tail]
		| num > 0
			= [!head:take`(dec num)tail]
		| otherwise
			= [!]
			
S_drop :: !Int !.(Stack a) -> .(Stack a)
S_drop num arg=:{stack}
	= {arg&stack=drop` num stack}
where
	drop` _ [!] = [!]
	drop` num list=:[!_:tail]
		| num > 0
			= drop`(dec num)tail
		| otherwise
			= list

S_sort :: !.(Stack a) -> .(Stack a) | Ord a
S_sort arg
	= fromList (sort (toList arg)) arg.bounded

S_occurrences :: !(a -> Bool) !.(Stack a) -> Int
S_occurrences fn {stack} = occurrences` 0 stack
where
	occurrences` acc [!] = acc
	occurrences` acc [!head:tail]
		| fn head
			= occurrences` (inc acc) tail
		| otherwise
			= occurrences` acc tail