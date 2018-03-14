implementation module stacks

import types, _SystemStrictLists, _SystemEnumStrict, StdEnv, StdLib, Data.Func, arithmetic, utilities

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

instance == (Stack Number) where
	(==) {stack=lhs, bounded=True} {stack=rhs, bounded=True} = lhs == rhs
	(==) _ _ = False
	
instance == [!Number] where
	(==) [!] [!] = True
	(==) [!l:lhs] [!r:rhs] = l == r && lhs == rhs
	(==) _ _ = False
	
/*	
fromList :: ![a] !Bool -> (Stack a)
fromList list bounded = {stack=[!el \\ el <- list], bounded=bounded}
toList :: !(Stack a) -> [a]
toList {stack} = toList` stack
where
	toList` [!] = []
	toList` [!head:tail] = [head:toList` tail]
	
fromSingle :: !a -> (Stack a)
fromSingle val = {stack=[!val],bounded=True}
*/

lastOf :: !(Stack a) -> a
lastOf {stack} = last` stack
where
	last` [!last] = last
	last` [!_:tail] = last` tail
	
initOf :: !(Stack a) -> (Stack a)
initOf arg = {arg&stack=init` arg.stack}
where
	init` [!last] = [!]
	init` [!head:tail] = [!head:init` tail]
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

S_zipWith :: !(a a -> b) !.(Stack a) !.(Stack a) -> .(Stack b)
S_zipWith fn lhs rhs
	= {stack=withEach` lhs.stack rhs.stack, bounded=lhs.bounded||rhs.bounded}
where
	withEach` [!] _ = [!]
	withEach` _ [!] = [!]
	withEach` [!l:lhs] [!r:rhs]
		= [!fn l r:withEach` lhs rhs]

S_map :: !(a -> b) !.(Stack a) -> .(Stack b)
S_map fn arg=:{stack, bounded}
	= {arg&stack=forEach` stack}
where
	forEach` [!] = [!]
	forEach` [!head:tail]
		= [!fn head:forEach` tail]
		
S_reduce :: !(a .b -> .b) !.b !.(Stack a) -> .b
S_reduce fn init arg=:{stack, bounded}
	//= if(bounded) hyperstrict id (reduce` init stack)
	= reduce` init stack
where
	reduce` acc [!] = acc
	reduce` acc [!head:tail]
		#! val = fn head acc
		= reduce` val tail
		
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
			
S_span :: !(a -> Bool) !.(Stack a) ->  *(Stack a, Stack a)
S_span fn arg=:{stack, bounded}
	= splitWhen` stack
where
	splitWhen` [!] = (zero, zero)
	splitWhen` [!head:tail]
		| fn head
			= (zero, {stack=[!head:tail],bounded=bounded})
		| otherwise
			# (l, r) = splitWhen` tail
			= (recons (head, l), r)

S_uniques :: !.(Stack a) -> .(Stack a) | Eq a
S_uniques arg
	= fromList (removeDup (toList arg)) arg.bounded

S_reverse :: !.(Stack a) -> .(Stack a)
S_reverse {stack, bounded}
	= {stack=reversed` [!] stack, bounded=bounded}
where
	//reversed` [!] = [!]
	//reversed` [!head:tail] = appendStrict (reversed` tail) [!head]
	reversed` rev [!] = rev
	reversed` rev [!head:tail] = reversed` [!head:rev] tail
	
S_rotate :: !Int !.(Stack a) -> .(Stack a)
S_rotate num arg
	= fromList (rotateList num (toList arg)) arg.bounded
	
S_take :: !Int !.(Stack a) -> .(Stack a)
S_take num arg
	= {stack=take` num arg.stack, bounded=True}
where
	take` _ [!] = [!]
	take` num [!head:tail]
		| num > 0
			= [!head:take`(dec num)tail]
		| otherwise
			= [!]
			
S_drop :: !Int !.(Stack a) -> .(Stack a)
S_drop num arg
	= {arg&stack=drop` num arg.stack}
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
		
S_all :: !(a -> Bool) !.(Stack a) -> Bool
S_all _ {bounded=False} = False
S_all fn {stack} = areAll` stack
where
	areAll` [!] = True
	areAll` [!head:tail] = fn head && areAll` tail
	
S_any :: !(a -> Bool) !.(Stack a) -> Bool
S_any _ {bounded=False} = True
S_any fn {stack} = areAny` stack
where
	areAny` [!] = False
	areAny` [!head:tail] = fn head || areAny` tail
