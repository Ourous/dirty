implementation module stacks

import types, _SystemStrictLists, _SystemEnumStrict, StdEnv, StdLib, Data.Func, arithmetic, utilities

appendStrict lhs [!] = lhs
appendStrict [!] rhs = rhs
appendStrict [!l:lhs] rhs = [!l:appendStrict lhs rhs]

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

filterBy :: !(a -> Bool) !(Stack a) -> (Stack a)
filterBy fn {stack, bounded}
	= {stack=filterBy` stack, bounded=bounded}
where
	filterBy` [!] = [!]
	filterBy` [!head:tail]
		| fn head
			= [!head:filterBy` tail]
		| otherwise
			= filterBy` tail
			
filterOn :: !(b -> Bool) !(Stack a) !(Stack b) -> (Stack a)
filterOn fn lhs rhs
	= {stack=filterOn` lhs.stack rhs.stack, bounded=lhs.bounded||rhs.bounded}
where
	filterOn` [!] _ = [!]
	filterOn` _ [!] = [!]
	filterOn` [!l:lhs] [!r:rhs]
		| fn r
			= [!l:filterOn` lhs rhs]
		| otherwise
			= filterOn` lhs rhs

withEach :: !(a a -> b) !(Stack a) !(Stack a) -> (Stack b)
withEach fn lhs rhs
	= {stack=withEach` lhs.stack rhs.stack, bounded=lhs.bounded||rhs.bounded}
where
	withEach` [!] _ = [!]
	withEach` _ [!] = [!]
	withEach` [!l:lhs] [!r:rhs]
		= [!fn l r:withEach` lhs rhs]

forEach :: !(a -> b) !(Stack a) -> (Stack b)
forEach fn arg=:{stack, bounded}
	= {arg&stack=forEach` stack}
where
	forEach` [!] = [!]
	forEach` [!head:tail]
		= [!fn head:forEach` tail]
		
reduce :: !(a b -> b) !b !(Stack a) -> b
reduce fn init arg=:{stack, bounded}
	= if(bounded) hyperstrict id (reduce` init stack)
where
	reduce` acc [!] = acc
	reduce` acc [!head:tail]
		= reduce` (fn head acc) tail
		
categorize :: !(a -> Bool) !(Stack a) -> (Stack a, Stack a)
categorize fn arg=:{stack, bounded}
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
			
splitWhen :: !(a -> Bool) !(Stack a) -> (Stack a, Stack a)
splitWhen fn arg=:{stack, bounded}
	= splitWhen` stack
where
	splitWhen` [!] = (zero, zero)
	splitWhen` [!head:tail]
		| fn head
			= (zero, {stack=[!head:tail],bounded=bounded})
		| otherwise
			# (l, r) = splitWhen` tail
			= (l + fromSingle head, r)

uniques :: !(Stack a) -> (Stack a) | Eq a
uniques arg
	= fromList (removeDup (toList arg)) arg.bounded

reversed :: !(Stack a) -> (Stack a)
reversed {stack, bounded}
	= {stack=reversed` stack, bounded=bounded}
where
	reversed` [!] = [!]
	reversed` [!head:tail] = appendStrict (reversed` tail) [!head]
	
rotated :: !Int !(Stack a) -> (Stack a)
rotated num arg
	= fromList (rotateList num (toList arg)) arg.bounded

occurrences :: !(a -> Bool) !(Stack a) -> Int
occurrences fn {stack} = occurrences` 0 stack
where
	occurrences` acc [!] = acc
	occurrences` acc [!head:tail]
		| fn head
			= occurrences` (inc acc) tail
		| otherwise
			= occurrences` acc tail
		
areAll :: !(a -> Bool) !(Stack a) -> Bool
areAll _ {bounded=False} = False
areAll fn {stack} = areAll` stack
where
	areAll` [!] = True
	areAll` [!head:tail] = fn head && areAll` tail
	
areAny :: !(a -> Bool) !(Stack a) -> Bool
areAny _ {bounded=False} = True
areAny fn {stack} = areAny` stack
where
	areAny` [!] = False
	areAny` [!head:tail] = fn head || areAny` tail
