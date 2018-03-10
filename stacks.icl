implementation module stacks

import types, _SystemStrictLists, _SystemEnumStrict, StdEnv, StdLib, Data.Func

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

forEach :: !(a -> b) !(Stack a) -> (Stack b)
forEach fn arg=:{stack, bounded}
	= {arg&stack=if(bounded) hyperstrict id (forEach` stack)}
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
		
