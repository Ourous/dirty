definition module stacks

import types, StdOverloadedList
from Data.Func import hyperstrict

reduce_strict fn acc arg :== reduce` acc arg
where
	reduce` acc [!head:tail]
		#! val = fn acc head
		= reduce` (hyperstrict val) tail
	reduce` acc _ = acc
	
splitAt_strict ind arg :== splitAt` ind [!] arg
where
	splitAt` _ acc [!] = (acc, [!])
	splitAt` ind acc arg=:[!h:t]
		| ind < one
			= (acc, arg)
		| otherwise
			= splitAt` (dec ind) (acc ++$ [!h]) t

instance == (Stack t) | Eq t
instance +++ (Stack t)
instance toString (Stack t) | toString t
instance zero (Stack t)

forceHead arg=:{head,tail,finite} :== {arg&head=head ++$ if(finite) (ReverseM tail) [!],tail=[!]}
forceTail arg=:{head,tail,finite} :== {arg&head=[!],tail=tail ++$ if(finite) (ReverseM head) [!]}

normalize :: !.(Stack a) -> .(Stack a)

//fromList :: ![a] !Bool -> (Stack a)
fromList list finite :== fromStrictList [!el \\ el <- list] finite
fromStrictList list finite :== {head=list,tail=[!],finite=finite}
toStrictList {head,tail,finite} :== head ++$ if(finite) (ReverseM tail) [!]
toList arg :== toList` (toStrictList arg)
where
	toList` [!] = []
	toList` [!head:tail] = [head:toList` tail]
//fromSingle :: !a -> (Stack a)
fromSingle val :== {head=[!val],tail=[!],finite=True}

decons :: !.(Stack a) -> *(!Maybe a, !.Stack a)
recons :: !*(!a, !.(Stack a)) -> .(Stack a)
fallback arg :== case arg of
	Nothing = zero
	(Just val) = val

S_filterBy :: !(a -> Bool) !.(Stack a) -> .(Stack a)
S_filterOn :: !(b -> Bool) !.(Stack a) !.(Stack b) -> .(Stack a)
//S_zipWith :: !(a b -> c) !.(Stack a) !.(Stack b) -> .(Stack c)

S_map fn arg :== {arg&head=MapM fn arg.head,tail=MapM fn arg.tail}
S_reduce fn acc arg :== reduce_strict fn acc (toStrictList arg)
S_collapse fn acc arg :== reduce_strict fn (reduce_strict fn acc arg.head) arg.tail
S_span :: !(a -> Bool) !.(Stack a) ->  *(Stack a, Stack a)
S_reverse ::  u:(Stack a) -> v:(Stack a), [u <= v]
S_rotate ::  !Int u:(Stack a) -> v:(Stack a), [u <= v]
//S_take :: !Int !.(Stack a) -> .(Stack a)
S_take num arg :== fromStrictList (TakeM num (toStrictList arg)) True
S_drop num arg :== {(fromStrictList (DropM num (toStrictList arg)) arg.finite)&tail=if(arg.finite)[!]arg.tail}
//S_drop :: !Int !.(Stack a) -> .(Stack a)
S_uniques :: !.(Stack a) -> .(Stack a) | Eq a
S_swap :: !.(Stack a) !.(Stack a) -> *(Stack a, Stack a)
S_sort :: !.(Stack a) -> .(Stack a) | Ord a
S_length arg :== (reduce_strict (\_ = \b -> inc b) (reduce_strict (\_ = \b -> inc b) zero arg.head) arg.tail) 
//S_occurrences :: !(a -> Bool) !.(Stack a) -> Int
S_occurrences fn arg :== S_collapse (\a -> \b -> if(fn b) (inc a) a) 0 arg

S_all fn arg=:{head, finite} :== finite && All fn arg.head && All fn arg.tail
S_any fn arg=:{head, finite} :== (not finite) || Any fn arg.head || Any fn arg.tail
