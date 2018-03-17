definition module stacks

import types, StdOverloadedList
from Data.Func import hyperstrict

reduce fn acc arg :== reduce` acc arg
where
	reduce` acc [!head:tail]
		#! val = fn acc head
		= reduce` (hyperstrict val) tail
	reduce` acc _ = acc

instance == (Stack t) | Eq t
instance +++ (Stack t)
instance toString (Stack t) | toString t
instance toString (MStack t) | toString t
instance zero (Stack t) | zero t
instance zero (MStack t)

normalize arg :== case arg of
	{tail=[!_:_],finite=True} = {arg&init=arg.init++$ReverseM arg.tail,tail=[!]}
	_ = arg
sanitize arg :== case arg of
	(Just arg=:{init=[!],tail=[!_:_],finite=True}) = Just{arg&init=[!LastM arg.tail],tail=InitM arg.tail}
	_ = arg

//fromList :: ![a] !Bool -> (Stack a)
fromList list finite :== fromStrictList [!el \\ el <- list] finite
fromStrictList [!head:init] finite :== {head=head,init=init,tail=[!],finite=finite}
toStrictList {head,init,tail,finite} :== [!head:init] ++$ if(finite) (ReverseM tail) [!]
toList arg :== toList` (toStrictList arg)
where
	toList` [!] = []
	toList` [!head:tail] = [head:toList` tail]
//fromSingle :: !a -> (Stack a)
fromSingle val :== {head=val,init=[!],tail=[!],finite=True}

decons :: !.(Stack a) -> *(!a, !.MStack a)
recons :: !*(!a, !.(MStack a)) -> .(Stack a)
fallback arg :== case arg of
	Nothing = zero
	(Just val) = val

tailOf :: !.(Stack a) -> .(MStack a)
initOf :: !.(Stack a) -> .(MStack a)

S_filterBy :: !(a -> Bool) !.(Stack a) -> .(MStack a)
S_filterOn :: !(b -> Bool) !.(Stack a) !.(Stack b) -> .(MStack a)
//S_zipWith :: !(a b -> c) !.(Stack a) !.(Stack b) -> .(Stack c)
S_zipWith fn lhs rhs :== S_zipWith` (normalize lhs) (normalize rhs)
where
	zipWith` [!l:lhs] [!r:rhs] = [!fn l r:zipWith` lhs rhs]
	zipWith` _ _ = [!]
	S_zipWith` lhs rhs = {head=fn lhs.head rhs.head,init=zipWith` lhs.init rhs.init,tail=zipWith` lhs.tail rhs.tail,finite=lhs.finite||rhs.finite}
S_map fn arg :== {arg&head=fn arg.head,init=MapM fn arg.init,tail=MapM fn arg.tail}
S_reduce fn acc arg :== reduce fn acc (toStrictList arg)
S_collapse fn acc arg :== reduce fn (reduce fn (fn acc arg.head) arg.init) arg.tail
S_span :: !(a -> Bool) !.(Stack a) ->  *(MStack a, MStack a)
S_reverse ::  u:(Stack a) -> v:(Stack a), [u <= v]
S_rotate ::  !Int u:(Stack a) -> v:(Stack a), [u <= v]
//S_take :: !Int !.(Stack a) -> .(Stack a)
S_take num arg :== fromStrictList (TakeM num (toStrictList arg)) True
S_drop num arg :== {(fromStrictList (DropM num (toStrictList arg)) arg.finite)&tail=if(arg.finite)[!]arg.tail}
//S_drop :: !Int !.(Stack a) -> .(Stack a)
S_uniques :: !.(Stack a) -> .(Stack a) | Eq a
S_swap :: !.(MStack a) !.(MStack a) -> *(MStack a, MStack a)
S_sort :: !.(Stack a) -> .(Stack a) | Ord a
S_length arg :== one + (reduce (\_ = \b -> inc b) (reduce (\_ = \b -> inc b) arg.tail) arg.init) 
//S_occurrences :: !(a -> Bool) !.(Stack a) -> Int
S_occurrences fn arg :== S_collapse (\a -> \b -> if(fn b) (inc a) a) Zero arg

S_all fn arg=:{head, finite} :== finite && fn head && All fn arg.init && All fn arg.tail
S_any fn arg=:{head, finite} :== finite || fn head || Any fn arg.init || Any fn arg.tail
