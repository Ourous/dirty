implementation module stacks

import types, StdOverloadedList, StdEnv, StdLib, Text, Data.Func

instance == (Stack t) | Eq t where
	(==) lhs rhs = lhs.finite && rhs.finite && (toStrictList lhs == toStrictList rhs)
	
instance +++ (Stack t) where
	(+++) lhs=:{finite=False} rhs=:{finite=False}
		= {lhs&tail=rhs.tail}
	(+++) lhs=:{finite=True} rhs=:{finite=False}
		= {rhs&head=lhs.head ++| (Reverse lhs.tail) ++| rhs.head}
	(+++) lhs=:{finite=False} rhs=:{finite=True}
		= {lhs&tail=(Reverse rhs.head) ++| rhs.tail ++| lhs.tail}
	(+++) lhs=:{finite=True} rhs=:{finite=True}
		| IsEmpty lhs.tail
			= {rhs&head=lhs.head ++| rhs.head}
		| IsEmpty rhs.head
			= {lhs&tail=rhs.tail ++| lhs.tail}
		| otherwise
			= {zero&head=lhs.head ++| (Reverse lhs.tail), tail=(Reverse rhs.head) ++| rhs.tail}
			
instance toString (Stack t) | toString t where
	toString {head=[!h:_],tail=[!t:_],finite=False} = "[" <+ h <+ "..." <+ t <+ "]"
	toString {head=[!h:_],finite=False} = "[" <+ h <+ "...]"
	toString {tail=[!t:_],finite=False} = "[..." <+ t <+ "]"
	toString arg = "["+join","(map toString (toList arg))+"]"
		
instance zero (Stack t) where
	zero = {head=[!],tail=[!],finite=True}
	
normalize :: u:(Stack a) -> v:(Stack a), [u <= v]
normalize arg=:{head=[!_:_],tail=[!_:_]} = arg
normalize arg=:{finite=False} = arg
normalize arg=:{head=head=:[!_,_:_],tail=[!],finite=True}
	# (head, tail) = SplitAt (((Length head)+1)/2) head
	= {arg&head=head,tail=ReverseM tail}
normalize arg=:{head=[!],tail=tail=:[!_:_],finite=True}
	# (tail, head) = SplitAt ((Length tail)/2) tail
	= {arg&head=ReverseM head,tail=tail}
		
decons :: u:(Stack (v:[.a] -> .a)) -> *(Maybe (v:[.a] -> .a),w:(Stack (v:[.a] -> .a))), [u <= w]
decons arg=:{head=[!],tail=[!]} = (Nothing, arg)
decons arg=:{head=[!h:t]} = (Just head, {arg&head=t})
decons arg=:{tail=[!_:_]}
	# arg = normalize arg
	= let {head=[!h:t]} = arg
	in (Just h, {arg&head=t})

recons :: !*(!a, !.(Stack a)) -> .(Stack a)
recons (val, arg=:{head}) = {arg&head=[!val:head]}


S_filterBy :: !(a -> Bool) !.(Stack a) -> .(Stack a)
S_filterBy fn arg
	= {arg&head=head`,tail=tail`}
where
	head` => Filter fn arg.head
	tail` => Filter fn arg.tail

S_filterOn :: !(b -> Bool) !.(Stack a) !.(Stack b) -> .(Stack a)
S_filterOn fn lhs rhs
	= {zero&head=lhs`}
where
	lr = Zip (toStrictList lhs, Map fn (toStrictList rhs))
	lhs` = Map (\(v, _) = v) (Filter (\(_, v) = v) lr)
	
S_span :: !(a -> Bool) !.(Stack a) ->  *(Stack a, Stack a)
S_span fn arg
	# (l, r) = Span fn (toStrictList arg)
	= ({zero&head=l}, {zero&head=r,finite=arg.finite})
	
S_reverse :: u:(Stack a) -> v:(Stack a), [u <= v]
S_reverse arg=:{head,tail} = {arg&head=tail,tail=head}
	
S_rotate ::  !Int u:(Stack a) -> v:(Stack a), [u <= v]
S_rotate num arg=:{finite=False}
	| num > 0
		# (head, next) = SplitAt (dec num) arg.head
		= {arg&head=next,tail=Reverse head ++| arg.tail}
	| num < 0
		# (tail, next) = SplitAt (dec (abs num)) arg.tail
		= {arg&head=Reverse tail ++| arg.head,tail=next}
	| otherwise
		= arg
S_rotate num arg=:{finite=True}
	= fromStrictList (rotate` num (toStrictList arg)) True
where
	rotate` _ [!] = [!]
	rotate` n list
		| n > zero
			= rotate` (dec n) ((Tl list) ++| [!Hd list])
		| n < zero
			= rotate` (inc n) [!Last list:Init list]
		| otherwise
			= list
			
S_uniques :: !.(Stack a) -> .(Stack a) | Eq a
S_uniques arg = fromStrictList (RemoveDup (toStrictList arg)) arg.finite
			
S_swap :: .(Stack (u:[.a] -> .a)) .(Stack (u:[.a] -> .a)) -> *(Stack (u:[.a] -> .a),Stack (u:[.a] -> .a))
S_swap lhs rhs = let
	(l, lhs`) = decons lhs
	(r, rhs`) = decons rhs
	in (M_recons (r, lhs`), M_recons (l, rhs`))


S_sort :: !.(Stack a) -> .(Stack a) | Ord a
S_sort arg = fromList (sort (toList arg)) arg.finite