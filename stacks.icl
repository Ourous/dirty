implementation module stacks

import types, StdOverloadedList, StdEnv, StdLib, Text, Data.Func

instance == (Stack t) | Eq t where
	(==) lhs rhs = lhs.finite && rhs.finite && (toStrictList lhs == toStrictList rhs)
	
instance +++ (Stack t) where
	(+++) lhs=:{finite=False} rhs=:{finite=False}
		= {lhs&tail=rhs.tail}
		
instance toString (Stack t) | toString t where
	toString {head=[!h:_],tail=[!t:_],finite=False} = "[" <+ h <+ "..." <+ t <+ "]"
	toString {head=[!h:_],finite=False} = "[" <+ h <+ "...]"
	toString {tail=[!t:_],finite=False} = "[..." <+ t <+ "]"
	toString arg = "["+join","(map toString (toList arg))+"]"
		
instance zero (Stack t) where
	zero = {head=[!],tail=[!],finite=True}
	
normalize :: !.(Stack a) -> .(Stack a)
normalize arg=:{head=[!_:_],tail=[!_:_]} = arg
normalize arg=:{finite=False} = arg
normalize arg=:{head=head=:[!_,_:_],tail=[!],finite=True}
	# (head, tail) = splitAt_strict (((LengthM head)+1)/2) head
	= {arg&head=head,tail=ReverseM tail}
normalize arg=:{head=[!],tail=tail=:[!_:_],finite=True}
	# (tail, head) = splitAt_strict ((LengthM tail)/2) tail
	= {arg&head=ReverseM head,tail=tail}
		
decons :: !.(Stack a) -> *(!Maybe a, !.Stack a)
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
	= {arg&head=Filter fn arg.head,tail=Filter fn arg.tail}
where
	head` => Filter fn arg.head
	tail` => Filter fn arg.tail

S_filterOn :: !(b -> Bool) !.(Stack a) !.(Stack b) -> .(Stack a)
S_filterOn fn lhs rhs
	= {lhs`&head=[!el\\el<-lhs`.head&cond<-rhs`.head|fn cond]}
where
	lhs` = forceHead lhs
	rhs` = forceHead rhs
	
S_span :: !(a -> Bool) !.(Stack a) ->  *(Stack a, Stack a)
S_span fn arg
	# (l, r) = Span fn (toStrictList arg)
	= (fsl l, fsl r)
where
	fsl [!] = Nothing
	fsl lst = (Just (fromStrictList lst True))
	
S_reverse :: u:(Stack a) -> v:(Stack a), [u <= v]
S_reverse arg=:{init=[!],tail=[!]} = arg
S_reverse arg=:{head,init,tail=[!h:t]} = {arg&head=h,init=t,tail=[!head:init]}
S_reverse arg=:{head,init} = {arg&head=Last init,init=[!],tail=Init init}
	
S_rotate ::  !Int u:(Stack a) -> v:(Stack a), [u <= v]
S_rotate num arg=:{finite=False}
	| num > 0
		# (head, [!next:t]) = SplitAt (dec num) arg.init
		= {arg&head=next,init=t,tail=Reverse[!arg.head:head]++|arg.tail}
	| num < 0
		# (head, [!next:t]) = SplitAt (dec (abs num)) arg.tail
		= {arg&head=next,init=Reverse[!arg.head:head]++|arg.init,tail=t}
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
			
S_swap :: !.(MStack a) !.(MStack a) -> *(MStack a, MStack a)
S_swap Nothing Nothing = (Nothing, Nothing)
S_swap Nothing (Just rhs)
	# (r, rhs) = decons rhs
	= (Just (fromSingle r), rhs)
S_swap (Just lhs) Nothing
	# (l, lhs) = decons lhs
	= (lhs, Just (fromSingle l))
S_swap (Just lhs) (Just rhs)
	= (Just {lhs&head=rhs.head}, Just {rhs&head=lhs.head})
			
S_sort :: !.(Stack a) -> .(Stack a) | Ord a
S_sort arg = fromList (sort (toList arg)) arg.finite