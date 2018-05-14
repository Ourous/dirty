implementation module stacks

import types, StdOverloadedList, StdEnv, StdLib, Text, Data.Func

instance == (Stack t) | Eq t where
	(==) lhs rhs = lhs.finite && rhs.finite && (toStrictList lhs == toStrictList rhs)
	
instance +++ (Stack t) where
	(+++) lhs=:{finite=False} rhs=:{finite=False}
		= {lhs&tail=rhs.tail}
	(+++) lhs=:{finite=False} rhs=:{finite=True}
		= {lhs&tail=rhs.tail ++| (Reverse [!rhs.head:rhs.init]) ++| lhs.tail}
	(+++) lhs=:{finite=True} rhs=:{finite=False}
		= {rhs&head=rhs.head,init=rhs.init ++| (Reverse rhs.tail) ++| [!lhs.head:lhs.init]}
	(+++) lhs rhs
		= {head=lhs.head,init=lhs.init ++| (Reverse lhs.tail),tail=(Reverse [!rhs.head:rhs.init]) ++| rhs.tail,finite=True}
instance +++ (MStack t) where
	(+++) Nothing Nothing = Nothing
	(+++) Nothing rhs = rhs
	(+++) lhs Nothing = lhs
	(+++) (Just lhs) (Just rhs) = Just (lhs +++ rhs)
		
instance toString (Stack t) | toString t where
	toString {head,finite=False} = "[" <+ head <+ "...]"
	toString arg = "["+join","(map toString (toList arg))+"]"
instance toString (MStack t) | toString t where
	toString Nothing = "[]"
	toString (Just stack) = toString stack
		
instance zero (Stack t) | zero t where
	zero = {head=zero,init=[!],tail=[!],finite=True}
instance zero (MStack t) where
	zero = Nothing
		
decons :: !.(Stack a) -> *(!a, !.MStack a)
decons arg=:{head,init=[!],tail=[!]} = (head, Nothing)
decons arg=:{head,init=[!h:t]} = (head, Just {arg&head=h,init=t})
decons arg=:{tail=[!_:_]}
	# arg = sanitize arg
	= let {head, init=[!h:t]} = arg
	in (head, Just {arg&head=h,init=t})

recons :: !*(!a, !.(MStack a)) -> .(Stack a)
recons (h, Just arg=:{head,init}) = {arg&head=h,init=[!head:init]}
recons (h, Nothing) = fromSingle h

lastOf :: !.(Stack a) -> a
lastOf arg=:{tail=[!l:_]} = l
lastOf arg=:{init=[!]} = arg.head
lastOf arg = Last arg.init

tailOf :: !.(Stack a) -> .(MStack a)
tailOf arg=:{init=[!h:t]} = (Just {arg&head=h,init=t})
tailOf arg=:{tail=[!_:_]} = (Just {arg&head=Last arg.tail,tail=Init arg.tail})
tailOf arg=:{init=[!],tail=[!]} = Nothing
initOf :: !.(Stack a) -> .(MStack a)
initOf arg=:{tail=[!_:t]} = (Just {arg&tail=t})
initOf arg=:{init=[!_:_]} = (Just {arg&init=Init arg.init})
initOf arg=:{init=[!],tail=[!]} = Nothing

S_filterBy :: !(a -> Bool) !.(Stack a) -> .(MStack a)
S_filterBy fn arg
	| fn arg.head
		= (Just {arg&init=init`,tail=tail`})
	| not (IsEmpty init`)
		= (Just {arg&head=Hd init`,init=Tl init`,tail=tail`})
	| not (IsEmpty tail`)
		= (Just {arg&head=Last tail`,init=init`,tail=Init tail`})
	| otherwise
		= Nothing
where
	init` => Filter fn arg.init
	tail` => Filter fn arg.tail

S_filterOn :: !(b -> Bool) !.(Stack a) !.(Stack b) -> .(MStack a)
S_filterOn fn lhs rhs
	| fn rhs.head
		= (Just {lhs&init=init`,tail=[!]})
	| not (IsEmpty init`)
		= (Just {lhs&head=Hd init`,init=Tl init`,tail=[!]})
	| otherwise
		= Nothing
where
	init` => (fst o Unzip o Filter (\(_, e) -> fn e)) (Zip2 (toStrictList lhs) (toStrictList rhs))
	
S_span :: !(a -> Bool) !.(Stack a) ->  *(MStack a, MStack a)
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