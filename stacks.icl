implementation module stacks

import types, StdOverloadedList, StdEnv, StdLib, Data.Func

instance == (Stack t) | Eq t where
	(==) lhs rhs = lhs.finite && rhs.finite
	
instance +++ (Stack t) where
	(+++) lhs=:{finite=False} rhs=:{finite=False}
		= {lhs&tail=rhs.tail}
		
decons :: !.(Stack a) -> *(!a, !.MStack a)
decons arg=:{head,init=[!h:t]} = (head, Just {arg&head=h,init=t})
decons arg=:{tail=[!_:_]} = decons (normalize arg)
decons arg=:{head,init=[!],tail=[!]} = (head, Nothing)

recons :: !*(!a, !.(MStack a)) -> .(Stack a)
recons (h, Just arg=:{head,init}) = {arg&head=h,init=[!head:init]}
recons (h, Nothing) = fromSingle h