implementation module Dirty.Backend.Stack

import Dirty.Backend.Number, Dirty.Backend.Value
import Data.Maybe
import StdEnv, StdOverloadedList

instance zero Stack
where zero = {length=zero,attrs=zero,list=[|]}

instance +++ Stack
where
	(+++) lhs rhs = {
		length=lhs.length+rhs.length,
		attrs={
			hasStacks=lhs.attrs.hasStacks||rhs.attrs.hasStacks,
			hasNumbers=lhs.attrs.hasNumbers||rhs.attrs.hasNumbers,
			hasRationals=lhs.attrs.hasRationals||rhs.attrs.hasRationals,
			hasImaginarys=lhs.attrs.hasImaginarys||rhs.attrs.hasImaginarys,
			hasComplexes=lhs.attrs.hasComplexes||rhs.attrs.hasComplexes
			},
		list=lhs.list++|rhs.list
		}

instance toBool Stack
where
	toBool {list=[|]} = False
	toBool _ = True

instance fromString Stack
where
	fromString str = {
		length=fromInt (size str),
		attrs={zero&hasNumbers=True,hasRationals=True},
		list=[|Num (fromInt (toInt c)) \\ c <-: str]
		}

instance zero StackAttrs
where zero = {
		hasStacks=False,
		hasNumbers=False,
		hasRationals=False,
		hasImaginarys=False,
		hasComplexes=False
		}
		
class toStack a :: a -> Stack
instance toStack [Char]
where
	toStack str = {
		length=fromInt (length str),
		attrs={zero&hasNumbers=True,hasRationals=True},
		list=[|toValue c \\ c <- str]
		}
		
fromValue :: Value -> Stack
fromValue val=:(Num i) = {
	length=one,
	attrs={
		hasStacks=False,
		hasNumbers=True,
		hasRationals=isRational i,
		hasImaginarys=isImaginary i,
		hasComplexes=isComplex i
		},
	list=[|val]
	}
fromValue val=:(Stk i) = {
	length=one,
	attrs={
		hasStacks=True,
		hasNumbers=False,
		hasRationals=False,
		hasImaginarys=False,
		hasComplexes=False
		},
	list=[|val]
	}
	
prepend :: Value Stack -> Stack
prepend val stack=:{attrs}
	= {stack&
		length=inc stack.length,
		attrs=(case val of
			(Num num) = {attrs&
				hasNumbers=True,
				hasRationals=attrs.hasRationals||isRational num,
				hasImaginarys=attrs.hasImaginarys||isImaginary num,
				hasComplexes=attrs.hasComplexes||isComplex num
				}
			(Stk grp) = {attrs&
				hasStacks=True
				}
			),
		list=[|val:stack.list]
		}