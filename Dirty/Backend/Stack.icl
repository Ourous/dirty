implementation module Dirty.Backend.Stack

import Dirty.Backend.Number, Dirty.Backend.Value
import Data.Maybe
import StdOverloaded, StdOverloadedList

instance zero Stack
where zero = {length=zero,attrs=zero,list=[!]}

instance +++ Stack
where
	(+++) lhs rhs = {
		length=lhs.length+rhs.length,
		attrs={
			hasStacks=lhs.attrs.hasStacks||rhs.attrs.hasStacks,
			hasNumbers=lhs.attrs.hasNumbers||rhs.attrs.hasNumbers,
			hasReals=lhs.attrs.hasReals||rhs.attrs.hasReals,
			hasImaginarys=lhs.attrs.hasImaginarys||rhs.attrs.hasImaginarys,
			hasComplexes=lhs.attrs.hasComplexes||rhs.attrs.hasComplexes
			},
		list=lhs.list++|rhs.list
		}

instance zero StackAttrs
where zero = {
		hasStacks=False,
		hasNumbers=False,
		hasReals=False,
		hasImaginarys=False,
		hasComplexes=False
		}
		
fromValue :: Value -> Stack
fromValue val=:(Item i) = {
	length=one,
	attrs={
		hasStacks=False,
		hasNumbers=True,
		hasReals=isReal i,
		hasImaginarys=isImaginary i,
		hasComplexes=isComplex i
		},
	list=[!val]
	}
fromValue val=:(Group i) = {
	length=one,
	attrs={
		hasStacks=True,
		hasNumbers=False,
		hasReals=False,
		hasImaginarys=False,
		hasComplexes=False
		},
	list=[!val]
	}