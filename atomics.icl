implementation module atomics

import types, StdOverloaded, StdBool, StdInt, StdReal
from StdLib import isFinite

FRAC_MAX :== 4503599627370496.0
FRAC_MIN :== -4503599627370496.0

IS_INT real :== toReal (entier real) == real

IS_ZERO numeric
	:== case numeric of
		(Int 0) = True
		(Real 0.0) = True
		(Real -0.0) = True
		_ = False

IS_NAN numeric
	:== case numeric of
		(Real val) = not (isFinite val || val <> 0.0)
		_ = False
		
IS_INF numeric
	:== case numeric of
		(Real val) = not (isFinite val) && val <> 0.0
		_ = False

// numeric implementations

instance + Numeric where
	(+) (Int lhs) (Int rhs)
		= IF_INT_64_OR_32
			(Int (lhs + rhs))
			(Real (toReal lhs + toReal rhs))
	(+) (Int lhs) (Real rhs)
		= (Real (toReal lhs + rhs))
	(+) (Real lhs) (Int rhs)
		= (Real (lhs + toReal rhs))
	(+) (Real lhs) (Real rhs)
		= (Real (lhs + rhs))
		
instance - Numeric where
	(-) (Int lhs) (Int rhs)
		= IF_INT_64_OR_32
			 (Int (lhs - rhs))
			 (Real (toReal lhs - toReal rhs))
	(-) (Int lhs) (Real rhs)
		= (Real (toReal lhs - rhs))
	(-) (Real lhs) (Int rhs)
		= (Real (lhs - toReal rhs))
	(-) (Real lhs) (Real rhs)
		= (Real (lhs - rhs))

instance * Numeric where
	(*) (Int lhs) (Int rhs)
		= (Real (toReal lhs * toReal rhs))
	(*) (Int lhs) (Real rhs)
		= (Real (toReal lhs * rhs))
	(*) (Real lhs) (Int rhs)
		= (Real (lhs * toReal rhs))
	(*) (Real lhs) (Real rhs)
		= (Real (lhs * rhs))

instance / Numeric where
	(/) (Int lhs) (Int rhs)
		= if(lhs rem rhs == 0) (Int (lhs / rhs)) (Real (lhs / rhs))
	(/) (Int lhs) (Real rhs)
		= (Real (toReal lhs / rhs))
	(/) (Real lhs) (Int rhs)
		= (Real (lhs / toReal rhs))
	(/) (Real lhs) (Real rhs)
		= (Real (lhs / rhs))

instance ^ Numeric where
	(^) (Int lhs) (Int rhs)
		= (Real (toReal lhs ^ toReal rhs))
	(^) (Int lhs) (Real rhs)
		= (Real (toReal lhs ^ rhs))
	(^) (Real lhs) (Int rhs)
		= (Real (lhs ^ toReal rhs))
	(^) (Real lhs) (Real rhs)
		= (Real (lhs ^ rhs))

instance abs Numeric where
	abs (Int val)
		= (Int (abs val))
	abs (Real val)
		= (Real (abs val))

instance ~ Numeric where
	~ (Int val)
		= (Int (~ val))
	~ (Real val)
		= (Real (~ val))

instance == Numeric where
	(==) (Int lhs) (Int rhs)
		= lhs == rhs
	(==) (Int lhs) (Real rhs)
		= toReal lhs == rhs
	(==) (Real lhs) (Int rhs)
		= lhs == toReal rhs
	(==) (Real lhs) (Real rhs)
		= lhs == rhs

instance < Numeric where
	(<) (Int lhs) (Int rhs)
		= lhs < rhs
	(<) (Int lhs) (Real rhs)
		= toReal lhs < rhs
	(<) (Real lhs) (Int rhs)
		= lhs < toReal rhs
	(<) (Real lhs) (Real rhs)
		= lhs < rhs
			
// number implementations

handle :: Number -> Number
handle Zero = Zero
handle NaN = NaN
handle Infinity = Infinity
handle (Rational val)
	| IS_ZERO val = Zero
	| IS_NAN val = NaN
	| IS_INF val = Infinity
	= (Rational val)
handle (Imaginary val)
	| IS_ZERO val = Zero
	| IS_NAN val = NaN
	| IS_INF val = Infinity
	= (Imaginary val)
handle (Complex re im)
	| IS_NAN re || IS_NAN im = NaN
	| IS_INF re || IS_INF im = Infinity
	= case (IS_ZERO re) (IS_ZERO im) of
		True True = Zero
		True False = (Imaginary im)
		False True = (Rational re)
		False False = (Complex re im)
		
instance + Number where
	(+) NaN _ = NaN
	(+) _ NaN = NaN
	(+) Infinity _ = Infinity
	(+) _ Infinity = Infinity
	(+) (Rational lhs) (Rational rhs)
		= handle (lhs + rhs)
	(+) (Rational lhs) (Imaginary rhs)
		= handle (Complex lhs rhs)
	(+) (Rational lhs) (Complex rhsRe rhsIm)
		= handle (Complex (lhs + rhsRe) rhsIm)
	(+) (Rational lhs) Infinity
		| IS_NAN lhs = NaN