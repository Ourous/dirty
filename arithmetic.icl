implementation module arithmetic

import types, atomics, StdBool, 

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
	= case ((IS_ZERO re), (IS_ZERO im)) of
		(True, True) = Zero
		(True, False) = (Imaginary im)
		(False, True) = (Rational re)
		(False, False) = (Complex re im)

instance + Number where
	(+) NaN _ = NaN
	(+) _ NaN = NaN
	(+) Infinity _ = Infinity
	(+) _ Infinity = Infinity
	(+) Zero val = val
	(+) val Zero = val
	(+) (Rational lhs) (Rational rhs)
		= handle (Rational (lhs + rhs))
	(+) (Rational lhs) (Imaginary rhs)
		= handle (Complex lhs rhs)
	(+) (Imaginary lhs) (Rational rhs)
		= handle (Complex rhs lhs)
	(+) (Imaginary lhs) (Imaginary rhs)
		= handle (Imaginary (lhs + rhs))
	(+) (Rational lhs) (Complex rhsRe rhsIm)
		= handle (Complex (lhs + rhsRe) rhsIm)
	(+) (Imaginary lhs) (Complex rhsRe rhsIm)
		= handle (Complex rhsRe (lhs + rhsIm))
	(+) (Complex lhsRe lhsIm) (Rational rhs)
		= handle (Complex (lhsRe + rhs) lhsIm)
	(+) (Complex lhsRe lhsIm) (Imaginary rhs)
		= handle (Complex lhsRe (lhsIm + rhs))
	(+) (Complex lhsRe lhsIm) (Complex rhsRe rhsIm)
		= handle (Complex (lhsRe + rhsRe) (lhsIm + rhsIm))

instance - Number where
	(-) NaN _ = NaN
	(-) _ NaN = NaN
	(-) Infinity _ = Infinity
	(-) _ Infinity = Infinity
	(-) val Zero = val
	(-) Zero val = ~val // this comes second for performance
	(-) (Rational lhs) (Rational rhs)
		= handle (Rational (lhs - rhs))
	(-) (Rational lhs) (Imaginary rhs)
		= handle (Complex lhs (~rhs))
	(-) (Imaginary lhs) (Rational rhs)
		= handle (Complex (~rhs) lhs)
	(-) (Imaginary lhs) (Imaginary rhs)
		= handle (Imaginary (lhs - rhs))
	(-) (Rational lhs) (Complex rhsRe rhsIm)
		= handle (Complex (lhs - rhsRe) (~rhsIm))
	(-) (Imaginary lhs) (Complex rhsRe rhsIm)
		= handle (Complex (~rhsRe) (lhs - rhsIm))
	(-) (Complex lhsRe lhsIm) (Rational rhs)
		= handle (Complex (lhsRe - rhs) lhsIm)
	(-) (Complex lhsRe lhsIm) (Imaginary rhs)
		= handle (Complex lhsRe (lhsIm - rhs))
	(-) (Complex lhsRe lhsIm) (Complex rhsRe rhsIm)
		= handle (Complex (lhsRe - rhsRe) (lhsIm - rhsIm))
		
instance zero Number where
	zero = Zero
	
instance * Number where
	(*) NaN _ = NaN
	(*) _ NaN = NaN
	(*) Zero _ = Zero
	(*) _ Zero = Zero
	(*) Infinity _ = Infinity
	(*) _ Infinity = Infinity
	(*) (Rational lhs) (Rational rhs)
		= handle (Rational (lhs * rhs))
	(*) (Rational lhs) (Imaginary rhs)
		= handle (Imaginary (lhs * rhs))
	(*) (Imaginary lhs) (Rational rhs)
		= handle (Imaginary (lhs * rhs))
	(*) (Imaginary lhs) (Imaginary rhs)
		= handle (Rational (~(lhs * rhs)))
	(*) (Rational lhs) (Complex rhsRe rhsIm)
		= handle (Complex (lhs * rhsRe) (lhs * rhsIm))
	(*) (Imaginary lhs) (Complex rhsRe rhsIm)
		= handle (Complex (~(lhs * rhsIm)) (lhs * rhsRe))
	(*) (Complex lhsRe lhsIm) (Rational rhs)
		= handle (Complex (lhsRe * rhs) (lhsIm * rhs))
	(*) (Complex lhsRe lhsIm) (Imaginary rhs)
		= handle (Complex (~(lhsIm * rhs)) (lhsRe * rhs))
	(*) (Complex lhsRe lhsIm) (Complex rhsRe rhsIm)
		= handle (Complex (lhsRe * rhsRe - lhsIm * rhsIm) (lhsIm * rhsRe + rhsIm * lhsRe))
		
instance / Number where
	(/) NaN _ = NaN
	(/) _ NaN = NaN
	(/) Infinity Infinity = NaN
	(/) Zero Zero = NaN
	(/) Infinity _ = Infinity
	(/) Zero _ = Zero
	(/) _ Zero = Infinity
	(/) _ Infinity = Zero
	(/) (Rational lhs) (Rational rhs)
		= handle (Rational (lhs / rhs))
	(/) (Rational lhs) (Imaginary rhs)
		= handle (Imaginary (~(lhs / rhs)))
	(/) (Imaginary lhs) (Rational rhs)
		= handle (Imaginary (lhs / rhs))
	(/) (Imaginary lhs) (Imaginary rhs)
		= handle (Rational (lhs / rhs))
	(/) (Rational lhs) (Complex rhsRe rhsIm)
		# denominator = rhsRe * rhsRe + rhsIm * rhsIm
		= handle (Complex (lhs * rhsRe / denominator) (~(lhs * rhsIm / denominator)))
	(/) (Imaginary lhs) (Complex rhsRe rhsIm)
		# denominator = rhsRe * rhsRe + rhsIm * rhsIm
		= handle (Complex (lhs * rhsIm / denominator) (lhs * rhsRe / denominator))
	(/) (Complex lhsRe lhsIm) (Rational rhs)
		= handle (Complex (lhsRe / rhs) (lhsIm / rhs))
	(/) (Complex lhsRe lhsIm) (Imaginary rhs)
		= handle (Complex (lhsIm / rhs) (~(lhsRe / rhs)))
	(/) (Complex lhsRe lhsIm) (Complex rhsRe rhsIm)
		# denominator = rhsRe * rhsRe + rhsIm * rhsIm
		= handle (Complex ((lhsRe * rhsRe + lhsIm * rhsIm) / denominator) ((lhsIm * rhsRe - lhsRe * rhsIm) / denominator))
		
instance one Number where
	one = (Rational (Int 1))
	
instance ^ Number where
	(^) NaN _ = NaN
	(^) _ NaN = NaN
	(^) _ Infinity = NaN
	(^) _ Zero = one
	(^) Infinity (Rational rhs)
		= case (sign rhs) of
			1 = Infinity
			0 = /*one*/ abort "tell Ourous you found a bug: Unhandled Zero at Exponential Infinity" // this should never happen
			-1 = Zero
	(^) Infinity (Imaginary _) = abort "Cannot raise `Infinity` to an Imaginary power"
	(^) Infinity (Complex _ _) = abort "Cannot raise `Infinity` to a Complex power"
	(^) Zero (Rational rhs)
		= case (sign rhs) of
			1 = Zero
			0 = /*one*/ abort "tell Ourous you found a bug: Unhandled Zero at Exponential Zero" // this should never happen
			-1 = Infinity
	(^) Zero (Imaginary _) = abort "Cannot raise `Zero` to an Imaginary power"
	(^) Zero (Complex _ _) = abort "Cannot raise `Zero` to a Complex power"
	(^) (Rational lhs) (Rational rhs)
		= handle (Rational (lhs ^ rhs))
	(^) (Rational _) (Imaginary _) = abort "Unimplemented Operation: Re^Im"
	(^) (Imaginary _) (Rational _) = abort "Unimplemented Operation: Im^Re"
	(^) (Imaginary _) (Imaginary _) = abort "Unimplemented Operation: Im^Im"
	(^) (Rational _) (Complex _ _) = abort "Unimplemented Operation: Re^Cx"
	(^) (Imaginary _) (Complex _ _) = abort "Unimplemented Operation: Im^Cx"
	(^) (Complex _ _) (Rational _) = abort "Unimplemented Operation: Cx^Re"
	(^) (Complex _ _) (Imaginary _) = abort "Unimplemented Operation: Cx^Im"
	(^) (Complex _ _) (Complex _ _) = abort "Unimplemented Operation: Cx^Cx"
		
instance abs Number where
	abs NaN = NaN
	abs Zero = Zero
	abs Infinity = Infinity
	abs (Rational val) = (Rational (abs val))
	abs (Imaginary val) = (Rational (abs val))
	abs (Complex re im) = (Rational ((re * re + im * im)^(Real 0.5)))
	
instance ~ Number where
	(~) NaN = NaN
	(~) Zero = Zero
	(~) Infinity = Infinity
	(~) (Rational val) = (Rational (~ val))
	(~) (Imaginary val) = (Imaginary (~ val))
	(~) (Complex re im) = (Complex re (~ im))
	
instance == Number where
	(==) NaN NaN = False
	(==) Zero Zero = True
	(==) Infinity Infinity = False
	(==) (Rational lhs) (Rational rhs) = lhs == rhs
	(==) (Imaginary lhs) (Imaginary rhs) = lhs == rhs
	(==) (Complex lhsRe lhsIm) (Complex rhsRe rhsIm) = lhsRe == rhsRe && lhsIm == rhsIm
	(==) _ _ = False
	
instance < Number where
	(<) NaN _ = False
	(<) _ NaN = False
	(<) Infinity _ = True
	(<) _ Infinity = True
	(<) Zero Zero = False
	(<) Zero (Rational rhs) = sign rhs == 1
	(<) (Rational lhs) Zero = sign lhs == -1
	(<) Zero (Imaginary rhs) = sign rhs == 1
	(<) (Imaginary lhs) Zero = sign lhs == -1
	(<) (Rational lhs) (Rational rhs) = lhs < rhs
	(<) (Imaginary lhs) (Imaginary rhs) = lhs < rhs
	(<) (Complex lhsRe lhsIm) (Complex rhsRe rhsIm) = lhsRe < rhsRe && lhsIm < rhsIm
	
instance mod Number where
	