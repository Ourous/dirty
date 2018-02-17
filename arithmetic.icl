implementation module arithmetic

import types, atomics, StdBool, StdOverloaded, StdInt, StdReal, StdClass, Math.Geometry
from StdLibMisc import isFinite
from StdMisc import abort

instance == Sign where
	(==) Positive Positive = True
	(==) Negative Negative = True
	(==) _ _ = False

instance ~ Sign where
	~ Positive = Negative
	~ Negative = Positive
	
instance * Sign where
	(*) Positive Positive = Positive
	(*) Negative Negative = Positive
	(*) _ _ = Negative
	
TO_SIGN numeric
	:== if(sign numeric < 0) Negative Positive

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
handle (Re (Fin val))
	| IS_ZERO val = Zero
	| IS_NAN val = NaN
	| IS_INF val = (Re (Inf (TO_SIGN val)))
	= (Re (Fin val))
handle (Im (Fin val))
	| IS_ZERO val = Zero
	| IS_NAN val = NaN
	| IS_INF val = (Im (Inf (TO_SIGN val)))
	= (Im (Fin val))
handle (Cx (Fin {re, im}))
	| IS_NAN re || IS_NAN im = NaN
	| IS_INF re
		| IS_INF im
			= (Cx (Inf Directed))
		= (Re (Inf (TO_SIGN re)))
	| IS_INF im
		= (Im (Inf (TO_SIGN im)))
	= case ((IS_ZERO re), (IS_ZERO im)) of
		(True, True) = Zero
		(True, False) = (Im (Fin im))
		(False, True) = (Re (Fin re))
		(False, False) = (Cx (Fin {re, im}))
handle val = val

instance + Number where
	(+) NaN _ = NaN
	(+) _ NaN = NaN
	(+) Zero val = val
	(+) val Zero = val
	(+) (Re (Inf lhs)) (Re (Inf rhs))
		| lhs == rhs = (Re (Inf lhs))
		= NaN
	(+) (Im (Inf lhs)) (Im (Inf rhs))
		| lhs == rhs = (Im (Inf lhs))
		| NaN
	(+) (Cx (Inf lhs)) _ = (Cx (Inf lhs))
	(+) _ (Cx (Inf rhs)) = (Cx (Inf rhs))
	(+) (Re (Inf _)) (Im (Inf _)) = (Cx (Inf Directed))
	(+) (Im (Inf _)) (Re (Inf _)) = (Cx (Inf Directed))
	(+) 
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
	(-) (Inf (Re lhs)) (Inf (Re rhs))
		| lhs <> rhs = (Inf (Re lhs))
		= NaN
	(-) (Inf (Im lhs)) (Inf (Im rhs))
		| lhs <> rhs = (Inf (Im lhs))
		= NaN
	(-) (Inf _) (Inf _) = (Inf Directed)
	(-) (Inf lhs) _ = (Inf lhs)
	(-) _ (Inf rhs) = ~(Inf rhs)
	(-) lhs Zero = lhs
	(-) Zero rhs = ~rhs // this comes second for performance
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
	(*) (Inf (Re lhs)) (Inf (Re rhs)) = (Inf (Re (lhs * rhs)))
	(*) (Inf (Re lhs)) (Rational rhs) = (Inf (Re (lhs * TO_SIGN rhs)))
	(*) (Rational lhs) (Inf (Re rhs)) = (Inf (Re (TO_SIGN lhs * rhs)))
	(*) (Inf (Re lhs)) (Inf (Im rhs)) = (Inf (Im (lhs * rhs)))
	(*) (Inf (Re lhs)) (Imaginary rhs) = (Inf (Im (lhs * TO_SIGN rhs)))
	(*) (Inf (Im lhs)) (Inf (Re rhs)) = (Inf (Im (lhs * rhs)))
	(*) (Inf (Im lhs)) (Inf (Im rhs)) = (Inf (Re (~(lhs * rhs))))
	(*) (Inf Directed) _ = (Inf Directed)
	(*) _ (Inf Directed) = (Inf Directed)
	(*) (Inf _) (Complex _ _) = (Inf Directed)
	(*) (Complex _ _) (Inf _) = (Inf Directed)
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
	(/) Zero Zero = NaN
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
		= handle (Complex ((lhs * rhsRe) / denominator) (~((lhs * rhsIm) / denominator)))
	(/) (Imaginary lhs) (Complex rhsRe rhsIm)
		# denominator = rhsRe * rhsRe + rhsIm * rhsIm
		= handle (Complex ((lhs * rhsIm) / denominator) ((lhs * rhsRe) / denominator))
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
	//(^) _ Infinity = NaN
	(^) _ Zero = one
	//(^) Infinity (Rational rhs)
	//	= case (sign rhs) of
	//		1 = Infinity
	//		0 = /*one*/ abort "tell Ourous you found a bug: Unhandled Zero at Exponential Infinity" // this should never happen
	//		-1 = Zero
	//(^) Infinity (Imaginary _) = abort "Cannot raise `Infinity` to an Imaginary power"
	//(^) Infinity (Complex _ _) = abort "Cannot raise `Infinity` to a Complex power"
	//(^) Zero (Rational rhs)
	//	= case (sign rhs) of
	//		1 = Zero
	//		0 = /*one*/ abort "tell Ourous you found a bug: Unhandled Zero at Exponential Zero" // this should never happen
	//		-1 = Infinity
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
	//abs Infinity = Infinity
	abs (Rational val) = (Rational (abs val))
	abs (Imaginary val) = (Rational (abs val))
	abs (Complex re im) = (Rational ((re * re + im * im)^(Real 0.5)))
	
instance ~ Number where
	(~) NaN = NaN
	(~) Zero = Zero
	//(~) Infinity = Infinity
	(~) (Rational val) = (Rational (~ val))
	(~) (Imaginary val) = (Imaginary (~ val))
	(~) (Complex re im) = (Complex re (~ im))
	
instance == Number where
	(==) NaN NaN = False
	(==) Zero Zero = True
	//(==) Infinity Infinity = False
	(==) (Rational lhs) (Rational rhs) = lhs == rhs
	(==) (Imaginary lhs) (Imaginary rhs) = lhs == rhs
	(==) (Complex lhsRe lhsIm) (Complex rhsRe rhsIm) = lhsRe == rhsRe && lhsIm == rhsIm
	(==) _ _ = False
	
instance < Number where
	(<) NaN _ = False
	(<) _ NaN = False
	//(<) Infinity _ = True
	//(<) _ Infinity = True
	(<) Zero Zero = False
	(<) Zero (Rational rhs) = sign rhs == 1
	(<) (Rational lhs) Zero = sign lhs == -1
	(<) Zero (Imaginary rhs) = sign rhs == 1
	(<) (Imaginary lhs) Zero = sign lhs == -1
	(<) (Rational lhs) (Rational rhs) = lhs < rhs
	(<) (Imaginary lhs) (Imaginary rhs) = lhs < rhs
	(<) (Complex lhsRe lhsIm) (Complex rhsRe rhsIm) = lhsRe < rhsRe && lhsIm < rhsIm
	
instance mod Number where
	(mod) NaN _ = NaN
	(mod) _ NaN = NaN
	(mod) _ Zero = NaN
	(mod) Zero _ = Zero
	//(mod) Infinity _ = NaN
	//(mod) val Infinity = val
	(mod) (Rational lhs) (Rational rhs)
		= handle (Rational (lhs mod rhs))
	(mod) (Rational lhs) (Imaginary rhs)
		= handle (Imaginary (~(lhs mod rhs)))
	(mod) (Imaginary lhs) (Rational rhs)
		= handle (Imaginary (lhs mod rhs))
	(mod) (Imaginary lhs) (Imaginary rhs)
		= handle (Rational (lhs mod rhs))
	(mod) (Rational lhs) (Complex rhsRe rhsIm)
		# denominator = rhsRe * rhsRe + rhsIm * rhsIm
		= handle (Complex ((lhs * rhsRe) mod denominator) (~((lhs * rhsIm) mod denominator)))
	(mod) (Imaginary lhs) (Complex rhsRe rhsIm)
		# denominator = rhsRe * rhsRe + rhsIm * rhsIm
		= handle (Complex ((lhs * rhsIm) mod denominator) ((lhs * rhsRe) mod denominator))
	(mod) (Complex lhsRe lhsIm) (Rational rhs)
		= handle (Complex (lhsRe mod rhs) (lhsIm mod rhs))
	(mod) (Complex lhsRe lhsIm) (Imaginary rhs)
		= handle (Complex (lhsIm mod rhs) (~(lhsRe mod rhs)))
	(mod) (Complex lhsRe lhsIm) (Complex rhsRe rhsIm)
		# denominator = rhsRe * rhsRe + rhsIm * rhsIm
		= handle (Complex ((lhsRe * rhsRe + lhsIm * rhsIm) mod denominator) ((lhsIm * rhsRe - lhsRe * rhsIm) mod denominator))
	
instance gcd Number where
	gcd NaN _ = NaN
	gcd _ NaN = NaN
	//gcd Infinity _ = NaN
	//gcd _ Infinity = NaN
	gcd Zero _ = NaN
	gcd _ Zero = NaN
	gcd (Rational lhs) (Rational rhs)
		= handle (Rational (gcd lhs rhs))
	gcd (Rational lhs) (Imaginary rhs)
		= handle (Rational (gcd lhs rhs))
	gcd (Imaginary lhs) (Rational rhs)
		= handle (Rational (gcd lhs rhs))
	gcd (Imaginary lhs) (Imaginary rhs)
		= handle (Imaginary (gcd lhs rhs))
	gcd (Rational _) (Complex _ _) = abort "Unimplemented Operation: gcd Re Cx"
	gcd (Imaginary _) (Complex _ _) = abort "Unimplemented Operation: gcd Im Cx"
	gcd (Complex _ _) (Rational _) = abort "Unimplemented Operation: gcd Cx Re"
	gcd (Complex _ _) (Imaginary _) = abort "Unimplemented Operation: gcd Cx Im"
	gcd (Complex _ _) (Complex _ _) = abort "Unimplemented Operation: gcd Cx Cx"
	
instance lcm Number where
	lcm NaN _ = NaN
	lcm _ NaN = NaN
	//lcm Infinity _ = NaN
	//lcm _ Infinity = NaN
	lcm Zero _ = NaN
	lcm _ Zero = NaN
	lcm (Rational lhs) (Rational rhs)
		= handle (Rational (lcm lhs rhs))
	lcm (Rational lhs) (Imaginary rhs)
		= handle (Imaginary (lcm lhs rhs))
	lcm (Imaginary lhs) (Rational rhs)
		= handle (Imaginary (lcm lhs rhs))
	lcm (Imaginary lhs) (Imaginary rhs)
		= handle (Rational (lcm lhs rhs))
	lcm (Rational _) (Complex _ _) = abort "Unimplemented Operation: lcm Re Cx"
	lcm (Imaginary _) (Complex _ _) = abort "Unimplemented Operation: lcm Im Cx"
	lcm (Complex _ _) (Rational _) = abort "Unimplemented Operation: lcm Cx Re"
	lcm (Complex _ _) (Imaginary _) = abort "Unimplemented Operation: lcm Cx Im"
	lcm (Complex _ _) (Complex _ _) = abort "Unimplemented Operation: lcm Cx Cx"
	
instance toInt Number where
	toInt NaN = abort "Unimplemented Operation: toInt NaN"
	//toInt Infinity = abort "Unimplemented Operation: toInt Infinity"
	toInt Zero = 0
	toInt (Rational val) = toInt val
	toInt (Imaginary val) = toInt val
	toInt (Complex re im) = toInt (sqrt (re*re + im*im))
	
instance toReal Number where
	toReal NaN = abort "Unimplemented Operation: toReal NaN"
	//toReal Infinity = abort "Unimplemented Operation: toReal Infinity"
	toReal Zero = 0.0
	toReal (Rational val) = toReal val
	toReal (Imaginary val) = toReal val
	toReal (Complex re im) = toReal (sqrt (re*re + im*im))
	
instance fromInt Number where fromInt val = (Rational (Int val))
	
instance ln Number where
	ln NaN = NaN
	//ln Infinity = Infinity
	ln Zero = NaN
	ln (Rational val) = handle (Rational (ln val))
	ln (Imaginary _) = abort "Unimplemented Operation: ln Im"
	ln (Complex _ _) = abort "Unimplemented Operation: ln Cx"
	
instance log10 Number where
	log10 NaN = NaN
	//log10 Infinity = Infinity
	log10 Zero = NaN
	log10 (Rational val) = handle (Rational (log10 val))
	log10 (Imaginary _) = abort "Unimplemented Operation: log10 Im"
	log10 (Complex _ _) = abort "Unimplemented Operation: log10 Cx"
	
instance exp Number where
	exp NaN = NaN
	//exp Infinity = Infinity
	exp Zero = one
	exp (Rational val) = handle (Rational (exp val))
	exp (Imaginary _) = abort "Unimplemented Operation: exp Im"
	exp (Complex _ _) = abort "Unimplemented Operation: exp Cx"
	
instance sqrt Number where
	sqrt NaN = NaN
	//sqrt Infinity = Infinity
	sqrt Zero = Zero
	sqrt (Rational val) = handle (Rational (sqrt val))
	sqrt (Imaginary _) = abort "Unimplemented Operation: sqrt Im"
	sqrt (Complex _ _) = abort "Unimplemented Operation: sqrt Cx"
	
instance sin Number where
	sin NaN = NaN
	//sin Infinity = NaN
	sin Zero = Zero
	sin (Rational val) = handle (Rational (sin val))
	sin (Imaginary _) = abort "Unimplemented Operation: sin Im"
	sin (Complex _ _) = abort "Unimplemented Operation: sin Cx"
	
instance cos Number where
	cos NaN = NaN
	//cos Infinity = NaN
	cos Zero = one
	cos (Rational val) = handle (Rational (cos val))
	cos (Imaginary _) = abort "Unimplemented Operation: cos Im"
	cos (Complex _ _) = abort "Unimplemented Operation: cos Cx"
	
instance tan Number where
	tan NaN = NaN
	//tan Infinity = NaN
	tan Zero = Zero
	tan (Rational val) = handle (Rational (tan val))
	tan (Imaginary _) = abort "Unimplemented Operation: tan Im"
	tan (Complex _ _) = abort "Unimplemented Operation: tan Cx"
	
instance asin Number where
	asin NaN = NaN
	//asin Infinity = Infinity
	asin Zero = Zero
	asin (Rational val) = handle (Rational (asin val))
	asin (Imaginary _) = abort "Unimplemented Operation: asin Im"
	asin (Complex _ _) = abort "Unimplemented Operation: asin Cx"
	
instance acos Number where
	acos NaN = NaN
	//acos Infinity = Infinity
	acos Zero = (Rational (Real (pi/2.0)))
	acos (Rational val) = handle (Rational (acos val))
	acos (Imaginary _) = abort "Unimplemented Operation: acos Im"
	acos (Complex _ _) = abort "Unimplemented Operation: acos Cx"
	
instance atan Number where
	atan NaN = NaN
	//atan Infinity = (Rational (Real (pi/2.0)))
	atan Zero = Zero
	atan (Rational val) = handle (Rational (atan val))
	atan (Imaginary _) = abort "Unimplemented Operation: atan Im"
	atan (Complex _ _) = abort "Unimplemented Operation: atan Cx"
	
bitOR :: Number Number -> Number
bitOR NaN _ = NaN
bitOR _ NaN = NaN
//bitOR Infinity _ = Infinity
//bitOR _ Infinity = Infinity
bitOR Zero rhs = rhs
bitOR lhs Zero = lhs
bitOR _ _ = abort "Unimplemented Operation: bitOR"
bitAND :: Number Number -> Number
bitAND _ _ = abort "Unimplemented Operation: bitAND"
bitXOR :: Number Number -> Number
bitXOR _ _ = abort "Unimplemented Operation: bitXOR"
bitNOT :: Number Number -> Number
bitNOT _ _ = abort "Unimplemented Operation: bitNOT"
bitNOR :: Number Number -> Number
bitNOR _ _ = abort "Unimplemented Operation: bitNOR"
bitNAND :: Number Number -> Number
bitNAND _ _ = abort "Unimplemented Operation: bitNAND"
bitXNOR :: Number Number -> Number
bitXNOR _ _ = abort "Unimplemented Operation: bitXNOR"
numFLOOR :: Number -> Number
numFLOOR _ = abort "Unimplemented Operation: numFLOOR"
numCEILING :: Number -> Number
numCEILING _ = abort "Unimplemented Operation: numCEILING"
numROUND :: Number -> Number
numROUND _ = abort "Unimplemented Operation: numROUND"
toRadians :: Number -> Number
toRadians _ = abort "Unimplemented Operation: toRadians"
toDegrees :: Number -> Number
toDegrees _ = abort "Unimplemented Operation: toDegrees"