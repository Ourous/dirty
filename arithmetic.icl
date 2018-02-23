implementation module arithmetic

import types, atomics, StdBool, StdOverloaded, StdInt, StdReal, StdClass, Math.Geometry, StdString
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
	
instance sign Sign where
	sign Positive = 1
	sign Negative = -1
	
VAL_SIGN val
	:== case val of
		(Inf val) = val
		(Fin val) = FIN_SIGN val
	
FIN_SIGN numeric
	:== if(sign numeric < 0) Negative Positive

IS_ZERO numeric
	:== case numeric of
		(Int 0) = True
		(Real 0.0) = True
		(Real -0.0) = True
		_ = False

IS_INF numeric
	:== case numeric of
		(Real val) = val <> 0.0
//		_ = True

IS_FIN numeric // or nan
	:== case numeric of
		(Real val) = isFinite val
		_ = True

// number implementations

handle number
	:== case number of
		(Re (Fin val)) = handleRe val
		(Im (Fin val)) = handleIm val
		(Cx (Fin {re, im})) = handleCx re im
		val = val
where
	handleRe val
		| IS_FIN val
			| IS_ZERO val = Zero
			| otherwise = (Re (Fin val))
		| IS_INF val = (Re (Inf (FIN_SIGN val)))
		| otherwise = NaN
	handleIm val
		| IS_FIN val
			| IS_ZERO val = Zero
			| otherwise = (Im (Fin val))
		| IS_INF val = (Im (Inf (FIN_SIGN val)))
		| otherwise = NaN
		// TODO : find good tests for complex number performance so I can rewrite it properly
	handleCx re im
		| IS_FIN re
			| IS_FIN im
				| IS_ZERO re
					| IS_ZERO im = Zero
					| otherwise = (Im (Fin im))
				| IS_ZERO im = (Re (Fin re))
				| otherwise = (Cx (Fin {re=re, im=im}))
			| IS_INF im = (Im (Inf (FIN_SIGN im)))
			| otherwise = NaN
		| IS_INF re
			| IS_INF im = (Cx (Inf Directed))
			| otherwise = (Re (Inf (FIN_SIGN re)))
		| otherwise = NaN
		
		
instance + Number where
	(+) NaN _ = NaN
	(+) _ NaN = NaN
	(+) Zero val = val
	(+) val Zero = val
	(+) (Re (Inf lhs)) (Re (Inf rhs))
		| lhs == rhs = (Re (Inf lhs))
		| otherwise = NaN
	(+) (Im (Inf lhs)) (Im (Inf rhs))
		| lhs == rhs = (Im (Inf lhs))
		| otherwise = NaN
	(+) (Cx (Inf _)) _ = (Cx (Inf Directed))
	(+) _ (Cx (Inf _)) = (Cx (Inf Directed))
	(+) (Im (Inf _)) (Re (Inf _)) = (Cx (Inf Directed))
	(+) (Re (Inf _)) (Im (Inf _)) = (Cx (Inf Directed))
	(+) (Im (Inf lhs)) _ = (Im (Inf lhs))
	(+) _ (Im (Inf rhs)) = (Im (Inf rhs))
	(+) (Re (Inf lhs)) _ = (Re (Inf lhs))
	(+) _ (Re (Inf rhs)) = (Re (Inf rhs))
	(+) (Re (Fin lhs)) (Re (Fin rhs))
		= handle (Re (Fin (lhs + rhs)))
	(+) (Re (Fin lhs)) (Im (Fin rhs))
		= (Cx (Fin {re=lhs, im=rhs}))
	(+) (Im (Fin lhs)) (Re (Fin rhs))
		= (Cx (Fin {re=rhs, im=lhs}))
	(+) (Im (Fin lhs)) (Im (Fin rhs))
		= handle (Im (Fin (lhs + rhs)))
	(+) (Re (Fin lhs)) (Cx (Fin rhs))
		= handle (Cx (Fin {rhs& re=lhs+rhs.re}))
	(+) (Im (Fin lhs)) (Cx (Fin rhs))
		= handle (Cx (Fin {rhs& im=lhs+rhs.im}))
	(+) (Cx (Fin lhs)) (Re (Fin rhs))
		= handle (Cx (Fin {lhs& re=lhs.re+rhs}))
	(+) (Cx (Fin lhs)) (Im (Fin rhs))
		= handle (Cx (Fin {lhs& im=lhs.im+rhs}))
	(+) (Cx (Fin lhs)) (Cx (Fin rhs))
		= handle (Cx (Fin {re=lhs.re+rhs.re, im=lhs.im+rhs.im}))

instance - Number where
	(-) NaN _ = NaN
	(-) _ NaN = NaN
	(-) lhs Zero = lhs
	(-) (Re (Inf lhs)) (Re (Inf rhs))
		| lhs <> rhs = (Re (Inf lhs))
		| otherwise = NaN
	(-) (Im (Inf lhs)) (Im (Inf rhs))
		| lhs <> rhs = (Im (Inf lhs))
		| otherwise = NaN
	(-) (Re (Inf _)) (Im (Inf _)) = (Cx (Inf Directed))
	(-) (Im (Inf _)) (Re (Inf _)) = (Cx (Inf Directed))
	(-) (Cx (Inf _)) _ = (Cx (Inf Directed))
	(-) _ (Cx (Inf _)) = (Cx (Inf Directed))
	(-) (Im (Inf lhs)) _ = (Im (Inf lhs))
	(-) _ (Im (Inf rhs)) = (Im (Inf (~rhs)))
	(-) (Re (Inf lhs)) _ = (Re (Inf lhs))
	(-) _ (Re (Inf rhs)) = (Re (Inf (~rhs)))
	(-) Zero rhs = ~rhs // this is down here for performance
	(-) (Re (Fin lhs)) (Re (Fin rhs))
		= handle (Re (Fin (lhs - rhs)))
	(-) (Re (Fin lhs)) (Im (Fin rhs))
		= (Cx (Fin {re=lhs, im=(~rhs)}))
	(-) (Im (Fin lhs)) (Re (Fin rhs))
		= (Cx (Fin {re=(~rhs), im=lhs}))
	(-) (Im (Fin lhs)) (Im (Fin rhs))
		= handle (Im (Fin (lhs - rhs)))
	(-) (Re (Fin lhs)) (Cx (Fin rhs))
		= handle (Cx (Fin {re=lhs-rhs.re, im=(~rhs.im)}))
	(-) (Im (Fin lhs)) (Cx (Fin rhs))
		= handle (Cx (Fin {re=(~rhs.re), im=lhs-rhs.im}))
	(-) (Cx (Fin lhs)) (Re (Fin rhs))
		= handle (Cx (Fin {lhs& re=lhs.re-rhs}))
	(-) (Cx (Fin lhs)) (Im (Fin rhs))
		= handle (Cx (Fin {lhs& im=lhs.im-rhs}))
	(-) (Cx (Fin lhs)) (Cx (Fin rhs))
		= handle (Cx (Fin {re=lhs.re-rhs.re, im=lhs.im-rhs.im}))
		
instance zero Number where
	zero = Zero
	
instance * Number where
	(*) NaN _ = NaN
	(*) _ NaN = NaN
	(*) Zero _ = Zero
	(*) _ Zero = Zero
	(*) (Cx (Inf _)) _ = (Cx (Inf Directed))
	(*) _ (Cx (Inf _)) = (Cx (Inf Directed))
	(*) (Re (Fin lhs)) (Re (Fin rhs))
		= (Re (Fin (lhs * rhs)))
	(*) (Re (Fin lhs)) (Im (Fin rhs))
		= (Im (Fin (lhs * rhs)))
	(*) (Im (Fin lhs)) (Re (Fin rhs))
		= (Im (Fin (lhs * rhs)))
	(*) (Im (Fin lhs)) (Im (Fin rhs))
		= (Re (Fin (~(lhs * rhs))))
	(*) (Re lhs) (Re rhs) = (Re (Inf (VAL_SIGN lhs * VAL_SIGN rhs)))
	(*) (Re lhs) (Im rhs) = (Im (Inf (VAL_SIGN lhs * VAL_SIGN rhs)))
	(*) (Im lhs) (Re rhs) = (Im (Inf (VAL_SIGN lhs * VAL_SIGN rhs)))
	(*) (Im lhs) (Im rhs) = (Im (Inf (~(VAL_SIGN lhs * VAL_SIGN rhs))))
	(*) (Re (Fin lhs)) (Cx (Fin rhs))
		= (Cx (Fin {re=lhs*rhs.re, im=lhs*rhs.im}))
	(*) (Im (Fin lhs)) (Cx (Fin rhs))
		= (Cx (Fin {re=(~(lhs*rhs.im)), im=lhs*rhs.re}))
	(*) (Cx (Fin lhs)) (Re (Fin rhs))
		= (Cx (Fin {re=lhs.re*rhs, im=lhs.im*rhs}))
	(*) (Cx (Fin lhs)) (Im (Fin rhs))
		= (Cx (Fin {re=(~(lhs.im*rhs)), im=lhs.re*rhs}))
	(*) (Cx (Fin lhs)) (Cx (Fin rhs))
		= handle (Cx (Fin {re=lhs.re*rhs.re-lhs.im*rhs.im, im=lhs.im*rhs.re+rhs.im*lhs.re}))
		
instance / Number where
	(/) NaN _ = NaN
	(/) _ NaN = NaN
	(/) Zero Zero = NaN
	(/) Zero _ = Zero
	(/) (Re lhs) Zero = (Re (Inf (VAL_SIGN lhs)))
	(/) (Im lhs) Zero = (Im (Inf (VAL_SIGN lhs)))
	(/) (Cx _) Zero = (Cx (Inf Directed))
	(/) (Re (Inf lhs)) (Re (Fin rhs)) = (Re (Inf (lhs * FIN_SIGN rhs)))
	(/) (Re (Inf lhs)) (Im (Fin rhs)) = (Im (Inf (~(lhs * FIN_SIGN rhs))))
	(/) (Im (Inf lhs)) (Re (Fin rhs)) = (Im (Inf (lhs * FIN_SIGN rhs)))
	(/) (Im (Inf lhs)) (Im (Fin rhs)) = (Re (Inf (lhs * FIN_SIGN rhs)))
	(/) (Re (Inf _)) (Cx (Fin _)) = (Cx (Inf Directed))
	(/) (Im (Inf _)) (Cx (Fin _)) = (Cx (Inf Directed))
	(/) (Cx (Inf _)) (Re (Fin _)) = (Cx (Inf Directed))
	(/) (Cx (Inf _)) (Im (Fin _)) = (Cx (Inf Directed))
	(/) (Cx (Inf _)) (Cx (Fin _)) = (Cx (Inf Directed))
	(/) (Re (Fin _)) (Re (Inf _)) = Zero
	(/) (Re (Fin _)) (Im (Inf _)) = Zero
	(/) (Im (Fin _)) (Re (Inf _)) = Zero
	(/) (Im (Fin _)) (Im (Inf _)) = Zero
	(/) (Re (Fin _)) (Cx (Inf _)) = Zero
	(/) (Im (Fin _)) (Cx (Inf _)) = Zero
	(/) (Cx (Fin _)) (Re (Inf _)) = Zero
	(/) (Cx (Fin _)) (Im (Inf _)) = Zero
	(/) (Cx (Fin _)) (Cx (Inf _)) = Zero
	(/) (Re (Fin lhs)) (Re (Fin rhs))
		= (Re (Fin (lhs / rhs)))
	(/) (Re (Fin lhs)) (Im (Fin rhs))
		= (Im (Fin (~(lhs / rhs))))
	(/) (Im (Fin lhs)) (Re (Fin rhs))
		= (Im (Fin (lhs / rhs)))
	(/) (Im (Fin lhs)) (Im (Fin rhs))
		= (Re (Fin (lhs / rhs)))
	(/) (Re (Fin lhs)) (Cx (Fin rhs))
		# denominator = rhs.re * rhs.re + rhs.im * rhs.im
		= (Cx (Fin {re=((lhs * rhs.re) / denominator), im=(~((lhs * rhs.im) / denominator))}))
	(/) (Im (Fin lhs)) (Cx (Fin rhs))
		# denominator = rhs.re * rhs.re + rhs.im * rhs.im
		= (Cx (Fin {re=((lhs * rhs.im) / denominator), im=((lhs * rhs.re) / denominator)}))
	(/) (Cx (Fin lhs)) (Re (Fin rhs))
		= (Cx (Fin {re=(lhs.re / rhs), im=(lhs.im / rhs)}))
	(/) (Cx (Fin lhs)) (Im (Fin rhs))
		= (Cx (Fin {re=(lhs.im / rhs), im=(~(lhs.re / rhs))}))
	(/) (Cx (Fin lhs)) (Cx (Fin rhs))
		# denominator = rhs.re * rhs.re + rhs.im * rhs.im
		= handle (Cx (Fin {re=((lhs.re * rhs.re + lhs.im * rhs.im) / denominator), im=((lhs.im * rhs.re - lhs.re * rhs.im) / denominator)}))
	(/) _ _ = NaN
	
instance one Number where
	one = (Re (Fin (Int 1)))

instance ^ Number where
	(^) NaN _ = NaN
	(^) _ NaN = NaN
	(^) Zero _ = Zero
	(^) _ Zero = one
	(^) (Re (Fin lhs)) (Re (Fin rhs))
		= (Re (Fin (lhs ^ rhs)))
	//(^) (Rational _) (Imaginary _) = abort "Unimplemented Operation: Re^Im"
	//(^) (Imaginary _) (Rational _) = abort "Unimplemented Operation: Im^Re"
	//(^) (Imaginary _) (Imaginary _) = abort "Unimplemented Operation: Im^Im"
	//(^) (Rational _) (Complex _ _) = abort "Unimplemented Operation: Re^Cx"
	//(^) (Imaginary _) (Complex _ _) = abort "Unimplemented Operation: Im^Cx"
	//(^) (Complex _ _) (Rational _) = abort "Unimplemented Operation: Cx^Re"
	//(^) (Complex _ _) (Imaginary _) = abort "Unimplemented Operation: Cx^Im"
	//(^) (Complex _ _) (Complex _ _) = abort "Unimplemented Operation: Cx^Cx"

instance abs Number where
	abs NaN = NaN
	abs Zero = Zero
	abs (Re (Fin val)) = (Re (Fin (abs val)))
	abs (Im (Fin val)) = (Re (Fin (abs val)))
	abs (Cx (Fin val)) = (Re (Fin (sqrt(val.re*val.re+val.im*val.im))))
	abs _ = (Re (Inf Positive))
	
instance ~ Number where
	(~) NaN = NaN
	(~) Zero = Zero
	(~) (Re (Inf val)) = (Re (Inf (~val)))
	(~) (Re (Fin val)) = (Re (Fin (~val)))
	(~) (Im (Inf val)) = (Im (Inf (~val)))
	(~) (Im (Fin val)) = (Im (Fin (~val)))
	(~) (Cx (Inf Directed)) = (Cx (Inf Directed))
	(~) (Cx (Fin val)) = (Cx (Fin {re=(~val.re), im=(~val.im)}))
	
instance == Number where
	(==) Zero Zero = True
	(==) (Re (Fin lhs)) (Re (Fin rhs)) = lhs == rhs
	(==) (Im (Fin lhs)) (Im (Fin rhs)) = lhs == rhs
	(==) (Cx (Fin lhs)) (Cx (Fin rhs)) = lhs.re == rhs.re && lhs.im == rhs.im
	(==) _ _ = False

instance < Number where
	(<) NaN _ = False
	(<) _ NaN = False
	//(<) Infinity _ = True
	//(<) _ Infinity = True
	(<) Zero Zero = False
	(<) Zero (Re rhs) = VAL_SIGN rhs == Positive
	(<) (Re lhs) Zero = VAL_SIGN lhs == Negative
	(<) Zero (Im rhs) = VAL_SIGN rhs == Positive
	(<) (Im lhs) Zero = VAL_SIGN lhs == Negative
	(<) (Re (Fin lhs)) (Re (Fin rhs)) = lhs < rhs
	(<) (Im (Fin lhs)) (Im (Fin rhs)) = lhs < rhs
	(<) (Cx (Fin lhs)) (Cx (Fin rhs)) = lhs.re < rhs.re && lhs.im < rhs.im
/*
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
*/
instance toInt Number where
	toInt Zero = 0
	toInt (Re (Fin val)) = toInt val
	toInt (Im (Fin val)) = toInt val
	toInt (Cx (Fin {re, im})) = toInt (sqrt (re*re + im*im)) * sign re * sign im
	
instance toReal Number where
	toReal Zero = 0.0
	toReal (Re (Inf val)) = (toReal (sign val)) / 0.0
	toReal (Re (Fin val)) = toReal val
	toReal (Im (Inf val)) = (toReal (sign val)) / 0.0
	toReal (Im (Fin val)) = toReal val
	toReal (Cx (Fin {re, im})) = toReal (sqrt (re*re + im*im)) * (toReal (sign re * sign im))
	toReal _ = 0.0/0.0
	
instance toBool Number where
	toBool Zero = False
	toBool NaN = False
	toBool _ = True
	
instance toString Number where
	toString Zero = "0"
	toString NaN = "NaN"
	toString (Re (Inf val)) = if(val == Negative) "-ReInf" "ReInf"
	toString (Re (Fin val)) = toString val
	toString (Im (Inf val)) = if(val == Negative) "-ImInf" "ImInf"
	toString (Im (Fin val)) = toString val +++ "i"
	toString (Cx (Inf _ )) = "?CxInf"
	toString (Cx (Fin {re, im})) = toString re +++ (if(sign im == -1) "-" "+") +++ toString (abs im) +++ "i"
	
instance fromInt Number where fromInt val = (Re (Fin (Int val)))

instance fromReal Number where fromReal val = (Re (Fin (Real val)))

instance fromBool Number where
	fromBool True = (Re (Fin (Int -1)))
	fromBool False = Zero

instance ln Number where
	ln NaN = NaN
	//ln Infinity = Infinity
	ln Zero = NaN
	ln (Re (Fin val)) = handle (Re (Fin (ln val)))
	//ln (Imaginary _) = abort "Unimplemented Operation: ln Im"
	//ln (Complex _ _) = abort "Unimplemented Operation: ln Cx"
	
instance log10 Number where
	log10 NaN = NaN
	//log10 Infinity = Infinity
	log10 Zero = NaN
	log10 (Re (Fin val)) = handle (Re (Fin (log10 val)))
	//log10 (Imaginary _) = abort "Unimplemented Operation: log10 Im"
	//log10 (Complex _ _) = abort "Unimplemented Operation: log10 Cx"
	
instance exp Number where
	exp NaN = NaN
	//exp Infinity = Infinity
	exp Zero = one
	exp (Re (Fin val)) = handle (Re (Fin (exp val)))
	//exp (Imaginary _) = abort "Unimplemented Operation: exp Im"
	//exp (Complex _ _) = abort "Unimplemented Operation: exp Cx"
	
instance sqrt Number where
	sqrt NaN = NaN
	//sqrt Infinity = Infinity
	sqrt Zero = Zero
	sqrt (Re (Fin val))
		| FIN_SIGN val <> Negative
			= handle (Re (Fin (sqrt val)))
		| otherwise
			= handle (Im (Fin (sqrt (abs val))))
	//sqrt (Imaginary _) = abort "Unimplemented Operation: sqrt Im"
	//sqrt (Complex _ _) = abort "Unimplemented Operation: sqrt Cx"
	
instance sin Number where
	sin NaN = NaN
	//sin Infinity = NaN
	sin Zero = Zero
	sin (Re (Fin val)) = handle (Re (Fin (sin val)))
	//sin (Imaginary _) = abort "Unimplemented Operation: sin Im"
	//sin (Complex _ _) = abort "Unimplemented Operation: sin Cx"
	
instance cos Number where
	cos NaN = NaN
	//cos Infinity = NaN
	cos Zero = one
	cos (Re (Fin val)) = handle (Re (Fin (cos val)))
	//cos (Imaginary _) = abort "Unimplemented Operation: cos Im"
	//cos (Complex _ _) = abort "Unimplemented Operation: cos Cx"
	
instance tan Number where
	tan NaN = NaN
	//tan Infinity = NaN
	tan Zero = Zero
	tan (Re (Fin val)) = handle (Re (Fin (tan val)))
	//tan (Imaginary _) = abort "Unimplemented Operation: tan Im"
	//tan (Complex _ _) = abort "Unimplemented Operation: tan Cx"
	
instance asin Number where
	asin NaN = NaN
	//asin Infinity = Infinity
	asin Zero = Zero
	asin (Re (Fin val)) = handle (Re (Fin (asin val)))
	//asin (Imaginary _) = abort "Unimplemented Operation: asin Im"
	//asin (Complex _ _) = abort "Unimplemented Operation: asin Cx"
	
instance acos Number where
	acos NaN = NaN
	//acos Infinity = Infinity
	acos Zero = (Re (Fin (Real (pi/2.0))))
	acos (Re (Fin val)) = handle (Re (Fin (acos val)))
	//acos (Imaginary _) = abort "Unimplemented Operation: acos Im"
	//acos (Complex _ _) = abort "Unimplemented Operation: acos Cx"
	
instance atan Number where
	atan NaN = NaN
	//atan Infinity = (Rational (Real (pi/2.0)))
	atan Zero = Zero
	atan (Re (Fin val)) = handle (Re (Fin (atan val)))
	//atan (Imaginary _) = abort "Unimplemented Operation: atan Im"
	//atan (Complex _ _) = abort "Unimplemented Operation: atan Cx"
	
INT_OPER op lhs rhs :== (Int (op (ENTIER lhs) (ENTIER rhs)))

ENTIER val
	:== case val of
		(Int i) = i
		(Real r) = entier r

bitOR :: !Number !Number -> Number
bitOR NaN _ = NaN
bitOR _ NaN = NaN
bitOR Zero rhs = rhs
bitOR lhs Zero = lhs
bitOR (Re (Inf _)) (Im (Inf _)) = (Cx (Inf Directed))
bitOR (Im (Inf _)) (Re (Inf _)) = (Cx (Inf Directed))
bitOR (Re (Fin lhs)) (Re (Fin rhs))
	= handle (Re (Fin (INT_OPER (bitor) lhs rhs)))
bitOR (Re (Fin lhs)) (Im (Fin rhs))
	= (Cx (Fin {re=lhs, im=rhs}))
bitOR (Im (Fin lhs)) (Re (Fin rhs))
	= (Cx (Fin {re=rhs, im=lhs}))	
bitOR (Im (Fin lhs)) (Im (Fin rhs))
	= handle (Im (Fin (INT_OPER (bitor) lhs rhs)))
bitOR (Re (Fin lhs)) (Cx (Fin rhs))
	= handle (Cx (Fin {rhs&re=INT_OPER (bitor) lhs rhs.re}))
bitOR (Im (Fin lhs)) (Cx (Fin rhs))
	= handle (Cx (Fin {rhs&im=INT_OPER (bitor) lhs rhs.im}))
bitOR (Cx (Fin lhs)) (Re (Fin rhs))
	= handle (Cx (Fin {lhs&re=INT_OPER (bitor) lhs.re rhs}))
bitOR (Cx (Fin lhs)) (Im (Fin rhs))
	= handle (Cx (Fin {lhs&im=INT_OPER (bitor) lhs.im rhs}))
bitOR (Cx (Fin lhs)) (Cx (Fin rhs))
	= handle (Cx (Fin {re=INT_OPER (bitor) lhs.re rhs.re, im=INT_OPER (bitor) lhs.im rhs.im}))

bitAND :: !Number !Number -> Number
bitAND NaN _ = NaN
bitAND _ NaN = NaN
bitAND Zero _ = Zero
bitAND _ Zero = Zero
bitAND (Re _) (Im _) = Zero
bitAND (Im _) (Re _) = Zero
bitAND (Re (Fin lhs)) (Re (Fin rhs))
	= handle (Re (Fin (INT_OPER (bitand) lhs rhs)))
bitAND (Im (Fin lhs)) (Im (Fin rhs))
	= handle (Im (Fin (INT_OPER (bitand) lhs rhs)))
bitAND (Re (Fin lhs)) (Cx (Fin rhs))
	= handle (Re (Fin (INT_OPER (bitand) lhs rhs.re)))
bitAND (Im (Fin lhs)) (Cx (Fin rhs))
	= handle (Im (Fin (INT_OPER (bitand) lhs rhs.im)))
bitAND (Cx (Fin lhs)) (Re (Fin rhs))
	= handle (Re (Fin (INT_OPER (bitand) lhs.re rhs)))
bitAND (Cx (Fin lhs)) (Im (Fin rhs))
	= handle (Im (Fin (INT_OPER (bitand) lhs.im rhs)))
bitAND (Cx (Fin lhs)) (Cx (Fin rhs))
	= handle (Cx (Fin {re=INT_OPER (bitand) lhs.re rhs.re, im=INT_OPER (bitand) lhs.im rhs.im}))
bitAND _ _ = NaN

bitXOR :: !Number !Number -> Number
bitXOR NaN _ = NaN
bitXOR _ NaN = NaN
bitXOR Zero rhs = rhs
bitXOR lhs Zero = lhs
bitXOR (Re (Fin lhs)) (Re (Fin rhs))
	= handle (Re (Fin (INT_OPER (bitxor) lhs rhs)))
bitXOR (Re (Fin lhs)) (Im (Fin rhs))
	= (Cx (Fin {re=lhs, im=rhs}))
bitXOR (Im (Fin lhs)) (Re (Fin rhs))
	= (Cx (Fin {re=rhs, im=lhs}))
bitXOR (Im (Fin lhs)) (Im (Fin rhs))
	= handle (Im (Fin (INT_OPER (bitxor) lhs rhs)))
bitXOR (Re (Fin lhs)) (Cx (Fin rhs))
	= handle (Cx (Fin {rhs&re=INT_OPER (bitxor) lhs rhs.re}))
bitXOR (Im (Fin lhs)) (Cx (Fin rhs))
	= handle (Cx (Fin {rhs&im=INT_OPER (bitxor) lhs rhs.im}))
bitXOR (Cx (Fin lhs)) (Re (Fin rhs))
	= handle (Cx (Fin {lhs&re=INT_OPER (bitxor) lhs.re rhs}))
bitXOR (Cx (Fin lhs)) (Im (Fin rhs))
	= handle (Cx (Fin {lhs&im=INT_OPER (bitxor) lhs.im rhs}))
bitXOR (Cx (Fin lhs)) (Cx (Fin rhs))
	= handle (Cx (Fin {re=INT_OPER (bitxor) lhs.re rhs.re, im=INT_OPER (bitxor) lhs.im rhs.im}))

bitNOT :: !Number -> Number
bitNOT NaN = NaN
bitNOT Zero = (Re (Fin (Int -1)))
bitNOT (Re (Fin val)) = handle (Re (Fin (Int (bitnot (ENTIER val)))))
bitNOT (Im (Fin val)) = handle (Im (Fin (Int (bitnot (ENTIER val)))))
bitNOT (Cx (Fin {re, im})) = handle (Cx (Fin {re=(Int(bitnot(ENTIER re))), im=(Int(bitnot(ENTIER im)))}))
bitNOT _ = NaN


numFLOOR :: !Number -> Number
numFLOOR (Re (Fin val)) = handle (Re (Fin (Int (ENTIER val))))
numFLOOR (Im (Fin val)) = handle (Im (Fin (Int (ENTIER val))))
numFLOOR (Cx (Fin {re, im})) = handle (Cx (Fin {re=(Int (ENTIER re)), im=(Int (ENTIER im))}))
numFLOOR val = val

numCEILING :: !Number -> Number
numCEILING (Re (Fin val)) = handle (Re (Fin (Int (~(ENTIER (~val))))))
numCEILING (Im (Fin val)) = handle (Im (Fin (Int (~(ENTIER (~val))))))
numCEILING (Cx (Fin {re, im})) = handle (Cx (Fin {re=(Int(~(ENTIER(~re)))), im=(Int(~(ENTIER(~im))))}))
numCEILING val = val

numROUND :: !Number -> Number
numROUND (Re (Fin val)) = handle (Re (Fin (Int (toInt val))))
numROUND (Im (Fin val)) = handle (Im (Fin (Int (toInt val))))
numROUND (Cx (Fin {re, im})) = handle (Cx (Fin {re=(Int (toInt re)), im=(Int (toInt im))}))
numROUND val = val
/*
toRadians :: Number -> Number
toRadians _ = abort "Unimplemented Operation: toRadians"
toDegrees :: Number -> Number
toDegrees _ = abort "Unimplemented Operation: toDegrees"*/