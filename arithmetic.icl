implementation module arithmetic

import types, atomics, Math.Geometry, StdLib, StdEnv

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
	
INT_MAX =: IF_INT_64_OR_32 9223372036854775807 2147483647
INT_MIN =: IF_INT_64_OR_32 -9223372036854775808 -2147483648
	
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

IS_NOT_NAN numeric // Don't use outside of `handle`
	:== case numeric of
		(Real val) = val <> 0.0
//		_ = True

IS_FINITE numeric // Don't use outside of `handle`
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
		| IS_FINITE val
			| IS_ZERO val = Zero
			| otherwise = (Re (Fin val))
		| IS_NOT_NAN val = (Re (Inf (FIN_SIGN val)))
		| otherwise = NaN
	handleIm val
		| IS_FINITE val
			| IS_ZERO val = Zero
			| otherwise = (Im (Fin val))
		| IS_NOT_NAN val = (Im (Inf (FIN_SIGN val)))
		| otherwise = NaN
		// TODO : find good tests for complex number performance so I can rewrite it properly
	handleCx re im
		| IS_FINITE re
			| IS_FINITE im
				| IS_ZERO re
					| IS_ZERO im = Zero
					| otherwise = (Im (Fin im))
				| IS_ZERO im = (Re (Fin re))
				| otherwise = (Cx (Fin {re=re, im=im}))
			| IS_NOT_NAN im = (Im (Inf (FIN_SIGN im)))
			| otherwise = NaN
		| IS_NOT_NAN re
			| IS_FINITE im = (Re (Inf (FIN_SIGN re)))
			| IS_NOT_NAN im = (Cx (Inf Directed))
			| otherwise = NaN
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
	(+) (Cx (Inf _)) (Cx (Inf _)) = NaN
	(+) (Cx (Inf _)) (Re (Inf _)) = NaN
	(+) (Cx (Inf _)) (Im (Inf _)) = NaN
	(+) (Cx (Inf _)) _ = (Cx (Inf Directed))
	(+) (Re (Inf _)) (Cx (Inf _)) = NaN
	(+) (Im (Inf _)) (Cx (Inf _)) = NaN
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
	(-) (Cx (Inf _)) (Cx (Inf _)) = NaN
	(-) (Cx (Inf _)) (Re (Inf _)) = NaN
	(-) (Cx (Inf _)) (Im (Inf _)) = NaN
	(-) (Cx (Inf _)) _ = (Cx (Inf Directed))
	(-) (Re (Inf _)) (Cx (Inf _)) = NaN
	(-) (Im (Inf _)) (Cx (Inf _)) = NaN
	(-) _ (Cx (Inf _)) = (Cx (Inf Directed))
	(-) (Re (Inf _)) (Im (Inf _)) = (Cx (Inf Directed))
	(-) (Im (Inf _)) (Re (Inf _)) = (Cx (Inf Directed))
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
	(*) Zero (Cx (Inf _)) = NaN
	(*) Zero (Re (Inf _)) = NaN
	(*) Zero (Im (Inf _)) = NaN
	(*) Zero _ = Zero
	(*) (Cx (Inf _)) Zero = NaN
	(*) (Re (Inf _)) Zero = NaN
	(*) (Im (Inf _)) Zero = NaN
	(*) _ Zero = Zero
	(*) (Cx (Inf _)) _ = (Cx (Inf Directed))
	(*) _ (Cx (Inf _)) = (Cx (Inf Directed))
	(*) (Re (Fin lhs)) (Re (Fin rhs))
		= handle (Re (Fin (lhs * rhs)))
	(*) (Re (Fin lhs)) (Im (Fin rhs))
		= handle (Im (Fin (lhs * rhs)))
	(*) (Im (Fin lhs)) (Re (Fin rhs))
		= handle (Im (Fin (lhs * rhs)))
	(*) (Im (Fin lhs)) (Im (Fin rhs))
		= handle (Re (Fin (~(lhs * rhs))))
	(*) (Re (Inf lhs)) (Re (Inf rhs)) = (Re (Inf (lhs * rhs)))
	(*) (Re (Inf lhs)) (Im (Inf rhs)) = (Im (Inf (lhs * rhs)))
	(*) (Im (Inf lhs)) (Re (Inf rhs)) = (Im (Inf (lhs * rhs)))
	(*) (Im (Inf lhs)) (Im (Inf rhs)) = (Im (Inf (~(lhs * rhs))))
	(*) (Re (Fin lhs)) (Cx (Fin rhs))
		= handle (Cx (Fin {re=lhs*rhs.re, im=lhs*rhs.im}))
	(*) (Im (Fin lhs)) (Cx (Fin rhs))
		= handle (Cx (Fin {re=(~(lhs*rhs.im)), im=lhs*rhs.re}))
	(*) (Cx (Fin lhs)) (Re (Fin rhs))
		= handle (Cx (Fin {re=lhs.re*rhs, im=lhs.im*rhs}))
	(*) (Cx (Fin lhs)) (Im (Fin rhs))
		= handle (Cx (Fin {re=(~(lhs.im*rhs)), im=lhs.re*rhs}))
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
		= handle (Re (Fin (lhs / rhs)))
	(/) (Re (Fin lhs)) (Im (Fin rhs))
		= handle (Im (Fin (~(lhs / rhs))))
	(/) (Im (Fin lhs)) (Re (Fin rhs))
		= handle (Im (Fin (lhs / rhs)))
	(/) (Im (Fin lhs)) (Im (Fin rhs))
		= handle (Re (Fin (lhs / rhs)))
	(/) (Re (Fin lhs)) (Cx (Fin rhs))
		# denominator = rhs.re * rhs.re + rhs.im * rhs.im
		= handle (Cx (Fin {re=((lhs * rhs.re) / denominator), im=(~((lhs * rhs.im) / denominator))}))
	(/) (Im (Fin lhs)) (Cx (Fin rhs))
		# denominator = rhs.re * rhs.re + rhs.im * rhs.im
		= handle (Cx (Fin {re=((lhs * rhs.im) / denominator), im=((lhs * rhs.re) / denominator)}))
	(/) (Cx (Fin lhs)) (Re (Fin rhs))
		= handle (Cx (Fin {re=(lhs.re / rhs), im=(lhs.im / rhs)}))
	(/) (Cx (Fin lhs)) (Im (Fin rhs))
		= handle (Cx (Fin {re=(lhs.im / rhs), im=(~(lhs.re / rhs))}))
	(/) (Cx (Fin lhs)) (Cx (Fin rhs))
		# denominator = rhs.re * rhs.re + rhs.im * rhs.im
		= handle (Cx (Fin {re=((lhs.re * rhs.re + lhs.im * rhs.im) / denominator), im=((lhs.im * rhs.re - lhs.re * rhs.im) / denominator)}))
	(/) _ _ = NaN
	
instance one Number where
	one = (Re (Fin (Int 1)))

instance ^ Number where
	(^) NaN _ = NaN
	(^) _ NaN = NaN
	(^) _ Zero = one
	(^) Zero _ = Zero
	(^) (Re (Fin lhs)) (Re (Fin rhs))
		= handle (Re (Fin (lhs ^ rhs)))
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

instance mod Number where
	(mod) NaN _ = NaN
	(mod) _ NaN = NaN
	(mod) _ Zero = NaN
	(mod) Zero _ = Zero
	//(mod) Infinity _ = NaN
	//(mod) val Infinity = val
	(mod) (Re (Fin lhs)) (Re (Fin rhs))
		= handle (Re (Fin (lhs mod rhs)))
	(mod) (Re (Fin lhs)) (Im (Fin rhs))
		= handle (Im (Fin (~(lhs mod rhs))))
	(mod) (Im (Fin lhs)) (Re (Fin rhs))
		= handle (Im (Fin (lhs mod rhs)))
	(mod) (Im (Fin lhs)) (Im (Fin rhs))
		= handle (Re (Fin (lhs mod rhs)))
	(mod) (Re (Fin lhs)) (Cx (Fin rhs))
		# denominator = rhs.re * rhs.re + rhs.im * rhs.im
		= handle (Cx (Fin {re=((lhs * rhs.re) mod denominator), im=(~((lhs * rhs.im) mod denominator))}))
	(mod) (Im (Fin lhs)) (Cx (Fin rhs))
		# denominator = rhs.re * rhs.re + rhs.im * rhs.im
		= handle (Cx (Fin {re=((lhs * rhs.im) mod denominator), im=((lhs * rhs.re) mod denominator)}))
	(mod) (Cx (Fin lhs)) (Re (Fin rhs))
		= handle (Cx (Fin {re=(lhs.re mod rhs), im=(lhs.im mod rhs)}))
	(mod) (Cx (Fin lhs)) (Im (Fin rhs))
		= handle (Cx (Fin {re=(lhs.im mod rhs), im=(~(lhs.re mod rhs))}))
	(mod) (Cx (Fin lhs)) (Cx (Fin rhs))
		# denominator = rhs.re * rhs.re + rhs.im * rhs.im
		= handle (Cx (Fin {re=((lhs.re * rhs.re + lhs.im * rhs.im) mod denominator), im=((lhs.im * rhs.re - lhs.re * rhs.im) mod denominator)}))
	(mod) _ _ = NaN

instance gcd Number where
	gcd NaN _ = NaN
	gcd _ NaN = NaN
	gcd Zero _ = NaN
	gcd _ Zero = NaN
	gcd (Re (Fin lhs)) (Re (Fin rhs))
		= handle (Re (Fin (gcd lhs rhs)))
	gcd (Im (Fin lhs)) (Im (Fin rhs))
		= handle (Im (Fin (gcd lhs rhs)))
	gcd (Cx (Fin _)) (Cx (Fin _))
		= abort "GCD of Complex not yet implemented!"
	
instance lcm Number where
	lcm NaN _ = NaN
	lcm _ NaN = NaN
	lcm Zero _ = Zero
	lcm _ Zero = Zero
	lcm (Re (Fin lhs)) (Re (Fin rhs))
		= handle (Re (Fin (lcm lhs rhs)))

instance toInt Number where
	toInt NaN = 0
	toInt Zero = 0
	toInt (Cx (Inf Directed)) = 0
	toInt (Re (Inf Positive)) = INT_MAX
	toInt (Re (Inf Negative)) = INT_MIN
	toInt (Im (Inf Positive)) = INT_MAX
	toInt (Im (Inf Negative)) = INT_MIN
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

bitLEFT :: !Number !Number -> Number
bitLEFT NaN _ = NaN
bitLEFT _ NaN = NaN
bitLEFT Zero _ = Zero
bitLEFT lhs Zero = lhs
bitLEFT (Re (Fin lhs)) (Re (Fin rhs))
	= handle (Re (Fin (INT_OPER (<<) lhs rhs)))
	
bitRIGHT :: !Number !Number -> Number
bitRIGHT NaN _ = NaN
bitRIGHT _ NaN = NaN
bitRIGHT Zero _ = Zero
bitRIGHT lhs Zero = lhs
bitRIGHT (Re (Fin lhs)) (Re (Fin rhs))
	= handle (Re (Fin (INT_OPER (>>) lhs rhs)))
	
numFloor :: !Number -> Number
numFloor (Re (Fin val)) = handle (Re (Fin (Int (ENTIER val))))
numFloor (Im (Fin val)) = handle (Im (Fin (Int (ENTIER val))))
numFloor (Cx (Fin {re, im})) = handle (Cx (Fin {re=(Int (ENTIER re)), im=(Int (ENTIER im))}))
numFloor val = val

numCeiling :: !Number -> Number
numCeiling (Re (Fin val)) = handle (Re (Fin (Int (~(ENTIER (~val))))))
numCeiling (Im (Fin val)) = handle (Im (Fin (Int (~(ENTIER (~val))))))
numCeiling (Cx (Fin {re, im})) = handle (Cx (Fin {re=(Int(~(ENTIER(~re)))), im=(Int(~(ENTIER(~im))))}))
numCeiling val = val

numRound :: !Number -> Number
numRound (Re (Fin val)) = handle (Re (Fin (Int (toInt val))))
numRound (Im (Fin val)) = handle (Im (Fin (Int (toInt val))))
numRound (Cx (Fin {re, im})) = handle (Cx (Fin {re=(Int (toInt re)), im=(Int (toInt im))}))
numRound val = val

DEG_TO_RAD val :== (Real (toRad (deg (toReal val))))

toRadians :: !Number -> Number
toRadians (Re (Fin val)) = handle (Re (Fin (DEG_TO_RAD val)))
toRadians (Im (Fin val)) = handle (Im (Fin (DEG_TO_RAD val)))
toRadians (Cx (Fin {re, im})) = handle (Cx (Fin {re=DEG_TO_RAD re, im=DEG_TO_RAD im}))
toRadians val = val

RAD_TO_DEG val :== (Real (toDeg (rad (toReal val))))

toDegrees :: !Number -> Number
toDegrees (Re (Fin val)) = handle (Re (Fin (RAD_TO_DEG val)))
toDegrees (Im (Fin val)) = handle (Im (Fin (RAD_TO_DEG val)))
toDegrees (Cx (Fin {re, im})) = handle (Cx (Fin {re=RAD_TO_DEG re, im=RAD_TO_DEG im}))
toDegrees val = val