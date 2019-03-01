implementation module Dirty.Backend.Rational

import StdEnv

:: Rational
	= Int !Int
	| Real !Real

FRAC_MAX :== 4503599627370496.0
FRAC_MIN :== -4503599627370496.0

IS_INT real :== toReal (entier real) == real

//applyUnaryReal :: (Real -> Real) (Int -> Real) Rational -> Rational
//applyUnaryReal op conv (Real val) = (Real (op val))
//applyUnaryReal op conv (Int val) = (Real (op (conv val)))
applyUnaryReal op conv val
	:== (Real (case val of
			(Real val) = op val
			(Int val) = op (conv val)
		))

//applyUnaryInt :: (Int -> Int) (Real -> Int) Rational -> Rational
//applyUnaryInt op conv (Real val) = (Int (op (conv val)))
//applyUnaryInt op conv (Int val) = (Int (op val))
applyUnaryInt op conv val
	:== (Int (case val of
			(Int val) = op val
			(Real val) = op (conv val)
		))
	
applyBinaryReal :: !(Real Real -> Real) !(Int -> Real) !Rational !Rational -> Rational
applyBinaryReal op conv (Real lhs) (Real rhs)
	= (Real (op lhs rhs))
applyBinaryReal op conv (Real lhs) (Int rhs)
	= (Real (op lhs (conv rhs)))
applyBinaryReal op conv (Int lhs) (Real rhs)
	= (Real (op (conv lhs) rhs))
applyBinaryReal op conv (Int lhs) (Int rhs)
	= (Real (op (conv lhs) (conv rhs)))
		
applyBinaryInt :: !(Int Int -> Int) !(Real -> Int) !Rational !Rational -> Rational
applyBinaryInt op conv (Int lhs) (Int rhs)
	= (Int (op lhs rhs))
applyBinaryInt op conv (Real lhs) (Int rhs)
	= (Int (op (conv lhs) rhs))
applyBinaryInt op conv (Int lhs) (Real rhs)
	= (Int (op lhs (conv rhs)))
applyBinaryInt op conv (Real lhs) (Real rhs)
	= (Int (op (conv lhs) (conv rhs)))

// Rational implementations

instance + Rational where (+) lhs rhs = applyBinaryReal (+) (toReal) lhs rhs
	/*
	(+) :: !Rational !Rational -> Rational
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
	*/
	
instance - Rational where (-) lhs rhs = applyBinaryReal (-) (toReal) lhs rhs
/*
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
*/
instance * Rational where (*) lhs rhs = applyBinaryReal (*) (toReal) lhs rhs
/*
	(*) (Int lhs) (Int rhs)
		= (Real (toReal lhs * toReal rhs))
	(*) (Int lhs) (Real rhs)
		= (Real (toReal lhs * rhs))
	(*) (Real lhs) (Int rhs)
		= (Real (lhs * toReal rhs))
	(*) (Real lhs) (Real rhs)
		= (Real (lhs * rhs))
*/
instance / Rational where (/) lhs rhs = applyBinaryReal (/) (toReal) lhs rhs
/*
	(/) (Int lhs) (Int rhs)
		= if(lhs rem rhs == 0) (Int (lhs / rhs)) (Real (toReal lhs / toReal rhs))
	(/) (Int lhs) (Real rhs)
		= (Real (toReal lhs / rhs))
	(/) (Real lhs) (Int rhs)
		= (Real (lhs / toReal rhs))
	(/) (Real lhs) (Real rhs)
		= (Real (lhs / rhs))
*/
instance ^ Rational where (^) lhs rhs = applyBinaryReal (^) (toReal) lhs rhs
/*
	(^) (Int lhs) (Int rhs)
		= (Real (toReal lhs ^ toReal rhs))
	(^) (Int lhs) (Real rhs)
		= (Real (toReal lhs ^ rhs))
	(^) (Real lhs) (Int rhs)
		= (Real (lhs ^ toReal rhs))
	(^) (Real lhs) (Real rhs)
		= (Real (lhs ^ rhs))
*/
instance abs Rational where
	abs (Int val)
		= (Int (abs val))
	abs (Real val)
		= (Real (abs val))

instance sign Rational where
	sign (Int val)
		= sign val
	sign (Real val)
		= sign val
		
instance ~ Rational where
	~ (Int val)
		= (Int (~ val))
	~ (Real val)
		= (Real (~ val))

instance == Rational where
	(==) (Int lhs) (Int rhs)
		= lhs == rhs
	(==) (Int lhs) (Real rhs)
		= toReal lhs == rhs
	(==) (Real lhs) (Int rhs)
		= lhs == toReal rhs
	(==) (Real lhs) (Real rhs)
		= lhs == rhs

instance < Rational where
	(<) (Int lhs) (Int rhs)
		= lhs < rhs
	(<) (Int lhs) (Real rhs)
		= toReal lhs < rhs
	(<) (Real lhs) (Int rhs)
		= lhs < toReal rhs
	(<) (Real lhs) (Real rhs)
		= lhs < rhs
		
		
instance toInt Rational where
	toInt (Int val) = val
	toInt (Real val) = toInt val

instance toReal Rational where
	toReal (Int val) = toReal val
	toReal (Real val) = val
	
instance toString Rational where
	toString (Int val) = toString val
	toString (Real val) = toString val
	
instance fromInt Rational where
	fromInt int = Int int

instance fromReal Rational where
	fromReal real = Real real
	
instance fromString Rational where
	fromString str
		| and [isDigit c \\ c <-: str]
			= Int (toInt str)
		| otherwise
			= Real (toReal str)
	
instance one Rational where one = (Int 1)

instance zero Rational where zero = (Int 0)
		
instance mod Rational where (mod) lhs rhs = applyBinaryInt (rem) (toInt) lhs rhs
		
instance gcd Rational where gcd lhs rhs = applyBinaryInt gcd (toInt) lhs rhs

instance lcm Rational where lcm lhs rhs = applyBinaryInt lcm (toInt) lhs rhs

instance ln Rational where ln val = applyUnaryReal ln (toReal) val

instance log10 Rational where log10 val = applyUnaryReal log10 (toReal) val

instance exp Rational where exp val = applyUnaryReal exp (toReal) val

instance sqrt Rational where sqrt val = applyUnaryReal sqrt (toReal) val

instance sin Rational where sin val = applyUnaryReal sin (toReal) val

instance cos Rational where cos val = applyUnaryReal cos (toReal) val

instance tan Rational where tan val = applyUnaryReal tan (toReal) val

instance asin Rational where asin val = applyUnaryReal asin (toReal) val

instance acos Rational where acos val = applyUnaryReal acos (toReal) val

instance atan Rational where atan val = applyUnaryReal atan (toReal) val