implementation module atomics

import types, StdOverloaded, StdBool, StdInt, StdReal
from StdLib import isFinite
from StdMisc import abort
import StdClass

FRAC_MAX :== 4503599627370496.0
FRAC_MIN :== -4503599627370496.0

IS_INT real :== toReal (entier real) == real

applyUnaryReal :: (Real -> Real) (Int -> Real) Numeric -> Numeric
applyUnaryReal op conv (Real val) = (Real (op val))
applyUnaryReal op conv (Int val) = (Real (op (conv val)))

applyUnaryInt :: (Int -> Int) (Real -> Int) Numeric -> Numeric
applyUnaryInt op conv (Real val) = (Int (op (conv val)))
applyUnaryInt op conv (Int val) = (Int (op val))

applyBinaryReal :: (Real Real -> Real) (Int -> Real) Numeric Numeric -> Numeric
applyBinaryReal op conv (Real lhs) (Real rhs)
	= (Real (op lhs rhs))
applyBinaryReal op conv (Real lhs) (Int rhs)
	= (Real (op lhs (conv rhs)))
applyBinaryReal op conv (Int lhs) (Real rhs)
	= (Real (op (conv lhs) rhs))
applyBinaryReal op conv (Int lhs) (Int rhs)
	= (Real (op (conv lhs) (conv rhs)))
	
applyBinaryInt :: (Int Int -> Int) (Real -> Int) Numeric Numeric -> Numeric
applyBinaryInt op conv (Real lhs) (Real rhs)
	= (Int (op (conv lhs) (conv rhs)))
applyBinaryInt op conv (Real lhs) (Int rhs)
	= (Int (op (conv lhs) rhs))
applyBinaryInt op conv (Int lhs) (Real rhs)
	= (Int (op lhs (conv rhs)))
applyBinaryInt op conv (Int lhs) (Int rhs)
	= (Int (op lhs rhs))

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
		= if(lhs rem rhs == 0) (Int (lhs / rhs)) (Real (toReal lhs / toReal rhs))
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

instance sign Numeric where
	sign (Int val)
		= sign val
	sign (Real val)
		= sign val
		
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
		
instance mod Numeric where (mod) lhs rhs = applyBinaryInt (rem) (toInt) lhs rhs
		
instance gcd Numeric where gcd lhs rhs = applyBinaryInt gcd (toInt) lhs rhs

instance lcm Numeric where lcm lhs rhs = applyBinaryInt lcm (toInt) lhs rhs

instance ln Numeric where ln val = applyUnaryReal ln (toReal) val

instance log10 Numeric where log10 val = applyUnaryReal log10 (toReal) val

instance exp Numeric where exp val = applyUnaryReal exp (toReal) val

instance sqrt Numeric where sqrt val = applyUnaryReal sqrt (toReal) val

instance sin Numeric where sin val = applyUnaryReal sin (toReal) val

instance cos Numeric