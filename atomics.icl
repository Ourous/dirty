implementation module atomics

import types, StdOverloaded, StdBool, StdInt, StdReal

FRAC_MAX :== 4503599627370496.0
FRAC_MIN :== -4503599627370496.0

IS_INT real :== toReal (entier real) == real

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

instance + Number where
	(