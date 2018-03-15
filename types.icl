implementation module types

import StdMaybe, StdOverloaded, StdClass

instance Nullable (Null t) t where
	toMaybe _ = Nothing
	
instance Nullable (Value t) t where
	toMaybe (Val val) = (Just val)