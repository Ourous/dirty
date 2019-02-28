implementation module Dirty.Backend.Value

import Dirty.Backend.Number, Dirty.Backend.Stack
import Data.Maybe
import Text.GenParse
import StdOverloaded

instance fromInt Value
where fromInt val = Item (fromInt val)

instance fromReal Value
where fromReal val = Item (fromReal val)

instance fromBool Value
where fromBool val = Item (fromBool val)

tryParseValue :: String -> Maybe Value
tryParseValue str
	# val = mapMaybe fromInt (gParse{|*|} expr)
	| isJust val = val
	# val = mapMaybe fromReal (gParse{|*|} expr)
	| isJust val = val
	# val = mapMaybe fromBool (gParse{|*|} expr)
	= val
	// TODO: strings
	// TODO: nested stacks
where expr = preParseString str