definition module Dirty.Backend

import Dirty.Backend.Number
import Dirty.Backend.Value
import Dirty.Backend.Stack
from Data.Maybe import ::Maybe
import Text.Unicode
from Text.Unicode.UChar import ::UChar, instance == UChar, instance < UChar,
                               instance fromInt UChar, instance fromChar UChar,
                               instance toChar UChar, instance toInt UChar

class toCharList a :: a -> [Char]
class fromCharList a :: [Char] -> a

class repr a :: Bool a -> [Char] // allow infinite output?
class disp a :: a -> UString // encoding converter
class eval a :: [Char] -> Maybe a