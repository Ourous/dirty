definition module Dirty.Backend

import Dirty.Backend.Number
import Dirty.Backend.Value
import Dirty.Backend.Stack

class repr a :: a -> [Char]