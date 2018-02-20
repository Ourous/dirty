definition module runtime

import types

execute :: State Memory Flags *World -> *World

evaluate :: [String] *World -> *(Memory, *World)