definition module runtime

import types

execute :: State Memory *World Flags-> *World

evaluate :: [String] *World -> *(Memory, *World)