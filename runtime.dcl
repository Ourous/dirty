definition module runtime

import types

construct :: !Program !Flags -> (State Memory *World -> *World)

//execute :: !State !Memory *World -> *World

evaluate :: ![String] *World -> *(Memory, *World)