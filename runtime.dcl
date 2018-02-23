definition module runtime

import types

construct :: !Program !Flags -> (*(State, Memory, *World) -> *World)

initialize :: !Program *World -> *(State, *World)

evaluate :: ![String] *World -> *(Memory, *World)