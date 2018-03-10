definition module runtime

import types

construct :: !Program !Flags -> (*(State, Memory, *World) -> *World)

initialize :: !Program ![String] *World -> (State, Memory, *World)