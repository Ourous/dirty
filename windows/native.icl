implementation module native
import System._WinDef, _SystemArray
import code from library "native_library"

:: COORD :== {#Int}

clearConsole :: *World -> *World
clearConsole world
	# (handle, world) = getHandle stdOutputHandle world
	# (_, world) = setCursorPosition handle {#0, 0} world
	= world

stdOutputHandle :: Int
stdOutputHandle =: -11

getHandle :: !Int !*World -> (!HANDLE, !*World)
getHandle hID world
	= code {
		ccall GetStdHandle@4 "PI:I:I"
	}
	
setCursorPosition :: !HANDLE !COORD !*World -> (!Bool, !*World)
setCursorPosition hConsole aCoord world = (True, world)
//	= code {
//		ccall SetConsoleCursorPosition@4 "PA:V:I"
//	}