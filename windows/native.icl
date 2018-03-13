implementation module native
import System._WinDef, System._WinBase, StdEnv, System.IO
import code from library "native_library"

DEBUG val :== execIO (putStrLn (toString val))
ERROR :== (\(e, w) -> DEBUG e w) o getLastError

clearConsole :: *World -> *World
clearConsole world
	//# world = ERROR world
	# (handle, world) = getHandle stdOutputHandle world
	//# world = ERROR world
	# (bool, world) = setCursorPosition handle "\0\0\0\0" world// setCursorPosition handle {#0, 0} world
	//# world = ERROR world
	= world
		

stdOutputHandle :== -11

invalidHandle :== -1

getHandle :: !Int !*World -> (!HANDLE, !*World)
getHandle hID world
	= code {
		ccall GetStdHandle@4 "PI:I:I"
	} // this one works
	
setCursorPosition :: !HANDLE !COORD !*World -> (!Bool, !*World)
setCursorPosition hConsole aCoord world //= (True, world)
	= code {
		ccall SetConsoleCursorPosition@4 "PIA:I:I" 
	} // extern sizeof number is probably wrong