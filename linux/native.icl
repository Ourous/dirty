implementation module native

import System.IO

clearConsole :: *World -> *World
clearConsole world = (execIO (putStr "\033[2J\033[1;1H")) world