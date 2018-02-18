implementation module converter

import StdBool, StdInt
import types, arithmetic, Text, Text.Unicode.Encodings.UTF8, Text.Unicode, Text.Unicode.UChar
from StdFunc import o
from StdList import map
	
unicodeToUTF8 :: [Number] -> String
unicodeToUTF8 string
	= conv (fromUnicode (map (fromInt o toInt) string))
where
	conv :: (UTF8 -> String)
	conv = toString
	
utf8ToUnicode :: String -> [Number]
utf8ToUnicode string
	= map (fromInt o toInt) (toUnicode (conv string))
where
	conv :: (String -> UTF8)
	conv = fromString

//nativeToUnicode :: String -> [Number]
//unicodeToNative :: [Number] -> String

:: Mapping
	= {
		native :: Char,
		meaning :: Token
	}

nativeMapping =
	[Control_Terminate
	,Control_Start East
	,Control_Start West
	,Control_Start North
	,Control_Start South
	,Stack_ShiftBase East
	,Stack_ShiftBase West
	,Operator_IO_Bell
	,Stack_ShiftBase North
	,Stack_ShiftBase South
	,Operator_Set_Length
	,Control_LINE
	,Control_Turn Anticlockwise
	,Control_Turn Clockwise
	,Control_Move Always East
	,Control_Move NoLoop East
	,Control_Move Always West
	,Control_Move NoLoop West
	,Control_Move Always North
	,Control_Move NoLoop North
	,Control_Move Always South
	,Control_Move NoLoop South
	,Control_Random Always Horizontal
	,Control_Random NoLoop Horizontal
	,Control_Random Always Vertical
	,Control_Random NoLoop Vertical
	