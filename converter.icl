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

nativeMaping :: [Token]
nativeMapping =
	[