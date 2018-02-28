implementation module unicode
import Text.Unicode, Text.Unicode.UChar, Text.Unicode.Encodings.UTF8
from StdEnv import map

isLowerUChar :: Int -> Bool
isLowerUChar char = isLower (fromInt char)

isUpperUChar :: Int -> Bool
isUpperUChar char = isUpper (fromInt char)

toLowerUChar :: Int -> Int
toLowerUChar char = toInt (toLower (fromInt char))

toUpperUChar :: Int -> Int
toUpperUChar char = toInt (toUpper (fromInt char))


unicodeToUTF8 :: [Int] -> String
unicodeToUTF8 string
	= conv (fromUnicode (map fromInt string))
where
	conv :: (UTF8 -> String)
	conv = toString
	
utf8ToUnicode :: String -> [Int]
utf8ToUnicode string
	= map toInt (toUnicode (conv string))
where
	conv :: (String -> UTF8)
	conv = fromString
