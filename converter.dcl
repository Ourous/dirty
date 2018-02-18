definition module converter

import types

//nativeToUTF8 :: String -> String
//utf8ToNative :: String -> String
//nativeToUTF8 string :== unicodeToUTF8 (nativeToUnicode string)
//utf8ToNative string :== unicodeToNative (utf8ToUnicode string)

unicodeToUTF8 :: [Int] -> String
utf8ToUnicode :: String -> [Int]

unicodeCharset :: [Int]

nativeCharset :: [Int]

commandMapping :: [Command]

//nativeToUnicode :: String -> [Number]
//unicodeToNative :: [Number] -> String