definition module unicode

import types

isUpperUChar :: Int -> Bool
isLowerUChar :: Int -> Bool
toUpperUChar :: Int -> Int
toLowerUChar :: Int -> Int

unicodeToUTF8 :: [Int] -> String
utf8ToUnicode :: String -> [Int]
