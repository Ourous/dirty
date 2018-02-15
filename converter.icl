implementation module converter

import qualified StdEnv as StdEnv
import Text, Text.Unicode.Encodings.UTF8, Text.Unicode, Text.Unicode.UChar

nativeToUTF8 :: String -> String
nativeToUTF8 string
	= 'StdEnv'.abort ""
	
utf8ToNative :: String -> String
utf8ToNative string
	= 'StdEnv'.abort ""