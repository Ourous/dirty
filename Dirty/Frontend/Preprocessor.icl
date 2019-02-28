implementation module Dirty.Frontend.Preprocessor

import Text, Text.Unicode, Text.Unicode.Encodings.UTF8, Text.Unicode.UChar
import Data.Matrix
from StdEnv import map, maxList, abort
import StdArray, StdInt

preprocessFile :: String -> Matrix Char
preprocessFile str
	= let
		lines = split "\n" str
		maxlen = maxList (map size lines)
	in {{c \\ c <-: rpad line maxlen ' '} \\ line <- lines}

preprocessUTF8 :: String -> String
preprocessUTF8 str
	= let
		ustr :: UTF8
		ustr = fromString str
	in {# mapToNative c \\c <- toUnicode ustr}

mapToNative :: UChar -> Char
mapToNative uchar
	| isAscii uchar
		= toChar uchar
	| otherwise
		= toChar case toInt uchar of
			0x25A0 =	0
			// reserved
			0x1F514 =	7
			// unallocated
			0x2343 =	14
			0x2344 =	15
			0x234C =	16
			0x2353 =	17
			0x233A =	18
			0xFE47 =	19
			0xFE48 =	20
			// unallocated
			0xFE35 =	22
			0xFE36 =	23
			// unallocated
			0xFE37 =	25
			0xFE38 =	26
			// unallocated
			0x27E8 =	28
			0x3008 =	28
			0x2329 =	28
			0x27E9 =	29
			0x3009 =	29
			0x232A =	29
			0xFE3F =	30
			0xFE40 =	31
			// ascii