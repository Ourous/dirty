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
		= case toInt uchar of
			0x25A0 =	'\000'
			// reserved
			0x1F514 =	'\007'
			// unallocated
			0x2353 =	'\016'
			0x2344 =	'\017'
			0x234C =	'\020'
			0x2343 =	'\021'
			0x233A =	'\022'
			0xFE47 =	'\023'
			0xFE48 =	'\024'
			// unallocated
			0xFE35 =	'\026'
			0xFE36 =	'\027'
			// unallocated
			0xFE37 =	'\031'
			0xFE38 =	'\032'
			// unallocated
			0xFE3F =	'\034'
			0x27E9 =	'\035'; 0x3009 = '\035'; 0x232A = '\035'
			0xFE40 =	'\036'
			0x27E8 =	'\037'; 0x3008 = '\037'; 0x2329 = '\037'
			// ascii
			0x203C =	'\200'
			0x2047 =	'\201'
			0x203D =	'\202'
			0x00A1 =	'\203'
			0x00BF =	'\204'
			0x2395 =	'\205'
			0x2370 =	'\206'
			0x2012 =	'\207'; 0x2013 = '\207'; 0x2014 = '\207'; 0x2015 = '\207'
			0x21BA =	'\210'
			0x21BB =	'\211'
			0x25B3 =	'\212'
			0x25B7 =	'\213'
			0x25BD =	'\214'
			0x25C1 =	'\215'
			0x25C7 =	'\216'
			0x25B2 =	'\217'
			0x25B6 =	'\220'
			0x25BC =	'\221'
			0x25C0 =	'\222'
			0x25C6 =	'\223'
			0x25FF =	'\224'
			0x25FA =	'\225'
			0x25F8 =	'\226'
			0x25F9 =	'\227'
			0x25E2 =	'\230'
			0x25E3 =	'\231'
			0x25E4 =	'\232'
			0x25E5 =	'\233'
			0x25CC =	'\234'
			0x25CF =	'\235'
			0x2190 =	'\236'
			0x2192 =	'\237'
			0x2194 =	'\240'
			0x21D0 =	'\241'
			0x21D2 =	'\242'
			0x21D4 =	'\243'
			0x2198 =	'\244'
			0x2196 =	'\245'
			0x2199 =	'\246'
			0x2197 =	'\247'
			0x1E80 =	'\250'
			0x1E81 =	'\251'
			0x1E82 =	'\252'
			0x1E83 =	'\253'
			0x221E =	'\254'
			0x221D =	'\255'
			0x2205 =	'\256'
			0x00D7 =	'\257'
			0x00F7 =	'\260'
			0x215F =	'\261'
			0x221A =	'\262'
			0x2211 =	'\263'
			0x220F =	'\264'
			0x017E =	'\265'
			0x1E91 =	'\266'
			0x0133 =	'\267'
			0x0132 =	'\270'
			0x2242 =	'\271'
			0x00AC =	'\272'
			0x00AB =	'\273'
			0x00BB =	'\274'
			0x2228 =	'\275'
			0x2203 =	'\276'
			0x2227 =	'\277'
			0x2200 =	'\300'
			0x22BB =	'\301'
			0x22A8 =	'\302'
			0x22AD =	'\303'
			0x2248 =	'\304'
			0x2260 =	'\305'
			0x2264 =	'\306'
			0x2265 =	'\307'
			0x2261 =	'\310'
			0x2262 =	'\311'
			0x2229 =	'\312'
			0x222A =	'\313'
			0x2206 =	'\314'
			0x220B =	'\315'
			0x2283 =	'\316'
			0x222B =	'\317'
			0x222C =	'\320'
			0x222D =	'\321'
			0x2119 =	'\322'
			0x2102 =	'\323'
			0x2207 =	'\324'
			0x010B =	'\325'
			0x1E59 =	'\326'
			0x2250 =	'\327'
			0x1E1F =	'\330'
			0x2C66 =	'\331'
			0x1E58 =	'\332'
			0x03C0 =	'\333'
			0x00C5 =	'\334'
			0x00E5 =	'\335'
			0x1E02 =	'\336'
			0x1E03 =	'\337'
			0x015D =	'\340'
			0x0161 =	'\341'
			0x0109 =	'\342'
			0x010D =	'\343'
			0x1E6B =	'\344'
			0x0226 =	'\345'
			0x00C4 =	'\346'
			0x1E40 =	'\347'
			0x1E44 =	'\350'
			0x1E56 =	'\351'
			0x1E60 =	'\352'
			0x1E84 =	'\353'
			0x010C =	'\354'
			0x0108 =	'\355'
			0x1E86 =	'\356'
			0x010A =	'\357'
			0x023B =	'\360'
			0x1E0A =	'\361'
			0x1E97 =	'\362'
			0x1E6A =	'\363'
			// unallocated
			0x2122 =	'\377'