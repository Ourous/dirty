implementation module Dirty.Frontend.Preprocessor

import Text, Text.Unicode, Text.Unicode.Encodings.UTF8, Text.Unicode.UChar
import Data.Matrix
from StdEnv import map, maxList, abort, instance toChar Int
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
			0x2353 =	14
			0x2344 =	15
			0x234C =	16
			0x2343 =	17
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
			0x27E8 =	28; 0x3008 = 28; 0x2329 = 28
			0x27E9 =	29; 0x3009 = 29; 0x232A = 29
			0xFE3F =	30
			0xFE40 =	31
			// ascii
			0x203C =	128
			0x2047 =	129
			0x203D =	130
			0x00A1 =	131
			0x00BF =	132
			0x2395 =	133
			0x2370 =	134
			0x2012 =	135; 0x2013 = 135; 0x2014 = 135; 0x2015 = 135
			0x21BA =	136
			0x21BB =	137
			0x25B3 =	138
			0x25B7 =	139
			0x25BD =	140
			0x25C1 =	141
			0x25C7 =	142
			0x25B2 =	143
			0x25B6 =	144
			0x25BC =	145
			0x25C0 =	146
			0x25C6 =	147
			0x25FF =	148
			0x25FA =	149
			0x25F8 =	150
			0x25F9 =	151
			0x25E2 =	152
			0x25E3 =	153
			0x25E4 =	154
			0x25E5 =	155
			0x25CC =	156
			0x25CF =	157
			0x2190 =	158
			0x2192 =	159
			0x2194 =	160
			0x21D0 =	161
			0x21D2 =	162
			0x21D4 =	163
			0x2198 =	164
			0x2196 =	165
			0x2199 =	166
			0x2197 =	167
			0x1E80 =	168
			0x1E81 =	169
			0x1E82 =	170
			0x1E83 =	171
			0x221E =	172
			0x221D =	173
			0x2205 =	174
			0x00D7 =	175
			0x00F7 =	176
			0x215F =	177
			0x221A =	178
			0x2211 =	179
			0x220F =	180
			0x017E =	181
			0x1E91 =	182
			0x0133 =	183
			0x0132 =	184
			0x2242 =	185
			0x00AC =	186
			0x00AB =	187
			0x00BB =	188
			0x2228 =	189
			0x2203 =	190
			0x2227 =	191
			0x2200 =	192
			0x22BB =	193
			0x22A8 =	194
			0x22AD =	195
			0x2248 =	196
			0x2260 =	197
			0x2264 =	198
			0x2265 =	199
			0x2261 =	200
			0x2262 =	201
			0x2229 =	202
			0x222A =	203
			0x2206 =	204
			0x220B =	205
			0x2283 =	206
			0x222B =	207
			0x222C =	208
			0x222D =	209
			0x2119 =	210
			0x2102 =	211
			0x2207 =	212
			0x010B =	213
			0x1E59 =	214
			0x2250 =	215
			0x1E1F =	216
			0x2C66 =	217
			0x1E58 =	218
			0x03C0 =	219
			0x00C5 =	220
			0x00E5 =	221
			0x1E02 =	222
			0x1E03 =	223
			0x015D =	224
			0x0161 =	225
			0x0109 =	226
			0x010D =	227
			0x1E6B =	228
			0x0226 =	229
			0x00C4 =	230
			0x1E40 =	231
			0x1E44 =	232
			0x1E56 =	233
			0x1E60 =	234
			0x1E84 =	235
			0x010C =	236
			0x0108 =	237
			0x1E86 =	238
			0x010A =	239
			0x023B =	240
			0x1E0A =	241
			0x1E97 =	242
			0x1E6A =	243
			// unallocated
			0x2122 =	255