implementation module converter

import StdBool, StdInt
import types, arithmetic, Text, Text.Unicode.Encodings.UTF8, Text.Unicode, Text.Unicode.UChar
from StdFunc import o
from StdList import map, !!
	
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

nativeCharset :: [Int]
nativeCharset =:
	[0
	,1
	,2
	,3
	,4
	,5
	,6
	,7
	,8
	,9
	,10
	,11
	,12
	,13
	,14
	,15
	,16
	,17
	,18
	,19
	,20
	,21
	,22
	,23
	,24
	,25
	,26
	,27
	,28
	,29
	,30
	,31
	,32
	,33
	,34
	,35
	,36
	,37
	,38
	,39
	,40
	,41
	,42
	,43
	,44
	,45
	,46
	,47
	,48
	,49
	,50
	,51
	,52
	,53
	,54
	,55
	,56
	,57
	,58
	,59
	,60
	,61
	,62
	,63
	,64
	,65
	,66
	,67
	,68
	,69
	,70
	,71
	,72
	,73
	,74
	,75
	,76
	,77
	,78
	,79
	,80
	,81
	,82
	,83
	,84
	,85
	,86
	,87
	,88
	,89
	,90
	,91
	,92
	,93
	,94
	,95
	,96
	,97
	,98
	,99
	,100
	,101
	,102
	,103
	,104
	,105
	,106
	,107
	,108
	,109
	,110
	,111
	,112
	,113
	,114
	,115
	,116
	,117
	,118
	,119
	,120
	,121
	,122
	,123
	,124
	,125
	,126
	,127
	,128
	,129
	,130
	,131
	,132
	,133
	,134
	,135
	,136
	,137
	,138
	,139
	,140
	,141
	,142
	,143
	,144
	,145
	,146
	,147
	,148
	,149
	,150
	,151
	,152
	,153
	,154
	,155
	,156
	,157
	,158
	,159
	,160
	,161
	,162
	,163
	,164
	,165
	,166
	,167
	,168
	,169
	,170
	,171
	,172
	,173
	,174
	,175
	,176
	,177
	,178
	,179
	,180
	,181
	,182
	,183
	,184
	,185
	,186
	,187
	,188
	,189
	,190
	,191
	,192
	,193
	,194
	,195
	,196
	,197
	,198
	,199
	,200
	,201
	,202
	,203
	,204
	,205
	,206
	,207
	,208
	,209
	,210
	,211
	,212
	,213
	,214
	,215
	,216
	,217
	,218
	,219
	,220
	,221
	,222
	,223
	,224
	,225
	,226
	,227
	,228
	,229
	,230
	,231
	,232
	,233
	,234
	,235
	,236
	,237
	,238
	,239
	,240
	,241
	,242
	,243
	,244
	,245
	,246
	,247
	,248
	,249
	,250
	,251
	,252
	,253
	,254
	,255
	]

unicodeCharset :: [Int]
unicodeCharset =:
		[9211
	,8680
	,8678
	,8679
	,8681
	,11116
	,11114
	,128276
	,11115
	,11117
	,10
	,8470
	,11118
	,11119
	,11157
	,11013
	,11014
	,11015
	,11020
	,11021
	,9180
	,9181
	,9140
	,9141
	,9182
	,9183
	,8942
	,8943
	,8944
	,8945
	,11012
	,8691
	,32
	,33
	,34
	,35
	,36
	,37
	,38
	,39
	,40
	,41
	,42
	,43
	,44
	,45
	,46
	,47
	,48
	,49
	,50
	,51
	,52
	,53
	,54
	,55
	,56
	,57
	,58
	,59
	,60
	,61
	,62
	,63
	,64
	,65
	,66
	,67
	,68
	,69
	,70
	,71
	,72
	,73
	,74
	,75
	,76
	,77
	,78
	,79
	,80
	,81
	,82
	,83
	,84
	,85
	,86
	,87
	,88
	,89
	,90
	,91
	,92
	,93
	,94
	,95
	,96
	,97
	,98
	,99
	,100
	,101
	,102
	,103
	,104
	,105
	,106
	,107
	,108
	,109
	,110
	,111
	,112
	,113
	,114
	,115
	,116
	,117
	,118
	,119
	,120
	,121
	,122
	,123
	,124
	,125
	,126
	,8252
	,8263
	,8264
	,8265
	,8253
	,215
	,10800
	,247
	,8724
	,8730
	,8543
	,8734
	,8744
	,10834
	,8743
	,10833
	,8891
	,172
	,8893
	,8892
	,8741
	,8745
	,8746
	,8712
	,8715
	,8918
	,8919
	,8784
	,10877
	,10879
	,10878
	,10880
	,8800
	,8801
	,8802
	,8842
	,8843
	,8838
	,8839
	,8840
	,8841
	,229
	,197
	,175
	,171
	,187
	,8987
	,8986
	,960
	,8660
	,8662
	,8663
	,8664
	,8665
	,217
	,218
	,220
	,362
	,7908
	,219
	,249
	,250
	,252
	,363
	,7909
	,251
	,7808
	,7810
	,7812
	,7816
	,7814
	,372
	,7809
	,7811
	,7813
	,7817
	,7815
	,10545
	,10546
	,10529
	,10530
	,11108
	,11109
	,11106
	,11104
	,11105
	,11107
	,11110
	,11111
	,11112
	,11113
	,11122
	,11120
	,11121
	,11126
	,11127
	,11128
	,11129
	,11134
	,11135
	,11152
	,11153
	,11154
	,11155
	,8719
	,8721
	,8707
	,8708
	,119979
	,119982
	,11016
	,11017
	,11018
	,11019
	,11036
	,11034
	,9675
	,9676
	,7690
	,7692
	,7691
	,7693
	,10516
	,8614
	,8611
	,10501
	,10518
	,7718
	,7719
	]
	
toCommand :: Char -> Command
toCommand char
	= commandMapping !! (toInt char)

commandMapping :: [Command]
commandMapping =:
	[(Control (Terminate))
	,(Control (Start East))
	,(Control (Start West))
	,(Control (Start North))
	,(Control (Start South))
	,(Stack (ShiftBase East))
	,(Stack (ShiftBase West))
	,(Operator (IO_Bell))
	,(Stack (ShiftBase North))
	,(Stack (ShiftBase South))
	,(Control (LINE))
	,(Operator (Set_Length))
	,(Control (Turn Anticlockwise))
	,(Control (Turn Clockwise))
	,(Control (Change East))
	,(Control (Change West))
	,(Control (Change North))
	,(Control (Change South))
	,(Control (Either Horizontal))
	,(Control (Either Vertical))
	,(Control (Loop_Left North Nothing))
	,(Control (Loop_Left South Nothing))
	,(Control (Goto North Nothing))
	,(Control (Goto South Nothing))
	,(Control (Loop_Right North Nothing))
	,(Control (Loop_Right South Nothing))
	,(Control (Mirror False Vertical))
	,(Control (Mirror False Horizontal))
	,(Control (Mirror False Identity))
	,(Control (Mirror False Inverse))
	,(Control (Start_Horizontal))
	,(Control (Start_Vertical))
	,(Control (NOOP))
	,(Operator (IO_WriteOnce))
	,(Literal (Quote))
	,(Variable (Random))
	,(Operator (Math_Integral))
	,(Operator (Math_Modulus))
	,(Stack (JoinFromBase))
	,(Control (String))
	,(Control (Loop_Left West Nothing))
	,(Control (Loop_Left East Nothing))
	,(Operator (Math_DotProduct))
	,(Operator (Binary (+)))//(Operator (Math_Addition))
	,(Operator (Range_FromLeftStepRight))
	,(Operator (Binary (-)))//(Operator (Math_Subtraction))
	,(Operator (Range_FromMiddleToZero))
	,(Control (Mirror True Identity))
	,(Literal (Digit Zero))
	,(Literal (Digit (Re (Fin (Int 1)))))
	,(Literal (Digit (Re (Fin (Int 2)))))
	,(Literal (Digit (Re (Fin (Int 3)))))
	,(Literal (Digit (Re (Fin (Int 4)))))
	,(Literal (Digit (Re (Fin (Int 5)))))
	,(Literal (Digit (Re (Fin (Int 6)))))
	,(Literal (Digit (Re (Fin (Int 7)))))
	,(Literal (Digit (Re (Fin (Int 8)))))
	,(Literal (Digit (Re (Fin (Int 9)))))
	,(Operator (Range_FromMiddleAvoidZero))
	,(Operator (Range_FromLeftTimesRight))
	,(Operator (Logic_LessThan))
	,(Operator (Logic_Equality))
	,(Operator (Logic_GreaterThan))
	,(Operator (IO_ReadOnce))
	,(Stack (AdjustOffset))
	,(Literal (Alphabet Uppercase))
	,(Operator (Math_ConvertToBase))
	,(Operator (Unary (acos)))//(Operator (Math_ArcCoSine))
	,(Stack (Uniques_Middle))
	,(Operator (Math_NaturalExponent))
	,(Operator (Set_Permutations))
	,(Operator (Math_GreatestCommonDivisor))
	,(Operator (Set_Maximum))
	,(Operator (Math_ImaginaryPart))
	,(Operator (Math_RealPart))
	,(Operator (Set_AntiFilter))
	,(Operator (Math_Logarithm))
	,(Operator (Set_Combinations))
	,(Operator (Chars_JoinWithNewlines))
	,(Operator (Logic_IsOrdered))
	,(Operator (Logic_IsPrime))
	,(Variable (Quine))
	,(Operator (Logic_IsReal))
	,(Operator (Unary (asin)))//(Operator (Math_ArcSine))
	,(Operator (Unary (atan)))//(Operator (Math_ArcTangent))
	,(Stack (Reverse_Middle))
	,(Operator (Math_Average))
	,(Stack (Delete_Middle))
	,(Operator (Chars_ToUppercase))
	,(Operator (Logic_IsUppercase))
	,(Operator (Logic_IsFinite))
	,(Control (Goto West Nothing))
	,(Control (Mirror True Inverse))
	,(Control (Goto East Nothing))
	,(Operator (Binary (^)))//(Operator (Math_Exponent))
	,(Operator (Math_Floor))
	,(Operator (Math_Differential))
	,(Literal (Alphabet Lowercase))
	,(Operator (Math_ConvertFromBase))
	,(Operator (Unary (cos)))//(Operator (Math_CoSine))
	,(Stack (Duplicates_Middle))
	,(Operator (Unary (ln)))//(Operator (Math_NaturalLogarithm))
	,(Operator (Math_Permutations))
	,(Operator (Math_LeastCommonMultiple))
	,(Operator (Set_Minimum))
	,(Operator (Math_ImaginaryUnit))
	,(Operator (Math_ComplexSplit))
	,(Operator (Set_Filter))
	,(Operator (Unary (log10)))//(Operator (Math_Base10Logarithm))
	,(Operator (Math_Combinations))
	,(Operator (Chars_SplitOnNewlines))
	,(Operator (Set_MakeOrdered))
	,(Operator (Math_PrimeFactors))
	,(Variable (History))
	,(Operator (Math_Round))
	,(Operator (Unary (sin)))//(Operator (Math_Sine))
	,(Operator (Unary (tan)))//(Operator (Math_Tangent))
	,(Stack (Rotate_Middle))
	,(Operator (Unary (abs)))//(Operator (Math_Abs))
	,(Stack (Drop_Middle))
	,(Operator (Logic_IsLowercase))
	,(Operator (Chars_ToLowercase))
	,(Operator (Math_Conjugate))
	,(Control (Loop_Right West Nothing))
	,(Control (Mirror True Vertical))
	,(Control (Loop_Right East Nothing))
	,(Operator (Unary (~)))//(Operator (Math_Negation))
	,(Operator (IO_WriteAll))
	,(Operator (IO_ReadAll))
	,(Operator (IO_ReadWrite))
	,(Operator (IO_WriteRead))
	,(Operator (IO_Interrobang))
	,(Operator (Binary (*)))//(Operator (Math_Multiplication))
	,(Operator (Vector_Multiplication))
	,(Operator (Binary (/)))//(Operator (Math_Division))
	,(Operator (Vector_Addition))
	,(Operator (Unary (sqrt)))//(Operator (Math_SquareRoot))
	,(Operator (Math_Reciprocal))
	,(Operator (Logic_IsInfinite))
	,(Operator (Bitwise_Or))
	,(Operator (Vector_Or))
	,(Operator (Bitwise_And))
	,(Operator (Vector_And))
	,(Operator (Bitwise_Xor))
	,(Operator (Bitwise_Not))
	,(Operator (Bitwise_Nor))
	,(Operator (Bitwise_Nand))
	,(Operator (Bitwise_Xnor))
	,(Operator (Set_Intersection))
	,(Operator (Set_Union))
	,(Operator (Logic_ElementOf))
	,(Operator (Logic_Contains))
	,(Operator (Vector_LessThan))
	,(Operator (Vector_GreaterThan))
	,(Operator (Vector_Equality))
	,(Operator (Logic_LessOrEqual))
	,(Operator (Vector_LessOrEqual))
	,(Operator (Logic_GreaterOrEqual))
	,(Operator (Vector_GreaterOrEqual))
	,(Operator (Logic_Inequality))
	,(Operator (Logic_SetEquality))
	,(Operator (Logic_SetInequality))
	,(Operator (Logic_SubsetNotEqual))
	,(Operator (Logic_SupersetNotEqual))
	,(Operator (Logic_SubsetOrEqual))
	,(Operator (Logic_SupersetOrEqual))
	,(Operator (Logic_NotSubsetNorEqual))
	,(Operator (Logic_NotSupersetNorEqual))
	,(Operator (Math_RadiansToDegrees))
	,(Operator (Math_DegreesToRadians))
	,(Operator (Math_Ceiling))
	,(Operator (Bitwise_LeftShift))
	,(Operator (Bitwise_RightShift))
	,(Operator (IO_Sleep))
	,(Operator (IO_Timestamp))
	,(Literal (Pi))
	,(Stack (SwapLeftRight))
	,(Stack (MoveAll NorthWest))
	,(Stack (MoveAll NorthEast))
	,(Stack (MoveAll SouthEast))
	,(Stack (MoveAll SouthWest))
	,(Stack (Reverse_Left))
	,(Stack (Reverse_Right))
	,(Stack (Reverse_Both))
	,(Stack (Reverse_Primary))
	,(Stack (Reverse_Base))
	,(Stack (Reverse_All))
	,(Stack (Rotate_Left))
	,(Stack (Rotate_Right))
	,(Stack (Rotate_Both))
	,(Stack (Rotate_Primary))
	,(Stack (Rotate_Base))
	,(Stack (Rotate_All))
	,(Stack (Delete_Left))
	,(Stack (Delete_Right))
	,(Stack (Delete_Both))
	,(Stack (Delete_Base))
	,(Stack (Delete_Main))
	,(Stack (Delete_All))
	,(Stack (Drop_Left))
	,(Stack (Drop_Right))
	,(Stack (Drop_Both))
	,(Stack (Drop_Base))
	,(Stack (Drop_Main))
	,(Stack (Unpack_RightLeft))
	,(Stack (Unpack_LeftRight))
	,(Stack (SwapTop Inverse))
	,(Stack (SwapTop Identity))
	,(Stack (SwapTop Horizontal))
	,(Stack (SwapTop Vertical))
	,(Stack (MoveTop East))
	,(Stack (MoveTop West))
	,(Stack (MoveTop North))
	,(Stack (MoveTop South))
	,(Stack (MoveTop NorthWest))
	,(Stack (MoveTop NorthEast))
	,(Stack (MoveTop SouthEast))
	,(Stack (MoveTop SouthWest))
	,(Stack (CopyTop East))
	,(Stack (CopyTop West))
	,(Stack (CopyTop North))
	,(Stack (CopyTop NorthWest))
	,(Stack (CopyTop NorthEast))
	,(Stack (CopyTop SouthEast))
	,(Stack (CopyTop SouthWest))
	,(Stack (CopyBoth Horizontal))
	,(Stack (CopyBoth Vertical))
	,(Stack (CycleFull Clockwise))
	,(Stack (CycleFull Anticlockwise))
	,(Stack (CycleTops Clockwise))
	,(Stack (CycleTops Anticlockwise))
	,(Operator (Math_Product))
	,(Operator (Math_Sum))
	,(Operator (Logic_Any))
	,(Operator (Logic_None))
	,(Operator (Set_PowerSet))
	,(Operator (Set_Subsets))
	,(Control (Bounce NorthEast))
	,(Control (Bounce NorthWest))
	,(Control (Bounce SouthEast))
	,(Control (Bounce SouthWest))
	,(Control (Mirror True Reflection))
	,(Control (Mirror False Reflection))
	,(Control (Skip True))
	,(Control (Skip False))
	,(Stack (Uniques_Main))
	,(Stack (Uniques_Base))
	,(Stack (Duplicates_Main))
	,(Stack (Duplicates_Base))
	,(Stack (Replicate_Base))
	,(Stack (Replicate_TopOfMiddle))
	,(Stack (Replicate_AllOfMiddle))
	,(Stack (Repeat_TopOfMiddle))
	,(Stack (Repeat_AllOfMiddle))
	,(Operator (Math_Maximum))
	,(Operator (Math_Minimum))
	]
	