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
	,Control_Move Depends East
	,Control_Move Always West
	,Control_Move Depends West
	,Control_Move Always North
	,Control_Move Depends North
	,Control_Move Always South
	,Control_Move Depends South
	,Control_Random Always Horizontal
	,Control_Random Depends Horizontal
	,Control_Random Always Vertical
	,Control_Random Depends Vertical
	,Control_Mirror Depends Vertical
	,Control_Mirror Depends Horizontal
	,Control_Mirror Depends Identity
	,Control_Mirror Depends Inverse
	,Operator_IO_WriteAll
	,Operator_IO_ReadAll
	,Control_NOOP
	,Operator_IO_OutputOnce
	,Literal_Quote
	,Variable_Random
	,Operator_Math_Integral
	,Operator_Math_Modulus
	,Stack_JoinFromBase
	,Control_String
	,Control_Loop Left West
	,Control_Loop Left East
	,Operator_Math_DotProduct
	,Operator_Math_Addition
	,Operator_Range_FromLeftStepRight
	,Operator_Math_Subtraction
	,Operator_Range_FromMiddleToZero
	,Control_Mirror Always Identity
	,Literal_Digit Digit_Zero
	,Literal_Digit Digit_One
	,Literal_Digit Digit_Two
	,Literal_Digit Digit_Three
	,Literal_Digit Digit_Four
	,Literal_Digit Digit_Five
	,Literal_Digit Digit_Six
	,Literal_Digit Digit_Seven
	,Literal_Digit Digit_Eight
	,Literal_Digit Digit_Nine
	,Operator_Range_FromMiddleAvoidZero
	,Operator_Range_FromLeftTimesRight
	,Operator_Logic_LessThan
	,Operator_Logic_Equality
	,Operator_Logic_GreaterThan
	,Operator_IO_ReadOnce
	,Stack_AdjustOffset
	,Literal_UpperAlpha
	,Operator_Math_ConvertToBase
	,Operator_Math_ArcCoSine
	,Stack_Uniques_Middle
	,Operator_Math_NaturalExponent
	,Operator_Set_Permutations
	,Operator_Math_GreatestCommonDivisor
	,Operator_Set_Maximum
	,Operator_Math_ImaginaryPart
	,Operator_Math_RealPart
	,Operator_Set_AntiFilter
	,Operator_Math_Logarithm
	,Operator_Set_Combinations
	,