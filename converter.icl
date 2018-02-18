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
	,Operator_IO_WriteOnce
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
	,Operator_Chars_JoinWithNewlines
	,Operator_Logic_IsOrdered
	,Operator_Logic_IsPrime
	,Variable_Quine
	,Operator_Logic_IsReal
	,Operator_Math_ArcSine
	,Operator_Math_ArcTangent
	,Stack_Reverse_Middle
	,Operator_Math_Average
	,Stack_Delete_Middle
	,Operator_Chars_ToUppercase
	,Operator_Logic_IsUppercase
	,Operator_Logic_IsFinite
	,Control_Loop Middle West
	,Control_Mirror Always Inverse
	,Control_Loop Middle East
	,Operator_Math_Exponent
	,Operator_Math_Floor
	,Operator_Math_Differential
	,Literal_LowerAlpha
	,Operator_Math_ConvertFromBase
	,Operator_Math_CoSine
	,Stack_Duplicates_Middle
	,Operator_Math_NaturalLogarithm
	,Operator_Math_Permutations
	,Operator_Math_LeastCommonMultiple
	,Operator_Set_Minimum
	,Operator_Math_ImaginaryUnit
	,Operator_Math_ComplexSplit
	,Operator_Set_Filter
	,Operator_Math_Base10Logarithm
	,Operator_Math_Combinations
	,Operator_Chars_SplitOnNewlines
	,Operator_Set_MakeOrdered
	,Operator_Math_PrimeFactors
	,Variable_History
	,Operator_Math_Round
	,Operator_Math_Sine
	,Operator_Math_Tangent
	,Stack_Rotate_Middle
	,Operator_Math_Abs
	,Stack_Drop_Middle
	,Operator_Logic_IsLowercase
	,Operator_Chars_ToLowercase
	,Operator_Math_Conjugate
	,Control_Loop Right West
	,Control_Mirror Always Vertical
	,Control_Loop Right East
	,Operator_Math_Negation
	,Operator_IO_ReadWrite
	,Operator_IO_WriteRead
	,Operator_IO_Interrobang
	,Operator_Math_Multiplication
	,Operator_Vector_Multiplication
	,Operator_Math_Division
	,Operator_Vector_Addition
	,Operator_Math_SquareRoot
	,Operator_Math_Reciprocal
	,Operator_Logic_IsInfinite
	,Operator_Bitwise_Or
	,Operator_Vector_Or
	,Operator_Bitwise_And
	,Operator_Vector_And
	,Operator_Bitwise_Xor
	,Operator_Bitwise_Not
	,Operator_Bitwise_Nor
	,Operator_Bitwise_Nand
	,Operator_Bitwise_Xnor
	,Operator_Set_Intersection
	,Operator_Set_Union
	,Operator_Logic_ElementOf
	,Operator_Logic_Contains
	,Operator_Vector_LessThan
	,Operator_Vector_GreaterThan
	,Operator_Vector_Equality
	,Operator_Logic_LessOrEqual
	,Operator_Vector_LessOrEqual
	,Operator_Logic_GreaterOrEqual
	,Operator_Vector_GreaterOrEqual
	,Operator_Logic_Inequality
	,Operator_Logic_SetEquality
	,Operator_Logic_SetInequality
	,Operator_Logic_SubsetNotEqual
	,Operator_Logic_SupersetNotEqual
	,Operator_Logic_SubsetOrEqual
	,Operator_Logic_SupersetOrEqual
	,Operator_Logic_NotSubsetNorEqual
	,Operator_Logic_NotSupersetNorEqual
	,Operator_Math_RadiansToDegrees
	,Operator_Math_DegreesToRadians
	,Operator_Math_Ceiling
	,Operator_Bitwise_LeftShift
	,Operator_Bitwise_RightShift
	,Operator_IO_Sleep
	,Variable_Time
	,Literal_Pi
	,Stack_SwapLeftRight
	,Stack_MoveAll NorthWest
	,Stack_MoveAll NorthEast
	,Stack_MoveAll SouthEast
	,Stack_MoveAll SouthWest
	,Stack_Reverse_Left
	,Stack_Reverse_Right
	,Stack_Reverse_Both
	,Stack_Reverse_Primary
	,Stack_Reverse_Base
	,Stack_Reverse_All
	,Stack_Rotate_Left
	,Stack_Rotate_Right
	,Stack_Rotate_Both
	,Stack_Rotate_Primary
	,Stack_Rotate_Base
	,Stack_Rotate_All
	,Stack_Delete_Left
	,Stack_Delete_Right
	,Stack_Delete_Both
	,Stack_Delete_Base
	,Stack_Delete_Main
	,Stack_Delete_All
	,Stack_Drop_Left
	,Stack_Drop_Right
	,Stack_Drop_Both
	,Stack_Drop_Base
	,Stack_Drop_Main
	,Stack_Unpack_RightLeft
	,Stack_Unpack_LeftRight
	,Stack_SwapTop Inverse
	,Stack_SwapTop Identity
	,Stack_SwapTop Horizontal
	,Stack_SwapTop Vertical
	,Stack_MoveTop East
	,Stack_MoveTop West
	,Stack_MoveTop North
	,Stack_MoveTop South
	,Stack_MoveTop NorthWest
	,Stack_MoveTop NorthEast
	,Stack_MoveTop SouthEast
	,Stack_MoveTop SouthWest
	,Stack_CopyTop East
	,Stack_CopyTop West
	,Stack_CopyTop North
	,Stack_CopyTop NorthWest
	,Stack_CopyTop NorthEast
	,Stack_CopyTop SouthEast
	,Stack_CopyTop SouthWest
	,Stack_CopyBoth Horizontal
	,Stack_CopyBoth Vertical
	,Stack_CycleFull Clockwise
	,Stack_CycleFull Anticlockwise
	,Stack_CycleTops Clockwise
	,Stack_CycleTops Anticlockwise
	,Operator_Math_Product
	,Operator_Math_Sum
	,Operator_Logic_Any
	,Operator_Logic_None
	,Operator_Set_PowerSet
	,Operator_Set_Subsets
	,Control_Bounce Always NorthEast
	,Control_Bounce Depends NorthEast
	,Control_Bounce Always NorthWest
	,Control_Bounce Depends NorthWest
	,Control_Bounce Always SouthEast
	,Control_Bounce Depends SouthEast
	,Control_Bounce Always SouthWest
	,Control_Bounce Depends SouthWest
	,Control_Mirror Always Reflection
	,Control_Mirror Depends Reflection
	,Stack_Uniques_Main
	,Stack_Uniques_Base
	,Stack_Duplicates_Main
	,Stack_Duplicates_Base
	,Stack_Replicate_Base
	,Stack_Replicate_TopOfMiddle
	,Stack_Replicate_AllOfMiddle
	,Stack_Repeat_TopOfMiddle
	,Stack_Repeat_AllOfMiddle
	,Operator_Math_Maximum
	,Operator_Math_Minimum
	]
	