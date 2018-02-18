definition module types

:: XYPair
	= {
		x :: Int,
		y :: Int
	}
	
:: State
	= {
		dimension :: XYPair,
		location :: XYPair,
		direction :: CardinalDirection,
		program :: !{{!Token}},
		random :: [Int],
		history :: [Token],
		memory :: Memory
	}

:: Memory
	= {
		left :: [Number],
		right :: [Number],
		bases :: [Int],
		main :: [[Number]]
	}

:: Numeric
	= Int Int
	| Real Real

:: Number
	= Zero
	| Re (Magnitude Sign Numeric)
	| Im (Magnitude Sign Numeric)
	| Cx (Magnitude Directed Complex)
	| NaN

:: Magnitude inf fin
	= Fin fin
	| Inf inf
	
:: Sign
	= Positive
	| Negative
	
:: Complex
	= {
		re :: Numeric,
		im :: Numeric
	}
	
:: Directed
	= Directed
	
:: Token
	= Control
	| Literal
	| Stack
	| Operator
	| Variable
	
:: Operator
	= Operator_IO_ReadAll
	| Operator_IO_WriteAll
	| Operator_IO_ReadWrite
	| Operator_IO_WriteRead
	| Operator_IO_WriteOnce
	| Operator_IO_ReadOnce
	| Operator_IO_Interrobang
	| Operator_IO_Bell
	| Operator_IO_Sleep
	| Operator_Math_Modulus
	| Operator_Math_Addition
	| Operator_Math_Multiplication
	| Operator_Math_Subtraction
	| Operator_Math_Division
	| Operator_Math_Logarithm
	| Operator_Math_Exponent
	| Operator_Math_DotProduct
	| Operator_Math_Sum
	| Operator_Math_Average
	| Operator_Math_Product
	| Operator_Math_SquareRoot
	| Operator_Math_Negate
	| Operator_Math_Conjugate
	| Operator_Math_RealPart
	| Operator_Math_ImagPart
	| Operator_Math_ImagUnit
	| Operator_Math_ImagSplit
	| Operator_Math_Floor
	| Operator_Math_Abs
	| Operator_Math_Ceiling
	| Operator_Math_Round
	| Operator_Math_Sine
	| Operator_Math_CoSine
	| Operator_Math_Tangent
	| Operator_Math_ArcSine
	| Operator_Math_ArcCoSine
	| Operator_Math_ArcTangent
	| Operator_Math_Reciprocal
	| Operator_Math_RadiansToDegrees
	| Operator_Math_DegreesToRadians
	| Operator_Math_Minimum
	| Operator_Math_Maximum
	| Operator_Math_Permutations
	| Operator_Math_Combinations
	| Operator_Math_PrimeFactors
	| Operator_Math_GreatestCommonDivisor
	| Operator_Math_LeastCommonMultiple
	| Operator_Math_NaturalExponent
	| Operator_Math_NaturalLogarithm
	| Operator_Math_ConvertToBase
	| Operator_Math_ConvertFromBase
	| Operator_Math_Integral
	| Operator_Math_Differential
	| Operator_Bitwise_And
	| Operator_Bitwise_Or
	| Operator_Bitwise_Xor
	| Operator_Bitwise_Xnor
	| Operator_Bitwise_Nor
	| Operator_Bitwose_Not
	| Operator_Bitwise_Nand
	| Operator_Bitwise_LeftShift
	| Operator_Bitwise_RightShift
	| Operator_Logic_Equality
	| Operator_Logic_Inequality
	| Operator_Logic_LessThan
	| Operator_Logic_GreaterThan
	| Operator_Logic_LessOrEqual
	| Operator_Logic_GreaterOrEqual
	| Operator_Logic_SetEquality
	| Operator_Logic_SetInequality
	| Operator_Logic_ElementOf
	| Operator_Logic_ContainedIn
	| Operator_Logic_SubsetOrEqual
	| Operator_Logic_SubsetNotEqual
	| Operator_Logic_NotSubsetNorEqual
	| Operator_Logic_SupersetOrEqual
	| Operator_Logic_SupersetNotEqual
	| Operator_Logic_NotSupersetNorEqual
	| Operator_Logic_Any
	| Operator_Logic_None
	| Operator_Logic_IsOrdered
	| Operator_Logic_IsLowercase
	| Operator_Logic_IsUppercase
	| Operator_Logic_IsPrime
	| Operator_Logic_IsReal
	| Operator_Logic_IsComplex
	| Operator_Logic_IsInfinite
	| Operator_Vector_Multiplication
	| Operator_Vector_Addition
	| Operator_Vector_Equality
	| Operator_Vector_LessThan
	| Operator_Vector_GreaterThan
	| Operator_Vector_LessOrEqual
	| Operator_Vector_GreaterOrEqual
	| Operator_Range_FromLeftStepRight
	| Operator_Range_FromMiddleToZero
	| Operator_Range_FromMiddleAvoidZero
	| Operator_Range_FromLeftTimesRight
	| Operator_Set_PowerSet
	| Operator_Set_Subsets
	| Operator_Set_Permutations
	| Operator_Set_Combinations
	| Operator_Set_MakeOrdered
	| Operator_Set_Length
	| Operator_Set_Filter
	| Operator_Set_Intersection
	| Operator_Set_Union
	| Operator_Set_Minimum
	| Operator_Set_Maximuim
	| Operator_Alphabet_ToLowercase
	| Operator_Alphabet_ToUppercase
	
:: Stack
	= Stack_Reverse_Left
	| Stack_Reverse_Right
	| Stack_Reverse_Middle
	| Stack_Reverse_Both
	| Stack_Reverse_Primary
	| Stack_Reverse_Base
	| Stack_Reverse_All
	| Stack_Rotate_Left
	| Stack_Rotate_Right
	| Stack_Rotate_Middle
	| Stack_Rotate_Both
	| Stack_Rotate_Primary
	| Stack_Rotate_Base
	| Stack_Rotate_All
	| Stack_Delete_Left
	| Stack_Delete_Right
	| Stack_Delete_Middle
	| Stack_Delete_Both
	| Stack_Delete_Base
	| Stack_Delete_Main
	| Stack_Delete_All
	| Stack_Drop_Left
	| Stack_Drop_Right
	| Stack_Drop_Middle
	| Stack_Drop_Both
	| Stack_Drop_Base
	| Stack_Drop_Main
	| Stack_CycleTops Rotation
	| Stack_CycleFull Rotation
	| Stack_Unpack_LeftRight
	| Stack_Unpack_RightLeft
	| Stack_SwapTop CompassAxis
	| Stack_SwapLeftRight
	| Stack_MoveTop CompassDirection
	| Stack_MoveAll DiagonalDirection
	| Stack_CopyTop CompassDirection
	| Stack_CopyBoth CardinalAxis
	| Stack_Replicate_Base
	| Stack_Replicate_TopOfMiddle
	| Stack_Replicate_AllOfMiddle
	| Stack_Repeat_TopOfMiddle
	| Stack_Repeat_AllOfMiddle
	| Stack_Uniques_Middle
	| Stack_Uniques_Main
	| Stack_Uniques_Base
	| Stack_Duplicates_Middle
	| Stack_Duplicates_Main
	| Stack_Duplicates_Base
	| Stack_ShiftBase CardinalDirection
	| Stack_JoinFromBase
	| Stack_AdjustOffset
	
:: Variable
	= Variable_Random
	| Variable_Quine
	| Variable_History
	| Variable_Time
	
:: Literal
	= Literal_Digit Digit
	| Literal_Quote
	| Literal_Pi
	| Literal_UpperAlpha
	| Literal_LowerAlpha
	
:: Digit
	= Digit_Zero
	| Digit_One
	| Digit_Two
	| Digit_Three
	| Digit_Four
	| Digit_Five
	| Digit_Six
	| Digit_Seven
	| Digit_Eight
	| Digit_Nine
	
:: Control
	= Control_Terminate
	| Control_Start CardinalDirection
	| Control_Move Bool CardinalDirection
	| Control_Bounce Bool DiagonalDirection
	| Control_Random Bool CardinalAxis
	| Control_Mirror Bool AnyAxis
	| Control_Turn Rotation
	| Control_Loop StackID3 CardinalDirection
	| Control_String
	| Control_NOOP
	| Control_LINE

:: Rotation
	= Clockwise
	| Anticlockwise
	
:: StackID1
	= Middle

:: StackID2
	= Left
	| Right
	
:: StackID3
	= StackID1
	| StackID2
	
:: StackID5
	= StackID3
	| Main
	| Base
	
:: CardinalAxis
	= Vertical
	| Horizontal
	
:: DiagonalAxis
	= Inverse
	| Identity
	
:: CompassAxis
	= CardinalAxis
	| DiagonalAxis
	
:: AnyAxis
	= CompassAxis
	| Reflection
	
:: CardinalDirection
	= North
	| South
	| East
	| West

:: CompassDirection
	= CardinalDirection
	| DiagonalDirection

:: DiagonalDirection
	= NorthWest
	| NorthEast
	| SouthWest
	| SouthEast