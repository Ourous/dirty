definition module types

:: Numeric
	= Int Int
	| Real Real

:: Number
	= Rational Numeric
	| Imaginary Numeric
	| Complex Numeric Numeric
	| Infinity
	| NaN
	
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
	| Operator_Math_Modulus
	| Operator_Math_Addition
	| Operator_Math_Multiplication
	| Operator_Math_Subtraction
	| Operator_Math_Division
	| Operator_Math_Logarithm
	| Operator_Math_Exponentiation
	| Operator_Math_DotProduct
	| Operator_Math_Sum
	| Operator_Math_Average
	| Operator_Math_Product
	| Operator_Math_SquareRoot
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
	| Operator_Math_Ar
	
	
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
	| Stack_MoveAll_MiddleToLeft
	| Stack_MoveAll_MiddleToRight
	| Stack_CopyTop CompassDirection
	| Stack_CopyBoth CardinalAxis
	| Stack_Replicate_Base
	| Stack_Replicate_Main
	| Stack_Replicate_Middle
	| Stack_Uniques_Middle
	| Stack_Uniques_Main
	| Stack_Uniques_Base
	| Stack_Duplicates_Middle
	| Stack_Duplicates_Main
	| Stack_Duplicates_Base
	| Stack_ShiftBase CardinalDirection
	
:: Variable
	= Variable_Random
	| Variable_Quine
	| Variable_Program
	| Variable_Time
	
	
:: Literal
	= Literal_Infinity
	| Literal_Digit Digit
	| Literal_Quote
	| Literal_Pi
	| Literal_E
	| Literal_UpperAlpha
	| Literal_LowerAlpha
	
:: Digit
	= Zero
	| One
	| Two
	| Three
	| Four
	| Five
	| Six
	| Seven
	| Eight
	| Nine
	
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
	
