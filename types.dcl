definition module types

:: XYPair
	= {
		x :: !Int,
		y :: !Int
	}
	
:: Program
	= {
		dimension :: !XYPair,
		source :: !{{#Char}},
		commands :: !{{Command}},
		wrapping :: !Bool,
		direction :: !Direction,
		location :: !XYPair
	}	
	
:: *State
	= {
		direction :: !Direction,
		location :: !*XYPair,
		history :: !*Char
	}

:: Memory
	= {
		left :: MonoStack,
		right :: MonoStack,
		main :: !MultiStack,
		random :: [Int]
	}
	
:: MonoStack
	:== [Number]
	
:: MultiStack
	:== [MultiStackElement]
	
:: MultiStackElement
	= El MonoStack
	| Delimiter
	
:: Flags
	= {
		debug :: Bool,
		dump :: Bool,
		nums :: Bool
	}

:: Numeric
	= Int !Int
	| Real !Real

:: Number
	= Zero
	| Re (Magnitude Sign Numeric)
	| Im (Magnitude Sign Numeric)
	| Cx (Magnitude Directed Complex)
	| NaN

:: Magnitude inf fin
	= Fin !fin
	| Inf !inf
	
:: Sign 
	= Positive
	| Negative
	
:: Complex
	= {
		re :: !Numeric,
		im :: !Numeric
	}
	
:: Directed
	= Directed
	
:: Command
	= Control ControlCommand
	| Literal LiteralCommand
	| Variable VariableCommand
	| Operator OperatorCommand
	| Stack StackCommand
	
:: ControlCommand
	= Terminate
	| Start Direction
	| Change Bool Direction 
	| Bounce Bool Direction
	| Either Bool Axes
	| Mirror Bool Axes
	| Turn Rotation
	| Loop StackID3 Direction
	| String
	| NOOP
	| LINE
	
:: LiteralCommand
	= Pi
	| Quote
	| Digit Number
	| Alphabet LetterCase
	
:: VariableCommand
	= Random
	| Quine
	| History
	
:: LetterCase
	= Uppercase
	| Lowercase
	
:: OperatorCommand
	= IO_WriteAll
	| IO_ReadAll
	| IO_ReadWrite
	| IO_WriteRead
	| IO_WriteOnce
	| IO_ReadOnce
	| IO_Interrobang
	| IO_Bell
	| IO_Timestamp
	| IO_Sleep
	| Binary (Number Number -> Number)
	| Unary (Number -> Number)
	| Math_Modulus
	| Math_Addition
	| Math_Multiplication
	| Math_Subtraction
	| Math_Division
	| Math_Logarithm
	| Math_Exponent
	| Math_DotProduct
	| Math_Sum
	| Math_Product
	| Math_SquareRoot
	| Math_Negation
	| Math_Conjugate
	| Math_RealPart
	| Math_ImaginaryPart
	| Math_ImaginaryUnit
	| Math_ComplexSplit
	| Math_Floor
	| Math_Abs
	| Math_Ceiling
	| Math_Round
	| Math_Sine
	| Math_CoSine
	| Math_Tangent
	| Math_ArcSine
	| Math_ArcCoSine
	| Math_ArcTangent
	| Math_Reciprocal
	| Math_RadiansToDegrees
	| Math_DegreesToRadians
	| Math_Minimum
	| Math_Maximum
	| Math_Permutations
	| Math_Combinations
	| Math_PrimeFactors
	| Math_GreatestCommonDivisor
	| Math_LeastCommonMultiple
	| Math_NaturalExponent
	| Math_NaturalLogarithm
	| Math_Base10Logarithm
	| Math_ConvertToBase
	| Math_ConvertFromBase
	| Math_Integral
	| Math_Differential
	| Math_Average
	| Bitwise_And
	| Bitwise_Or
	| Bitwise_Xor
	| Bitwise_Xnor
	| Bitwise_Nor
	| Bitwise_Not
	| Bitwise_Nand
	| Bitwise_LeftShift
	| Bitwise_RightShift
	| Logic_Equality
	| Logic_Inequality
	| Logic_LessThan
	| Logic_GreaterThan
	| Logic_LessOrEqual
	| Logic_GreaterOrEqual
	| Logic_SetEquality
	| Logic_SetInequality
	| Logic_ElementOf
	| Logic_Contains
	| Logic_SubsetOrEqual
	| Logic_SubsetNotEqual
	| Logic_NotSubsetNorEqual
	| Logic_SupersetOrEqual
	| Logic_SupersetNotEqual
	| Logic_NotSupersetNorEqual
	| Logic_Any
	| Logic_None
	| Logic_IsOrdered
	| Logic_IsLowercase
	| Logic_IsUppercase
	| Logic_IsPrime
	| Logic_IsReal
	| Logic_IsFinite
	| Logic_IsInfinite
	| Vector_And
	| Vector_Or
	| Vector_Multiplication
	| Vector_Addition
	| Vector_Equality
	| Vector_LessThan
	| Vector_GreaterThan
	| Vector_LessOrEqual
	| Vector_GreaterOrEqual
	| Range_FromLeftStepRight
	| Range_FromMiddleToZero
	| Range_FromMiddleAvoidZero
	| Range_FromLeftTimesRight
	| Set_PowerSet
	| Set_Subsets
	| Set_Permutations
	| Set_Combinations
	| Set_MakeOrdered
	| Set_Length
	| Set_Filter
	| Set_AntiFilter
	| Set_Intersection
	| Set_Union
	| Set_Minimum
	| Set_Maximum
	| Chars_ToLowercase
	| Chars_ToUppercase
	| Chars_JoinWithNewlines
	| Chars_SplitOnNewlines
	
:: StackCommand
	= Reverse_Left
	| Reverse_Right
	| Reverse_Middle
	| Reverse_Both
	| Reverse_Primary
	| Reverse_Base
	| Reverse_All
	| Rotate_Left
	| Rotate_Right
	| Rotate_Middle
	| Rotate_Both
	| Rotate_Primary
	| Rotate_Base
	| Rotate_All
	| Delete_Left
	| Delete_Right
	| Delete_Middle
	| Delete_Both
	| Delete_Base
	| Delete_Main
	| Delete_All
	| Drop_Left
	| Drop_Right
	| Drop_Middle
	| Drop_Both
	| Drop_Base
	| Drop_Main
	| CycleTops Rotation
	| CycleFull Rotation
	| Unpack_LeftRight
	| Unpack_RightLeft
	| SwapTop Axes
	| SwapLeftRight
	| MoveTop Direction
	| MoveAll Direction
	| CopyTop Direction
	| CopyBoth Axes
	| Replicate_Base
	| Replicate_TopOfMiddle
	| Replicate_AllOfMiddle
	| Repeat_TopOfMiddle
	| Repeat_AllOfMiddle
	| Uniques_Middle
	| Uniques_Main
	| Uniques_Base
	| Duplicates_Middle
	| Duplicates_Main
	| Duplicates_Base
	| ShiftBase Direction
	| JoinFromBase
	| AdjustOffset

:: Rotation
	= Clockwise
	| Anticlockwise
	
:: StackID3
	= Middle
	| Left
	| Right
	
:: Axes
	= Reflection
	| Inverse
	| Identity
	| Vertical
	| Horizontal
	
:: Direction
	= North
	| South
	| East
	| West
	| NorthWest
	| NorthEast
	| SouthWest
	| SouthEast