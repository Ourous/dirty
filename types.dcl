definition module types

import StdMaybe

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
		wrapping :: !Bool
	}	
	
:: State	
	= {
		direction :: !Direction,
		location :: !XYPair,
		history :: !Char,
		terminate :: !Bool
	}

:: Memory
	= {
		left :: [Number],
		right :: [Number],
		main :: [Element],
		random :: ![Int]
	}
	
:: Element
	= El [Number]
	| Delim Bool // "is active"
	
:: Flags
	= {
		debug :: !Bool,
		dump :: !Bool,
		nums :: !Bool
	}

:: Numeric
	= Int !Int
	| Real !Real

:: Number
	= Zero
	| Re !(Magnitude Sign Numeric)
	| Im !(Magnitude Sign Numeric)
	| Cx !(Magnitude Directed Complex)
	| NaN

:: Magnitude inf fin
	= Fin !fin
	| Inf inf
	
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
	| Start Orientation
	| Change Direction 
	| Bounce Direction
	| Either Axes
	| Mirror Bool Axes
	| Skip Bool
	| Turn Rotation
	| Loop StackID Direction (Maybe XYPair)
	| Goto Direction (Maybe XYPair)
	| String
	| Restart
	| NOOP
	| LINE
	
:: Orientation
	= Dir Direction
	| Axis Axes
	
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
	| IO_ClearConsole
	| IO_Backspace
	| IO_Environment
	| Binary_NN_N (Number Number -> Number)
	| Binary_NN_S (Number Number -> [Number])
	| Binary_SN_N ([Number] Number -> Number)
	| Binary_SN_S ([Number] Number -> [Number])
	| Binary_NS_N (Number [Number] -> Number)
	| Binary_NS_S (Number [Number] -> [Number])
	| Binary_SS_N ([Number] [Number] -> Number)
	| Binary_SS_S ([Number] [Number] -> [Number])
	| Unary_N_N (Number -> Number)
	| Unary_N_S (Number -> [Number])
	| Unary_S_N ([Number] -> Number)
	| Unary_S_S ([Number] -> [Number])
	| Unary_M_M (Memory -> Memory)
	| Math_Logarithm
	| Math_DotProduct
	| Math_Sum
	| Math_Product
	| Math_Conjugate
	| Math_RealPart
	| Math_ImaginaryPart
	| Math_ImaginaryUnit
	| Math_ComplexSplit
	| Math_Reciprocal
	| Math_Minimum
	| Math_Maximum
	| Math_Permutations
	| Math_Combinations
	| Math_PrimeFactors
	| Math_GreatestCommonDivisor
	| Math_LeastCommonMultiple
	| Math_ConvertToBase
	| Math_ConvertFromBase
	| Math_Average
	| Bitwise_LeftShift
	| Bitwise_RightShift
	| Logic_Equality
	| Logic_Inequality
	| Logic_LessThan
	| Logic_GreaterThan
	| Logic_LessOrEqual
	| Logic_GreaterOrEqual
	| Logic_SetEquality
	| Logic_ElementOf
	| Logic_SubsetOrEqual
	| Logic_SubsetNotEqual
	| Logic_NotSubsetNorEqual
	| Logic_Any
	| Logic_All
	| Logic_IsOrdered
	| Logic_IsLowercase
	| Logic_IsUppercase
	| Logic_IsPrime
	| Logic_IsReal
	| Logic_IsFinite
	| Logic_IsInfinite
	| Logic_Negation
	| Logic_Coalesce
	| Vector_And
	| Vector_Or
	| Vector_Multiplication
	| Vector_Addition
	| Vector_Subtraction
	| Vector_Equality
	| Vector_LessThan
	| Vector_GreaterThan
	| Vector_LessOrEqual
	| Vector_GreaterOrEqual
	| Vector_Negation
	| Vector_ElementOf
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
	| Set_Exclusion
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
	| Duplicates_Main
	| Duplicates_Middle
	| Duplicates_Base
	| ShiftBase Direction
	| JoinFromBase
	| AdjustOffset

:: Rotation
	= Clockwise
	| Anticlockwise
	
:: StackID
	= Middle
	| Left
	| Right
	| Both
	| Primary
	| Base
	| Main
	| All
	
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