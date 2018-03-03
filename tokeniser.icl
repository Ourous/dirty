implementation module tokeniser

import types, arithmetic, builtins, unicode, StdEnv, StdLib

unicodeToNative :: Int -> Char
unicodeToNative char
	# index = findIndex (\e -> any ((==) char) e) unicodeCharset
	| isNothing index
		= abort "No corresponding native character!"
	| otherwise
		= toChar (fromJust index)
		
unicodeCharset :: [[Int]]
unicodeCharset =
	[[8680]
	,[8678]
	,[8679]
	,[8681]
	,[11116]
	,[11114]
	,[11115]
	,[7,128276]
	,[8,9224]
	,[11117]
	,[10,9252]
	,[11118]
	,[11119]
	,[11157]
	,[11013]
	,[11014]
	,[11015]
	,[11020]
	,[11021]
	,[9180]
	,[9181]
	,[9140]
	,[9141]
	,[9182]
	,[9183]
	,[8942]
	,[8943]
	,[27,9243]
	,[8944]
	,[8945]
	,[11012]
	,[8691]
	,[32,9248]
	,[33]
	,[34]
	,[35]
	,[36]
	,[37]
	,[38]
	,[39]
	,[40]
	,[41]
	,[42]
	,[43]
	,[44]
	,[45]
	,[46]
	,[47]
	,[48]
	,[49]
	,[50]
	,[51]
	,[52]
	,[53]
	,[54]
	,[55]
	,[56]
	,[57]
	,[58]
	,[59]
	,[60]
	,[61]
	,[62]
	,[63]
	,[64]
	,[65]
	,[66]
	,[67]
	,[68]
	,[69]
	,[70]
	,[71]
	,[72]
	,[73]
	,[74]
	,[75]
	,[76]
	,[77]
	,[78]
	,[79]
	,[80]
	,[81]
	,[82]
	,[83]
	,[84]
	,[85]
	,[86]
	,[87]
	,[88]
	,[89]
	,[90]
	,[91]
	,[92]
	,[93]
	,[94]
	,[95]
	,[96]
	,[97]
	,[98]
	,[99]
	,[100]
	,[101]
	,[102]
	,[103]
	,[104]
	,[105]
	,[106]
	,[107]
	,[108]
	,[109]
	,[110]
	,[111]
	,[112]
	,[113]
	,[114]
	,[115]
	,[116]
	,[117]
	,[118]
	,[119]
	,[120]
	,[121]
	,[122]
	,[123]
	,[124]
	,[125]
	,[126]
	,[127,9249]
	,[8252]
	,[8263]
	,[8264]
	,[8265]
	,[8253]
	,[215]
	,[10800]
	,[247]
	,[8724]
	,[8760]
	,[11806]
	,[8730]
	,[8543]
	,[8734]
	,[8744]
	,[10834]
	,[8743]
	,[10833]
	,[8891]
	,[172]
	,[8872]
	,[8877]
	,[8745]
	,[8746]
	,[8705]
	,[8712]
	,[8949]
	,[8918]
	,[8919]
	,[8784]
	,[10877]
	,[10879]
	,[10878]
	,[10880]
	,[8800]
	,[8801]
	,[8842]
	,[8838]
	,[8840]
	,[229]
	,[197]
	,[175]
	,[171]
	,[187]
	,[8987]
	,[8986]
	,[960]
	,[8660]
	,[8662]
	,[8663]
	,[8664]
	,[8665]
	,[217]
	,[218]
	,[220]
	,[362]
	,[7908]
	,[219]
	,[249]
	,[250]
	,[252]
	,[363]
	,[7909]
	,[251]
	,[7808]
	,[7810]
	,[7812]
	,[7816]
	,[7814]
	,[372]
	,[7809]
	,[7811]
	,[7813]
	,[7817]
	,[8523]
	,[10545]
	,[10546]
	,[10529]
	,[10530]
	,[11108]
	,[11109]
	,[11106]
	,[11104]
	,[11105]
	,[11107]
	,[11110]
	,[11111]
	,[11112]
	,[11113]
	,[11122]
	,[11120]
	,[11121]
	,[11126]
	,[11127]
	,[11128]
	,[11129]
	,[11134]
	,[11135]
	,[11152]
	,[11153]
	,[11154]
	,[11155]
	,[8470]
	,[8719]
	,[8721]
	,[8707]
	,[8704]
	,[119979]
	,[119982]
	,[11016]
	,[11017]
	,[11018]
	,[11019]
	,[11036]
	,[11034]
	,[9675]
	,[9676]
	,[7690]
	,[7692]
	,[7691]
	,[7693]
	,[10516]
	,[8614]
	,[8611]
	,[10501]
	,[10518]
	,[7718]
	,[7719]
	]
	
toCommand :: Char -> Command
toCommand char
	= commandMapping !! (toInt char)

commandMapping :: [Command]
commandMapping =: // TODO: make this a function in the above ^
	[(Control (Start (Dir East)))
	,(Control (Start (Dir West)))
	,(Control (Start (Dir North)))
	,(Control (Start (Dir South)))
	,(Stack (ShiftBase East))
	,(Stack (ShiftBase West))
	,(Stack (ShiftBase North))
	,(Operator (IO_Bell))
	,(Operator (IO_Backspace))
	,(Stack (ShiftBase South))
	,(Control (LINE))
	,(Control (Turn Anticlockwise))
	,(Control (Turn Clockwise))
	,(Control (Change East))
	,(Control (Change West))
	,(Control (Change North))
	,(Control (Change South))
	,(Control (Either Horizontal))
	,(Control (Either Vertical))
	,(Control (Loop Left North Nothing))
	,(Control (Loop Left South Nothing))
	,(Control (Goto North Nothing))
	,(Control (Goto South Nothing))
	,(Control (Loop Right North Nothing))
	,(Control (Loop Right South Nothing))
	,(Control (Mirror False Vertical))
	,(Control (Mirror False Horizontal))
	,(Control (Terminate))
	,(Control (Mirror False Identity))
	,(Control (Mirror False Inverse))
	,(Control (Start (Axis Horizontal)))
	,(Control (Start (Axis Vertical)))
	,(Control (NOOP))
	,(Operator (IO_WriteOnce))
	,(Literal (Quote))
	,(Variable (Random))
	,(Operator (IO_Environment))
	,(Operator (Binary_NN_N (mod)))//(Operator (Math_Modulus))
	,(Stack (JoinFromBase))
	,(Control (String))
	,(Control (Loop Left West Nothing))
	,(Control (Loop Left East Nothing))
	,(Operator (Math_DotProduct))
	,(Operator (Binary_NN_N (+)))//(Operator (Math_Addition))
	,(Operator (Range_FromLeftStepRight))
	,(Operator (Binary_NN_N (-)))//(Operator (Math_Subtraction))
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
	,(Operator (Binary_NN_N (isLessThan)))//(Operator (Logic_LessThan))
	,(Operator (Binary_NN_N (isEqualTo)))//(Operator (Logic_Equality))
	,(Operator (Binary_NN_N (isGreaterThan)))//(Operator (Logic_GreaterThan))
	,(Operator (IO_ReadOnce))
	,(Stack (AdjustOffset))
	,(Literal (Alphabet Uppercase))
	,(Operator (Math_ConvertToBase))
	,(Operator (Unary_N_N (acos)))//(Operator (Math_ArcCoSine))
	,(Stack (Uniques_Middle))
	,(Operator (Unary_N_N (exp)))//(Operator (Math_NaturalExponent))
	,(Operator (Set_Permutations))
	,(Operator (Binary_NN_N (gcd)))//(Operator (Math_GreatestCommonDivisor))
	,(Operator (Set_Maximum))
	,(Operator (Math_ImaginaryPart))
	,(Operator (Math_RealPart))
	,(Operator (Set_AntiFilter))
	,(Operator (Math_Logarithm))
	,(Operator (Set_Combinations))
	,(Operator (Chars_JoinWithNewlines))
	,(Operator (Unary_S_N (isSorted)))//(Operator (Logic_IsOrdered))
	,(Operator (Unary_N_N (isPrime)))//(Operator (Logic_IsPrime))
	,(Variable (Quine))
	,(Operator (Unary_N_N (isFiniteReal)))//(Operator (Logic_IsReal))
	,(Operator (Unary_N_N (asin)))//(Operator (Math_ArcSine))
	,(Operator (Unary_N_N (atan)))//(Operator (Math_ArcTangent))
	,(Operator (Unary_M_M (stackReverse Middle)))//(Stack (Reverse_Middle))
	,(Operator (Math_Average))
	,(Operator (Unary_M_M (stackDelete Middle)))//(Stack (Delete_Middle))
	,(Operator (Chars_ToUppercase))
	,(Operator (Unary_N_N (isUppercase)))//(Operator (Logic_IsUppercase))
	,(Operator (Unary_N_N (isFiniteNumber)))//(Operator (Logic_IsFinite))
	,(Control (Goto West Nothing))
	,(Control (Mirror True Inverse))
	,(Control (Goto East Nothing))
	,(Operator (Binary_NN_N (^)))//(Operator (Math_Exponent))
	,(Operator (Unary_N_N (numFloor)))//(Operator (Math_Floor))
	,(Control (Restart))
	,(Literal (Alphabet Lowercase))
	,(Operator (Math_ConvertFromBase))
	,(Operator (Unary_N_N (cos)))//(Operator (Math_CoSine))
	,(Stack (Duplicates_Middle))
	,(Operator (Unary_N_N (ln)))//(Operator (Math_NaturalLogarithm))
	,(Operator (Binary_NN_N (numPermute)))//(Operator (Math_Permutations))
	,(Operator (Binary_NN_N (lcm)))//(Operator (Math_LeastCommonMultiple))
	,(Operator (Set_Minimum))
	,(Operator (Unary_N_N (imagUnit)))//(Operator (Math_ImaginaryUnit))
	,(Operator (Math_ComplexSplit))
	,(Operator (Set_Filter))
	,(Operator (Unary_N_N (log10)))//(Operator (Math_Base10Logarithm))
	,(Operator (Binary_NN_N (numCombin)))//(Operator (Math_Combinations))
	,(Operator (Chars_SplitOnNewlines))
	,(Operator (Unary_S_S (sort)))//(Operator (Set_MakeOrdered))
	,(Operator (Unary_N_S (primeFactors)))//(Operator (Math_PrimeFactors))
	,(Variable (History))
	,(Operator (Unary_N_N (numRound)))//(Operator (Math_Round))
	,(Operator (Unary_N_N (sin)))//(Operator (Math_Sine))
	,(Operator (Unary_N_N (tan)))//(Operator (Math_Tangent))
	,(Operator (Unary_M_M (stackRotate Middle)))//(Stack (Rotate_Middle))
	,(Operator (Unary_N_N (abs)))//(Operator (Math_Abs))
	,(Operator (Unary_M_M (stackDrop Middle)))//(Stack (Drop_Middle))
	,(Operator (Chars_ToLowercase))
	,(Operator (Unary_N_N (isLowercase)))//(Operator (Logic_IsLowercase))
	,(Operator (Unary_N_N (conjugate)))//(Operator (Math_Conjugate))
	,(Control (Loop Right West Nothing))
	,(Control (Mirror True Vertical))
	,(Control (Loop Right East Nothing))
	,(Operator (Unary_N_N (~)))//(Operator (Math_Negation))
	,(Operator (IO_ClearConsole))
	,(Operator (IO_WriteAll))
	,(Operator (IO_ReadAll))
	,(Operator (IO_ReadWrite))
	,(Operator (IO_WriteRead))
	,(Operator (IO_Interrobang))
	,(Operator (Binary_NN_N (*)))//(Operator (Math_Multiplication))
	,(Operator (Vector_Multiplication))
	,(Operator (Binary_NN_N (/)))//(Operator (Math_Division))
	,(Operator (Vector_Addition))
	,(Operator (Vector_Subtraction))
	,(Operator (Vector_Negation))
	,(Operator (Unary_N_N (sqrt)))//(Operator (Math_SquareRoot))
	,(Operator (Math_Reciprocal))
	,(Operator (Unary_N_N (isInfinite)))//(Operator (Logic_IsInfinite))
	,(Operator (Binary_NN_N (bitOR)))//(Operator (Bitwise_Or))
	,(Operator (Vector_Or))
	,(Operator (Binary_NN_N (bitAND)))//(Operator (Bitwise_And))
	,(Operator (Vector_And))
	,(Operator (Binary_NN_N (bitXOR)))//(Operator (Bitwise_Xor))
	,(Operator (Unary_N_N (bitNOT)))//(Operator (Bitwise_Not))
	,(Operator (Unary_N_N (logicEquiv)))//(Operator (Logic_Coalesce))
	,(Operator (Unary_N_N (logicNegate)))//(Operator (Logic_Negation))
	,(Operator (Set_Intersection))
	,(Operator (Set_Union))
	,(Operator (Set_Exclusion))
	,(Operator (Binary_NS_N (isElementOf)))//(Operator (Logic_ElementOf))
	,(Operator (Vector_ElementOf))
	,(Operator (Vector_LessThan))
	,(Operator (Vector_GreaterThan))
	,(Operator (Vector_Equality))
	,(Operator (Binary_NN_N (isLessOrEqual)))//(Operator (Logic_LessOrEqual))
	,(Operator (Vector_LessOrEqual))
	,(Operator (Binary_NN_N (isGreaterOrEqual)))//(Operator (Logic_GreaterOrEqual))
	,(Operator (Vector_GreaterOrEqual))
	,(Operator (Binary_NN_N (isNotEqual)))//(Operator (Logic_Inequality))
	,(Operator (Binary_SS_N (isIdentical)))//(Operator (Logic_SetEquality))
	,(Operator (Binary_SS_N (isProperSubsetOf)))//(Operator (Logic_SubsetNotEqual))
	,(Operator (Binary_SS_N (isImproperSubsetOf)))//(Operator (Logic_SubsetOrEqual))
	,(Operator (Binary_SS_N (isNotSubsetOf)))//(Operator (Logic_NotSubsetNorEqual))
	,(Operator (Unary_N_N (toDegrees)))//(Operator (Math_RadiansToDegrees))
	,(Operator (Unary_N_N (toRadians)))//(Operator (Math_DegreesToRadians))
	,(Operator (Unary_N_N (numCeiling)))//(Operator (Math_Ceiling))
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
	,(Operator (Unary_M_M (stackReverse Left)))//(Stack (Reverse_Left))
	,(Operator (Unary_M_M (stackReverse Right)))//(Stack (Reverse_Right))
	,(Operator (Unary_M_M (stackReverse Both)))//(Stack (Reverse_Both))
	,(Operator (Unary_M_M (stackReverse Primary)))//(Stack (Reverse_Primary))
	,(Operator (Unary_M_M (stackReverse Base)))//(Stack (Reverse_Base))
	,(Operator (Unary_M_M (stackReverse All)))//(Stack (Reverse_All))
	,(Operator (Unary_M_M (stackRotate Left)))//(Stack (Rotate_Left))
	,(Operator (Unary_M_M (stackRotate Right)))//(Stack (Rotate_Right))
	,(Operator (Unary_M_M (stackRotate Both)))//(Stack (Rotate_Both))
	,(Operator (Unary_M_M (stackRotate Primary)))//(Stack (Rotate_Primary))
	,(Operator (Unary_M_M (stackRotate Base)))//(Stack (Rotate_Base))
	,(Operator (Unary_M_M (stackRotate All)))//(Stack (Rotate_All))
	,(Operator (Unary_M_M (stackDelete Left)))//(Stack (Delete_Left))
	,(Operator (Unary_M_M (stackDelete Right)))//(Stack (Delete_Right))
	,(Operator (Unary_M_M (stackDelete Both)))//(Stack (Delete_Both))
	,(Operator (Unary_M_M (stackDelete Base)))//(Stack (Delete_Base))
	,(Operator (Unary_M_M (stackDelete Main)))//(Stack (Delete_Main))
	,(Operator (Unary_M_M (stackDelete All)))//(Stack (Delete_All))
	,(Operator (Unary_M_M (stackDrop Left)))//(Stack (Drop_Left))
	,(Operator (Unary_M_M (stackDrop Right)))//(Stack (Drop_Right))
	,(Operator (Unary_M_M (stackDrop Both)))//(Stack (Drop_Both))
	,(Operator (Unary_M_M (stackDrop Base)))//(Stack (Drop_Base))
	,(Stack (Unjoin))
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
	,(Operator (Set_Length))
	,(Operator (Unary_S_N (prod)))//(Operator (Math_Product))
	,(Operator (Unary_S_N (sum)))//(Operator (Math_Sum))
	,(Operator (Unary_S_N (areAnyTrue)))//(Operator (Logic_Any))
	,(Operator (Unary_S_N (areAllTrue)))//(Operator (Logic_All))
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
	,(Operator (Binary_NN_N (max)))//(Operator (Math_Maximum))
	,(Operator (Binary_NN_N (min)))//(Operator (Math_Minimum))
	]
	