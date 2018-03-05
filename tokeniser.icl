implementation module tokeniser

import types, arithmetic, builtins, unicode, StdEnv, StdLib, environment

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
	,[249]
	,[250]
	,[252]
	,[363]
	,[7909]
	,[161]
	,[191]
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
	,[7885]
	,[8470]
	,[574]
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
	,[10872]
	,[8709]
	,[7692]
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
	,(Operator (Unary_M_M (moveCursorBackwards)))//(Stack (ShiftBase East))
	,(Operator (Unary_M_M (moveCursorForwards)))//(Stack (ShiftBase West))
	,(Operator (Unary_M_M (shiftCursorUpwards)))//(Stack (ShiftBase North))
	,(Operator (IO_Bell))
	,(Operator (IO_Backspace))
	,(Operator (Unary_M_M (shiftCursorDownwards)))//(Stack (ShiftBase South))
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
	,(Environment (getEnvVariable))//(Operator (IO_Environment))
	,(Operator (Binary_NN_N False (mod)))//(Operator (Math_Modulus))
	,(Operator (Unary_M_M (stackJoin)))//(Stack (JoinFromBase))
	,(Control (String))
	,(Control (Loop Left West Nothing))
	,(Control (Loop Left East Nothing))
	,(Operator (Unary_M_M (matrixProduct)))//(Operator (Math_DotProduct))
	,(Operator (Binary_NN_N True (+)))//(Operator (Math_Addition))
	,(Operator (Binary_NN_S False (fromLeftStepRight)))//(Operator (Range_FromLeftStepRight))
	,(Operator (Binary_NN_N False (-)))//(Operator (Math_Subtraction))
	,(Operator (Unary_N_S (fromZeroToMiddle)))//(Operator (Range_FromMiddleToZero))
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
	,(Operator (Unary_N_S (fromMiddleAvoidZero)))//(Operator (Range_FromMiddleAvoidZero))
	,(Operator (Binary_NN_S False (fromLeftTimesRight)))//(Operator (Range_FromLeftTimesRight))
	,(Operator (Binary_NN_N False (isLessThan)))//(Operator (Logic_LessThan))
	,(Operator (Binary_NN_N True (isEqualTo)))//(Operator (Logic_Equality))
	,(Operator (Binary_NN_N False (isGreaterThan)))//(Operator (Logic_GreaterThan))
	,(Operator (IO_ReadOnce))
	,(Stack (AdjustOffset))
	,(Literal (Alphabet Uppercase))
	,(Operator (Math_ConvertToBase))
	,(Operator (Unary_N_N (acos)))//(Operator (Math_ArcCoSine))
	,(Operator (Unary_S_S (removeDup)))//(Stack (Uniques_Middle))
	,(Operator (Unary_N_N (exp)))//(Operator (Math_NaturalExponent))
	,(Operator (Set_Permutations))
	,(Operator (Binary_NN_N True (gcd)))//(Operator (Math_GreatestCommonDivisor))
	,(Operator (Unary_S_N (setMaximum)))//(Operator (Set_Maximum))
	,(Operator (Unary_N_N (justImag)))//(Operator (Math_ImaginaryPart))
	,(Operator (Unary_N_N (justReal)))//(Operator (Math_RealPart))
	,(Operator (Binary_SS_S False (antiFilter)))//(Operator (Set_AntiFilter))
	,(Operator (Binary_NN_N False (logarithm)))//(Operator (Math_Logarithm))
	,(Operator (Set_Combinations))
	,(Operator (Unary_M_M (joinWithNewlines)))//(Operator (Chars_JoinWithNewlines))
	,(Operator (Unary_S_N (isSorted)))//(Operator (Logic_IsOrdered))
	,(Operator (Unary_N_N (isPrime)))//(Operator (Logic_IsPrime))
	,(Variable (Quine))
	,(Operator (Unary_N_N (isFiniteReal)))//(Operator (Logic_IsReal))
	,(Operator (Unary_N_N (asin)))//(Operator (Math_ArcSine))
	,(Operator (Unary_N_N (atan)))//(Operator (Math_ArcTangent))
	,(Operator (Unary_M_M (stackReverse Middle)))//(Stack (Reverse_Middle))
	,(Operator (Unary_S_N (avg)))//(Operator (Math_Average))
	,(Operator (Unary_M_M (stackDelete Middle)))//(Stack (Delete_Middle))
	,(Operator (Unary_N_N (toUppercase)))//(Operator (Chars_ToUppercase))
	,(Operator (Unary_N_N (isUppercase)))//(Operator (Logic_IsUppercase))
	,(Operator (Unary_N_N (isFiniteNumber)))//(Operator (Logic_IsFinite))
	,(Control (Goto West Nothing))
	,(Control (Mirror True Inverse))
	,(Control (Goto East Nothing))
	,(Operator (Binary_NN_N False (^)))//(Operator (Math_Exponent))
	,(Operator (Unary_N_N (numFloor)))//(Operator (Math_Floor))
	,(Control (Restart))
	,(Literal (Alphabet Lowercase))
	,(Operator (Math_ConvertFromBase))
	,(Operator (Unary_N_N (cos)))//(Operator (Math_CoSine))
	,(Operator (Unary_S_S (dupesMiddle)))//(Stack (Duplicates_Middle))
	,(Operator (Unary_N_N (ln)))//(Operator (Math_NaturalLogarithm))
	,(Operator (Binary_NN_N False (numPermute)))//(Operator (Math_Permutations))
	,(Operator (Binary_NN_N True (lcm)))//(Operator (Math_LeastCommonMultiple))
	,(Operator (Unary_S_N (setMinimum)))//(Operator (Set_Minimum))
	,(Operator (Unary_N_N (imagUnit)))//(Operator (Math_ImaginaryUnit))
	,(Operator (Unary_M_M (complexSplit)))//(Operator (Math_ComplexSplit))
	,(Operator (Binary_SS_S False (setFilter)))//(Operator (Set_Filter))
	,(Operator (Unary_N_N (log10)))//(Operator (Math_Base10Logarithm))
	,(Operator (Binary_NN_N False (numCombin)))//(Operator (Math_Combinations))
	,(Operator (Unary_S_T (splitOnNewlines)))//(Operator (Chars_SplitOnNewlines))
	,(Operator (Unary_S_S (sort)))//(Operator (Set_MakeOrdered))
	,(Operator (Unary_N_S (primeFactors)))//(Operator (Math_PrimeFactors))
	,(Variable (History))
	,(Operator (Unary_N_N (numRound)))//(Operator (Math_Round))
	,(Operator (Unary_N_N (sin)))//(Operator (Math_Sine))
	,(Operator (Unary_N_N (tan)))//(Operator (Math_Tangent))
	,(Operator (Unary_M_M (stackRotate Middle)))//(Stack (Rotate_Middle))
	,(Operator (Unary_N_N (abs)))//(Operator (Math_Abs))
	,(Operator (Unary_M_M (stackDrop Middle)))//(Stack (Drop_Middle))
	,(Operator (Unary_N_N (toLowercase)))//(Operator (Chars_ToLowercase))
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
	,(Operator (Binary_NN_N True (*)))//(Operator (Math_Multiplication))
	,(Operator (Binary_SS_S True (vectorTimes)))//(Operator (Vector_Multiplication))
	,(Operator (Binary_NN_N False (/)))//(Operator (Math_Division))
	,(Operator (Binary_SS_S True (vectorPlus)))//(Operator (Vector_Addition))
	,(Operator (Unary_S_S (vectorNegate)))//(Operator (Vector_Negation))
	,(Operator (Unary_N_N (sqrt)))//(Operator (Math_SquareRoot))
	,(Operator (Unary_N_N (reciprocal)))//(Operator (Math_Reciprocal))
	,(Operator (Unary_N_N (isInfinite)))//(Operator (Logic_IsInfinite))
	,(Operator (Binary_NN_N True (bitOR)))//(Operator (Bitwise_Or))
	,(Operator (Binary_SS_S True (vectorOR)))//(Operator (Vector_Or))
	,(Operator (Binary_NN_N True (bitAND)))//(Operator (Bitwise_And))
	,(Operator (Binary_SS_S True (vectorAND)))//(Operator (Vector_And))
	,(Operator (Binary_NN_N True (bitXOR)))//(Operator (Bitwise_Xor))
	,(Operator (Unary_N_N (bitNOT)))//(Operator (Bitwise_Not))
	,(Operator (Unary_N_N (logicEquiv)))//(Operator (Logic_Coalesce))
	,(Operator (Unary_N_N (logicNegate)))//(Operator (Logic_Negation))
	,(Operator (Set_Intersection))
	,(Operator (Set_Union))
	,(Operator (Set_Exclusion))
	,(Operator (Binary_NS_N (isElementOf)))//(Operator (Logic_ElementOf))
	,(Operator (Binary_SS_S False (vectorElementOf)))//(Operator (Vector_ElementOf))
	,(Operator (Binary_SS_S False (vectorLessThan)))//(Operator (Vector_LessThan))
	,(Operator (Binary_SS_S False (vectorGreaterThan)))//(Operator (Vector_GreaterThan))
	,(Operator (Binary_SS_S True (vectorIsEqual)))//(Operator (Vector_Equality))
	,(Operator (Binary_NN_N False (isLessOrEqual)))//(Operator (Logic_LessOrEqual))
	,(Operator (Binary_SS_S False (vectorLessOrEqual)))//(Operator (Vector_LessOrEqual))
	,(Operator (Binary_NN_N False (isGreaterOrEqual)))//(Operator (Logic_GreaterOrEqual))
	,(Operator (Binary_SS_S False (vectorGreaterOrEqual)))//(Operator (Vector_GreaterOrEqual))
	,(Operator (Binary_NN_N True (isNotEqual)))//(Operator (Logic_Inequality))
	,(Operator (Binary_SS_N True (isIdentical)))//(Operator (Logic_SetEquality))
	,(Operator (Binary_SS_N False (isProperSubsetOf)))//(Operator (Logic_SubsetNotEqual))
	,(Operator (Binary_SS_N False (isImproperSubsetOf)))//(Operator (Logic_SubsetOrEqual))
	,(Operator (Binary_SS_N False (isNotSubsetOf)))//(Operator (Logic_NotSubsetNorEqual))
	,(Operator (Unary_N_N (toDegrees)))//(Operator (Math_RadiansToDegrees))
	,(Operator (Unary_N_N (toRadians)))//(Operator (Math_DegreesToRadians))
	,(Operator (Unary_N_N (numCeiling)))//(Operator (Math_Ceiling))
	,(Operator (Bitwise_LeftShift))
	,(Operator (Bitwise_RightShift))
	,(Environment (sleepFor))//(Operator (IO_Sleep))
	,(Operator (IO_Timestamp))
	,(Literal (Pi))
	,(Operator (Unary_M_M (swapLeftRight)))//(Stack (SwapLeftRight))
	,(Operator (Unary_M_M (moveAll NorthWest)))//(Stack (MoveAll NorthWest))
	,(Operator (Unary_M_M (moveAll NorthEast)))//(Stack (MoveAll NorthEast))
	,(Operator (Unary_M_M (moveAll SouthEast)))//(Stack (MoveAll SouthEast))
	,(Operator (Unary_M_M (moveAll SouthWest)))//(Stack (MoveAll SouthWest))
	,(Operator (Unary_M_M (stackReverse Left)))//(Stack (Reverse_Left))
	,(Operator (Unary_M_M (stackReverse Right)))//(Stack (Reverse_Right))
	,(Operator (Unary_M_M (stackReverse Both)))//(Stack (Reverse_Both))
	,(Operator (Unary_M_M (stackReverse Primary)))//(Stack (Reverse_Primary))
	,(Operator (Unary_M_M (stackReverse Base)))//(Stack (Reverse_Base))
	,(Operator (Unary_M_M (stackRotate Left)))//(Stack (Rotate_Left))
	,(Operator (Unary_M_M (stackRotate Right)))//(Stack (Rotate_Right))
	,(Operator (Unary_M_M (stackRotate Both)))//(Stack (Rotate_Both))
	,(Operator (Unary_M_M (stackRotate Primary)))//(Stack (Rotate_Primary))
	,(Operator (Unary_M_M (stackRotate Base)))//(Stack (Rotate_Base))
	,(Operator (Unary_M_M (remember)))
	,(Operator (Unary_M_M (recall)))
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
	,(Operator (Unary_M_M (stackUnjoin)))//(Stack (Unjoin))
	,(Operator (Unary_M_M (unpackRightLeft)))//(Stack (Unpack_RightLeft))
	,(Operator (Unary_M_M (unpackLeftRight)))//(Stack (Unpack_LeftRight))
	,(Operator (Unary_M_M (swapTop Inverse)))//(Stack (SwapTop Inverse))
	,(Operator (Unary_M_M (swapTop Identity)))//(Stack (SwapTop Identity))
	,(Operator (Unary_M_M (swapTop Horizontal)))//(Stack (SwapTop Horizontal))
	,(Operator (Unary_M_M (swapTop Vertical)))//(Stack (SwapTop Vertical))
	,(Operator (Unary_M_M (moveTop East)))
	,(Operator (Unary_M_M (moveTop West)))
	,(Operator (Unary_M_M (moveTop South)))
	,(Operator (Unary_M_M (moveTop NorthWest)))
	,(Operator (Unary_M_M (moveTop NorthEast)))
	,(Operator (Unary_M_M (moveTop SouthEast)))
	,(Operator (Unary_M_M (moveTop SouthWest)))
	,(Operator (Unary_M_M (copyTop East)))
	,(Operator (Unary_M_M (copyTop West)))
	,(Operator (Unary_M_M (copyTop North)))
	,(Operator (Unary_M_M (copyTop NorthWest)))
	,(Operator (Unary_M_M (copyTop NorthEast)))
	,(Operator (Unary_M_M (copyTop SouthEast)))
	,(Operator (Unary_M_M (copyTop SouthWest)))
	,(Operator (Unary_M_M (copyBoth Horizontal)))//(Stack (CopyBoth Horizontal))
	,(Operator (Unary_M_M (copyBoth Vertical)))//(Stack (CopyBoth Vertical))
	,(Operator (Unary_M_M (cycleStacks Clockwise)))//(Stack (CycleFull Clockwise))
	,(Operator (Unary_M_M (cycleStacks Anticlockwise)))//(Stack (CycleFull Anticlockwise))
	,(Operator (Unary_M_M (cycleTops Clockwise)))//(Stack (CycleTops Clockwise))
	,(Operator (Unary_M_M (cycleTops Anticlockwise)))//(Stack (CycleTops Anticlockwise))
	,(Operator (Unary_M_M (sortBaseline)))
	,(Operator (Unary_M_M (stacksFromCursor)))//(Operator (Set_Length))
	,(Operator (Unary_M_M (transposeFromCursor)))
	,(Operator (Unary_S_N (numProduct)))//(Operator (Math_Product))
	,(Operator (Unary_S_N (numSum)))//(Operator (Math_Sum))
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
	,(Operator (Unary_S_T (groupMiddle)))//(Stack (Group_Base))
	,(Literal (EmptySet))
	,(Operator (Unary_M_M (removeDupBase)))//(Stack (Uniques_Base))
	,(Operator (Unary_M_M (dupesBase)))//(Stack (Duplicates_Base))
	,(Operator (Unary_M_M (replicateBase)))//(Stack (Replicate_Base))
	,(Operator (Unary_M_M (replicateTop)))//(Stack (Replicate_TopOfMiddle))
	,(Operator (Unary_M_M (replicateMiddle)))//(Stack (Replicate_AllOfMiddle))
	,(Operator (Unary_M_M (repeatTopMiddle)))//(Stack (Repeat_TopOfMiddle))
	,(Operator (Unary_M_M (repeatFullMiddle)))//(Stack (Repeat_AllOfMiddle))
	,(Operator (Binary_NN_N True (max)))//(Operator (Math_Maximum))
	,(Operator (Binary_NN_N True (min)))//(Operator (Math_Minimum))
	]
	