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
	| Interface
	
:: Control
	= Control_Terminate
	| Control_Start CardinalDirection
	| Control_Change Bool CardinalDirection
	| Control_Bounce Bool DiagonalDirection
	| Control_Random Bool CardinalAxis
	| 
	
:: CardinalAxis
	= Vertical
	| Horizontal
	
:: DiagonalAxis
	= Inverse
	| Identity
	
:: CardinalDirection
	= North
	| South
	| East
	| West

:: CompassDirection
	= CardinalDirection
	| DiagonalDirection

:: DiagonalDirection
	| NorthWest
	| NorthEast
	| SouthWest
	| SouthEast
	
