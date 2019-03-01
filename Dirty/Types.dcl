definition module Dirty.Types

:: Point = {
	x :: Int,
	y :: Int
	}
	
:: Direction
	= North
	| South
	| East
	| West
	
:: Region
	= NormalCode
	| StringLiteral
	| FractionLiteral
	| DecimalLiteral
	| IntegerLiteral