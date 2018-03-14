implementation module utilities

import types, StdEnv, StdLib, stacks

instance == Direction where
	(==) East East = True
	(==) West West = True
	(==) North North = True
	(==) South South = True
	(==) NorthEast NorthEast = True
	(==) NorthWest NorthWest = True
	(==) SouthWest SouthWest = True
	(==) SouthEast SouthEast = True
	(==) _ _ = False
	
instance == StackID where
	(==) Left Left = True
	(==) Right Right = True
	(==) Middle Middle = True
	(==) Main Main = True
	(==) Base Base = True
	(==) Both Both = True
	(==) Primary Primary = True
	(==) Every Every = True
	(==) _ _ = False

rotateList :: a u:[b] -> v:[b] | Enum a, [u <= v]
rotateList _ [] = []
rotateList n list
	| n > zero
		= rotateList (dec n) ((tl list) ++ [hd list])
	| n < zero
		= rotateList (inc n) [last list:init list]
	| otherwise
		= list
		
mergeDelims :: !Memory -> Memory
mergeDelims memory=:{main={bounded}}
	# (cursor, reversed) = collapseDelims memory.cursor [] (toList memory.main)
	# (delims, cursor, main) = expandDelims 0 cursor [] reversed
	= {memory&cursor=cursor,delims=delims,main=fromList main bounded}
where
	collapseDelims cursor head [Delim hi: tail=:[Delim _: _]]
		# (delimsGroup, others) = span (IS_DELIM) tail
		# (Delim lo) = last delimsGroup
		| (hi == cursor || any (\(Delim val) -> val == cursor) delimsGroup) || (hi >= cursor && cursor >= lo)
			= collapseDelims lo [Delim lo:head] others
		| otherwise
			= collapseDelims cursor [Delim lo:head] others
	//where
	//	collectDelims [Delim _: tail=:[Delim _: _]] = collectDelims tail
	//	collectDelims [Delim val: tail] = (val, tail)
		
	collapseDelims cursor head [h:tail]
		= collapseDelims cursor [h:head] tail
	collapseDelims cursor head []
		= (cursor, head)

	expandDelims delims cursor head [Delim val:tail]
		= expandDelims (inc delims) (if(cursor==val) delims cursor) [Delim delims:head] tail
	expandDelims delims cursor head [h:tail]
		= expandDelims delims cursor [h:head] tail
	expandDelims delims cursor head []
		= (delims, cursor, head)