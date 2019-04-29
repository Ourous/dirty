implementation module Dirty.Backend.Stack

import Dirty.Backend.Number, Dirty.Backend.Value
import Data.Maybe, Data.List
import StdEnv, StdOverloadedList, StdDebug

:: Stack
	= Head !Value Stack
	| Loop ![!Value!] Stack
	| Lazy ![!Value] Attrs Stack
	| Null

:: Attrs = {
	hasStk :: Maybe Bool,
	hasNum :: Maybe Bool,
	hasRe :: Maybe Bool,
	hasIm :: Maybe Bool,
	hasCx :: Maybe Bool,
	hasNaN :: Maybe Bool,
	minVal :: Maybe Number,
	maxVal :: Maybe Number,
	sorted :: Maybe Bool
	}
	
instance zero Stack
where zero = Null

instance +++ Stack
where
	(+++) Null Null = Null
	(+++) Null rhs = rhs
	(+++) lhs Null = lhs
	//(+++) lhs=:(Lazy _ _) _ = lhs // probably okay to ignore attributes here
	(+++) (Head val lhs) rhs = (Head val (lhs +++ rhs))

instance toBool Stack
where
	toBool Null = False
	toBool stack = maybe False toBool (peek stack)
	
instance < Stack
where
	(<) _ Null = False
	(<) Null _ = True
	(<) (Head lh lt) (Head rh rt)
		= lh < rh && lt < rt
	
instance == Stack
where
	(==) Null Null = True
	(==) _ Null = False
	(==) Null _ = False
	(==) (Head lh lt) (Head rh rt)
		= lh == rh && lt == rt // TODO revise for lazy stacks

instance toString Stack
where
	toString Null = ""

instance fromString Stack
where
	fromString str = toStack [c \\ c <-: str]

instance zero Attrs
where zero = {
		hasStk=Nothing,
		hasNum=Nothing,
		hasRe=Nothing,
		hasIm=Nothing,
		hasCx=Nothing,
		hasNaN=Nothing,
		minVal=Nothing,
		maxVal=Nothing,
		sorted=Nothing
		}
		
class toStack a :: a -> Stack
instance toStack [Char]
where
	toStack [] = Null
	toStack [h:t] = (Head (toValue h) (toStack t))
	
instance toStack Value
where
	toStack v = (Head v Null)
	
instance toStack Number
where
	toStack v = (Head (toValue v) Null)

instance repr Stack where
	repr _ Null = []
	repr inf (Head h t) = repr inf h ++ case t of Null = []; _ = [',':repr inf t]
	repr True (Loop l _) = (intercalate [','] o cycle o Map (repr True)) l
	repr False (Loop l t) = ['('] ++ intercalate [','] (Map (repr False) l) ++ [')':case t of Null = []; _ = [',':repr False t]]
	repr True (Lazy l _ _) = (intercalate [','] o Map (repr True)) l
	repr False (Lazy l _ t) = ['...':case t of Null = []; _ = [',':repr False t]]//TODO: find better solution
	
instance disp Stack where
	disp Null = []
	disp (Head h t) = disp h ++ disp t
	disp (Loop l _) = (flatten o cycle o Map disp) l
	disp (Lazy l _ _) = (flatten o Map disp) l
	
instance eval Stack where
	eval _ = abort "Eval for stacks not implemented yet"

/*	
instance toStack [!a] | toValue a
where
	toStack [!a]
instance toStack [!a!] | toValue a
		*/
fromValue :: Value -> Stack
fromValue val = (Head val Null)
	
push :: Value Stack -> Stack
push val stack = (Head val stack)
		
pop :: Stack -> (Maybe Value, Stack)
pop Null = (Nothing, Null)//abort "Fatal: cannot pop null"
pop (Head val stack) = (Just val, stack)

peek :: Stack -> Maybe Value
peek Null = Nothing//abort "Fatal: cannot peek null"
peek (Head val _) = Just val

appH :: (Value -> Value) Stack -> Stack
appH _ Null = Null
appH fn (Head val stack) = (Head (fn val) stack)


// misc builtins
S_sort :: Stack -> Stack
S_sort Null = Null
S_sort stack=:(Head _ Null) = stack
S_sort h=:(Head a t=:(Head b c))
	| a > b
		= (Head b (S_sort (Head a c)))
	| otherwise
		= (Head a (S_sort t))
	
//sort stack=:(More _ Null) = stack
//sort (Loop loop Null) = Foldr (\a b = (More a b)) Null loop

S_isEmpty :: Stack -> Bool
S_isEmpty Null = True
S_isEmpty _ = False

S_isSorted :: Stack -> Bool
S_isSorted Null = True
S_isSorted (Head h t) = maybe True ((<) h) (peek t) && S_isSorted t
S_isSorted (Loop [!v!] t) = maybe True ((<) v) (peek t) && S_isSorted t
S_isSorted (Loop _ _) = False
S_isSorted (Lazy _ _ _) = abort "Sequence tracking not implemented"

S_reduce :: (Value -> Value -> Value) Value Stack -> Value
S_reduce _ st Null = st
S_reduce f st (Head h t) = S_reduce f (f st h) t
//reduce f st t=:(More h _) = S_reduce f (f st h) t
S_reduce f st t=:(Loop h _) = S_reduce f (Foldl f st h) t
S_reduce f st (Lazy h _ t) = S_reduce f (Foldl f st h) (trace_n "Error: access past end of infinite list" t)

S_map :: (Value -> Value) Stack -> Stack
S_map _ Null = Null
S_map f (Head h t) = (Head (f h) (S_map f t))
S_map f (Loop h t) = (Loop (Map f h) (S_map f t))
S_map f (Lazy h _ t) = (Lazy (Map f h) zero (trace_n "Error: access past end of infinite list" t))

S_reverse :: Stack -> Stack
S_reverse Null = Null
S_reverse (Head h t) = (S_reverse t) +++ (Head h Null)

S_indexOf :: Stack Value -> Value
S_indexOf _ _ = undef

S_valueAt :: Stack Value -> Value
S_valueAt _ _ = undef

S_length :: Stack -> Int
S_length Null = 0
S_length (Head _ stack) = 1 + S_length stack

S_takeWhile :: Stack Stack -> Stack // cond, list
S_takeWhile Null _ = Null
S_takeWhile _ Null = Null
S_takeWhile (Head lh lt) (Head rh rt)
	| toBool lh
		= (Head rh (S_takeWhile lt rt))
	| otherwise
		= Null

S_dropWhile :: Stack Stack -> Stack
S_dropWhile Null rhs = rhs
S_dropWhile _ Null = Null
S_dropWhile (Head lh lt) rhs=:(Head _ rt)
	| toBool lh
		= S_dropWhile lt rt
	| otherwise
		= rhs

S_repeat :: Int Value -> Stack
S_repeat 0 _ = Null
S_repeat n v
	| n < 0
		= trace_n "Warning: behaviour for repeating negative ammounts is TBD, yielding Null" Null
	| otherwise
		= (Head v (S_repeat (dec n) v))

S_zipWith :: (Value Value -> Value) Stack Stack -> Stack
S_zipWith _ Null _ = Null
S_zipWith _ _ Null = Null
S_zipWith fn (Head lh lt) (Head rh rt)
	= (Head (fn lh rh) (S_zipWith fn lt rt))

S_removeDup :: Stack -> Stack
S_removeDup Null = Null
S_removeDup stack = removeDup` stack []
where
	removeDup Null _ = Null
	removeDup` (Head h t) seen // TODO use ordered list to make this faster
		| any ((==)h) seen
			= removeDup` t seen
		| otherwise
			= (Head h (removeDup` t [h:seen]))		

S_countDup :: Stack -> Int
S_countDup Null = 0
S_countDup stack = countDup` stack []
where
	countDup` Null _ = 0
	countDup` (Head h t) seen
		| any ((==)h) seen
			= inc (countDup` t seen)
		| otherwise
			= countDup` t [h:seen]
S_hasDup :: Stack -> Bool
S_hasDup stack = hasDup` stack []
where
	hasDup` Null _ = False
	hasDup` (Head h t) seen
		= any ((==)h) seen || hasDup` t [h:seen]


