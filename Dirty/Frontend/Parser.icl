implementation module Dirty.Frontend.Parser

//import Dirty.Backend.Value, Dirty.Backend.Stack, Dirty.Backend.Number
import Dirty.Backend
import Dirty.Frontend.Arguments
import Dirty.Runtime.Instruction
from Dirty.Types import ::Point(..)
import Data.Matrix, Data.List
//import Text
import StdEnv
import Regex

START_CHARS :== ['\016\017\020\021\022']
NUMBER_CHARS :== ['0123456789.:i-E']
NUMBER_REGEX :== regex "^-?\\d*\\.?\\d*(?:E-?\d+)?(?::\\d*\\.?\\d*(?:E-?\d+)?)?(?:i(?:-?\\d*\\.?\\d*(?:E-?\d+)?(?::\\d*\\.?\\d*(?:E-?\d+)?)?)?)?"


parseFile :: (Matrix Char) -> (Matrix Instruction, Vector Point)
parseFile m = ({{parse c {x=x,y=y} \\ c <-: r & x <- [0..]} \\ r <-: m & y <- [0..]}, start)
where
	start = {{x=x,y=y} \\ r <-: m & y <- [0..], c <-: r & x <- [0..] | isMember c START_CHARS}
	
	// "^(-?\\d*\\.?\\d*(?:(?::\\d*\\.*\\d*)?(?:i-?\\d*\\.?\\d*(?::\\d*\\.*\\d*)?)?)?)" should match valid numeric literals
	// will have to be manually constructed from smaller parse sections
	
	from_position_north pos = reverse [m.[y,pos.x] \\ y <- [pos.y..rows m-1] ++ [0..pos.y-1]]
	from_position_east pos = [m.[pos.y,x] \\ x <- [pos.x..cols m-1] ++ [0..pos.x-1]]
	from_position_south pos = [m.[y,pos.x] \\ y <- [pos.y..rows m-1] ++ [0..pos.y-1]]
	from_position_west pos = reverse [m.[pos.y,x] \\ x <- [pos.x..cols m-1] ++ [0..pos.x-1]]
	
	parse_literal_number pos // . associates first, then : and i in the order they appear (i multiplies whatever is in front of it by `i`, adjacency behind adds, : behind divides)
		= I_LITERAL_REGION (toValue n_num, n_end) (toValue e_num, e_end) (toValue s_num, s_end) (toValue w_num, w_end)
	where
		n_str = let base_num = from_position_north pos
		in snd3 (hd (match NUMBER_REGEX (reverse (takeWhile (\e = isMember e NUMBER_CHARS) base_num))))
		e_str = let base_num = from_position_east pos
		in snd3 (hd (match NUMBER_REGEX (reverse (takeWhile (\e = isMember e NUMBER_CHARS) base_num))))
		s_str = let base_num = from_position_south pos
		in snd3 (hd (match NUMBER_REGEX (reverse (takeWhile (\e = isMember e NUMBER_CHARS) base_num))))
		w_str = let base_num = from_position_west pos
		in snd3 (hd (match NUMBER_REGEX (reverse (takeWhile (\e = isMember e NUMBER_CHARS) base_num))))
		n_end = {pos&y=(rows m + pos.y - length n_str) rem (rows m)}
		e_end = {pos&x=(pos.x + length e_str) rem (cols m)}
		s_end = {pos&y=(pos.y + length s_str) rem (rows m)}
		w_end = {pos&x=(cols m + pos.x - length w_str) rem (cols m)}
		sub_parse_number :: [Char] -> Number
		sub_parse_number str = fromString (toString str)
		n_num = sub_parse_number n_str
		e_num = sub_parse_number e_str
		s_num = sub_parse_number s_str
		w_num = sub_parse_number w_str
		
	find_matching_pair_dist str this other
		= sum [1 \\ _ <- takeWhile (\e = sum[1 \\ c <- e | c == this] > sum[1 \\ c <- e | c == other]) (drop 2 (inits str))] 

	parse '\000' pos = I_TERMINATE
	parse '\001' pos = I_GET_TIME
	
	parse '\007' pos = I_BEEP
	
	parse '\010' pos = I_CLEAR_CONSOLE
	
	parse '\016' pos = I_START_NORTH pos
	parse '\017' pos = I_START_EAST pos
	parse '\020' pos = I_START_SOUTH pos
	parse '\021' pos = I_START_WEST pos
	parse '\022' pos = I_START_RANDOM pos
	
	parse '\023' pos = I_MAYBE_GOTO_NORTH other
	where
		dist = sum [1 \\ _ <- takeWhile (\e = e <> '\024' /* && e <> '\036' */) (from_position_south pos)]
		other = {pos&y=(pos.y + dist) rem (rows m)}
		
	parse '\024' pos = I_MAYBE_GOTO_SOUTH other
	where
		dist = sum [1 \\ _ <- takeWhile (\e = e <> '\023' /* && e <> '\034' */) (from_position_north pos)]
		other = {pos&y=(rows m + pos.y - dist) rem (rows m)}
	
	parse '\026' pos = I_ALWAYS_LOOP_NORTH other
	where
		dist = find_matching_pair_dist (from_position_south pos) '\026' '\027'
		other = {pos&y=(pos.y + dist) rem (rows m)}
		
	parse '\027' pos = I_ALWAYS_LOOP_SOUTH other
	where
		dist = find_matching_pair_dist (from_position_north pos) '\027' '\026'
		other = {pos&y=(rows m + pos.y - dist) rem (rows m)}
	
	parse '\031' pos = I_MAYBE_LOOP_NORTH other
	where
		dist = find_matching_pair_dist (from_position_south pos) '\031' '\032'
		other = {pos&y=(pos.y + dist) rem (rows m)}
	
	parse '\032' pos = I_MAYBE_LOOP_SOUTH other
	where
		dist = find_matching_pair_dist (from_position_north pos) '\032' '\031'
		other = {pos&y=(rows m + pos.y - dist) rem (rows m)}
	
	parse '\034' pos = I_ALWAYS_GOTO_NORTH other
	where
		dist = sum [1 \\ _ <- takeWhile (\e = /* e <> '\024' && */ e <> '\036') (from_position_south pos)]
		other = {pos&y=(pos.y + dist) rem (rows m)}
		
	parse '\035' pos = I_ALWAYS_GOTO_EAST other
	where
		dist = sum [1 \\ _ <- takeWhile (\e = /* e <> '\133' && */ e <> '\037') (from_position_west pos)]
		other = {pos&x=(cols m + pos.x - dist) rem (cols m)}
		
	parse '\036' pos = I_ALWAYS_GOTO_SOUTH other
	where
		dist = sum [1 \\ _ <- takeWhile (\e = /* e <> '\023' && */ e <> '\034') (from_position_north pos)]
		other = {pos&y=(rows m + pos.y - dist) rem (rows m)}
		
	parse '\037' pos = I_ALWAYS_GOTO_WEST other
	where
		dist = sum [1 \\ _ <- takeWhile (\e = /* e <> '\135' && */ e <> '\035') (from_position_east pos)]
		other = {pos&x=(pos.x + dist) rem (cols m)}
	
	parse '\040' pos = I_NO_OP
	parse '\041' pos = I_WRITE_SHORT
	parse '\042' pos = I_LITERAL_SINGLE (toValue '\'')
	parse '\043' pos = I_HASH
	parse '\044' pos = I_GET_ENV_VAR
	parse '\045' pos = I_MODULUS
	parse '\046' pos = I_PAIR
	parse '\047' pos // string
		= I_LITERAL_REGION (toValue (toStack n_str), n_end) (toValue (toStack e_str), e_end) (toValue (toStack s_str), s_end) (toValue (toStack w_str), w_end)
	where
		n_str = takeWhile ((<>) '\047') (from_position_north pos)
		e_str = takeWhile ((<>) '\047') (from_position_east pos)
		s_str = takeWhile ((<>) '\047') (from_position_south pos)
		w_str = takeWhile ((<>) '\047') (from_position_west pos)
		n_end = {pos&y=(abs(pos.y - length n_str)) rem (rows m)}
		e_end = {pos&x=(abs(pos.x + length e_str)) rem (cols m)}
		s_end = {pos&y=(abs(pos.y + length s_str)) rem (rows m)}
		w_end = {pos&x=(abs(pos.x - length w_str)) rem (cols m)}
		
	parse '\050' pos = I_ALWAYS_LOOP_WEST other
	where
		dist = find_matching_pair_dist (from_position_east pos) '\050' '\051'
		other = {pos&x=(pos.x + dist) rem (cols m)}
		
	parse '\051' pos = I_ALWAYS_LOOP_EAST other
	where
		dist = find_matching_pair_dist (from_position_east pos) '\051' '\050'
		other = {pos&x=(cols m + pos.x - dist) rem (cols m)}
		
	parse '\052' pos = I_DOT_PRODUCT
	parse '\053' pos = I_ADD
	parse '\054' pos = I_CONCATENATE
	
	parse '\055' pos = parse_literal_number pos
	parse '\056' pos = parse_literal_number pos
	
	parse '\057' pos = I_REFLECT_IDENTITY
	
	parse '\060' pos = parse_literal_number pos
	parse '\061' pos = parse_literal_number pos
	parse '\062' pos = parse_literal_number pos
	parse '\063' pos = parse_literal_number pos
	parse '\064' pos = parse_literal_number pos
	parse '\065' pos = parse_literal_number pos
	parse '\066' pos = parse_literal_number pos
	parse '\067' pos = parse_literal_number pos
	parse '\070' pos = parse_literal_number pos
	parse '\071' pos = parse_literal_number pos
	parse '\072' pos = parse_literal_number pos
	
	parse '\073' pos = I_UNFLATTEN
	parse '\074' pos = I_LESS_THAN
	parse '\075' pos = I_EQUAL_TO
	parse '\076' pos = I_MORE_THAN
	parse '\077' pos = I_READ_SHORT
	parse '\100' pos = I_REGEX
	parse '\101' pos = I_LITERAL_SINGLE (toValue (toStack ['A'..'Z']))
	parse '\102' pos = I_FROM_BINARY
	parse '\103' pos = I_ARC_COSINE
	parse '\104' pos = I_HAS_DUPLICATES
	parse '\105' pos = parse_literal_number pos // E for exponential
	parse '\106' pos = I_IS_PALINDROME
	parse '\107' pos = I_FILTER
	parse '\110' pos = I_TAIL
	parse '\111' pos = I_IMAGINARY_PART
	parse '\112' pos = I_REAL_PART
	parse '\113' pos = I_DROP
	parse '\114' pos = I_IS_LIST
	parse '\115' pos = I_MAXIMUM
	parse '\116' pos = I_IS_NUMBER
	parse '\117' pos = I_IS_SORTED
	parse '\120' pos = I_IS_PRIME
	parse '\121' pos = I_IS_RATIONAL
	parse '\122' pos = I_COUNT_REPEATS
	parse '\123' pos = I_ARC_SINE
	parse '\124' pos = I_ARC_TANGENT
	parse '\125' pos = I_UNIFORM_EXPAND
	parse '\126' pos = I_EVAL
	parse '\127' pos = I_DROP_WHILE
	parse '\130' pos = I_DIAGONALIZE
	parse '\131' pos = I_JOIN
	parse '\132' pos = I_IS_INTEGER
	
	parse '\133' pos = I_MAYBE_GOTO_WEST other
	where
		dist = sum [1 \\ _ <- takeWhile (\e = e <> '\135' /* && e <> '\035' */) (from_position_east pos)]
		other = {pos&x=(pos.x + dist) rem (cols m)}
	
	parse '\134' pos = I_REFLECT_INVERSE
	
	parse '\135' pos = I_MAYBE_GOTO_EAST other
	where
		dist = sum [1 \\ _ <- takeWhile (\e = e <> '\133' /* && e <> '\037' */) (from_position_west pos)]
		other = {pos&x=(cols m + pos.x - dist) rem (cols m)}
		
	parse '\136' pos = I_EXPONENTIATE
	
	parse '\137' pos = I_FLATTEN
	parse '\140' pos = I_LITERAL_SINGLE (toValue '\n')
	parse '\141' pos = I_LITERAL_SINGLE (toValue (toStack ['a'..'z']))
	parse '\142' pos = I_TO_BINARY
	parse '\143' pos = I_COSINE
	parse '\144' pos = I_REMOVE_DUPLICATES
	parse '\145' pos = parse_literal_number pos // euler's constant
	parse '\146' pos = I_REVERSE
	parse '\147' pos = I_GROUP
	parse '\150' pos = I_HEAD
	parse '\151' pos = parse_literal_number pos // imaginary unit
	parse '\152' pos = I_REPOSITION
	parse '\153' pos = I_TAKE
	parse '\154' pos = I_LENGTH
	parse '\155' pos = I_MINIMUM
	parse '\156' pos = I_RANDOM
	parse '\157' pos = I_SORT
	parse '\160' pos = I_PRIMES
	parse '\161' pos = I_ABSOLUTE
	parse '\162' pos = I_REPEAT
	parse '\163' pos = I_SINE
	parse '\164' pos = I_TANGENT
	parse '\165' pos = I_UNIFORM_COLLAPSE
	parse '\166' pos = I_TO_STRING
	parse '\167' pos = I_TAKE_WHILE
	parse '\170' pos = I_CROSS_PRODUCT
	parse '\171' pos = I_SPLIT
	parse '\172' pos = I_ROUND
	
	parse '\173' pos = I_MAYBE_LOOP_WEST other
	where
		dist = find_matching_pair_dist (from_position_east pos) '\173' '\175'
		other = {pos&x=(pos.x + dist) rem (cols m)}
	
	parse '\174' pos = I_REFLECT_VERTICAL
	
	parse '\175' pos = I_MAYBE_LOOP_EAST other
	where
		dist = find_matching_pair_dist (from_position_east pos) '\175' '\173'
		other = {pos&x=(cols m + pos.x - dist) rem (cols m)}
		
	parse '\176' pos = I_NEGATE
	parse '\177' pos = I_DELETE_FILE
	parse '\200' pos = I_WRITE_LONG
	parse '\201' pos = I_READ_LONG
	parse '\202' pos = I_SYSTEM_COMMAND
	parse '\203' pos = I_WRITE_FILE
	parse '\204' pos = I_READ_FILE
	parse '\205' pos = I_RESTART_LAST
	parse '\206' pos = I_RESTART_RANDOM start
	parse '\207' pos = I_REFLECT_HORIZONTAL
	parse '\210' pos = I_TURN_ANTICLOCKWISE
	parse '\211' pos = I_TURN_CLOCKWISE
	parse '\212' pos = I_MAYBE_MOVE_NORTH
	parse '\213' pos = I_MAYBE_MOVE_EAST
	parse '\214' pos = I_MAYBE_MOVE_SOUTH
	parse '\215' pos = I_MAYBE_MOVE_WEST
	parse '\216' pos = I_MAYBE_MOVE_RANDOM
	parse '\217' pos = I_ALWAYS_MOVE_NORTH
	parse '\220' pos = I_ALWAYS_MOVE_EAST
	parse '\221' pos = I_ALWAYS_MOVE_SOUTH
	parse '\222' pos = I_ALWAYS_MOVE_WEST
	parse '\223' pos = I_ALWAYS_MOVE_RANDOM
	parse '\224' pos = I_MAYBE_JUMP_SE
	parse '\225' pos = I_MAYBE_JUMP_SW
	parse '\226' pos = I_MAYBE_JUMP_NW
	parse '\227' pos = I_MAYBE_JUMP_NE
	parse '\230' pos = I_ALWAYS_JUMP_SE
	parse '\231' pos = I_ALWAYS_JUMP_SW
	parse '\232' pos = I_ALWAYS_JUMP_NW
	parse '\233' pos = I_ALWAYS_JUMP_NE
	parse '\234' pos = I_MAYBE_SKIP_NEXT
	parse '\235' pos = I_ALWAYS_SKIP_NEXT
	parse '\236' pos = I_DUPLICATE_TOP
	parse '\237' pos = I_TOP_RIGHT_TO_LEFT
	parse '\240' pos = I_TOP_LEFT_TO_RIGHT
	parse '\241' pos = I_SWAP_STACK_TOPS
	parse '\242' pos = I_PREPEND_RIGHT_TO_LEFT
	parse '\243' pos = I_PREPEND_LEFT_TO_RIGHT
	parse '\244' pos = I_SWAP_FULL_STACKS
	parse '\245' pos = I_STORE_LEFT
	parse '\246' pos = I_RECALL_LEFT
	parse '\247' pos = I_STORE_RIGHT
	parse '\250' pos = I_RECALL_RIGHT
	parse '\251' pos = undef
	parse '\252' pos = undef
	parse '\253' pos = undef
	parse '\254' pos = undef
	parse '\255' pos = undef
	parse '\256' pos = undef
	parse '\257' pos = undef
	parse '\260' pos = undef
	parse '\261' pos = undef
	parse '\262' pos = undef
	parse '\263' pos = undef
	parse '\264' pos = undef
	parse '\265' pos = undef
	parse '\266' pos = undef
	parse '\267' pos = undef
	parse '\270' pos = undef
	parse '\271' pos = undef
	parse '\272' pos = undef
	parse '\273' pos = undef
	parse '\274' pos = undef
	parse '\275' pos = undef
	parse '\276' pos = undef
	parse '\277' pos = undef
	parse '\300' pos = undef
	parse '\301' pos = undef
	parse '\302' pos = undef
	parse '\303' pos = undef
	parse '\304' pos = undef
	parse '\305' pos = undef
	parse '\306' pos = undef
	parse '\307' pos = undef
	parse '\310' pos = undef
	parse '\311' pos = undef
	parse '\312' pos = undef
	parse '\313' pos = undef
	parse '\314' pos = undef
	parse '\315' pos = undef
	parse '\316' pos = undef
	parse '\317' pos = undef
	parse '\320' pos = undef
	parse '\321' pos = undef
	parse '\322' pos = undef
	parse '\323' pos = undef
	parse '\324' pos = undef
	parse '\325' pos = undef
	parse '\326' pos = undef
	parse '\327' pos = undef
	parse '\330' pos = undef
	parse '\331' pos = undef
	parse '\332' pos = undef
	parse '\333' pos = undef
	parse '\334' pos = undef
	parse '\335' pos = undef
	parse '\336' pos = undef
	parse '\337' pos = undef
	parse '\340' pos = undef
	parse '\341' pos = undef
	parse '\342' pos = undef
	parse '\343' pos = undef
	parse '\344' pos = undef
	parse '\345' pos = undef
	parse '\346' pos = undef
	parse '\347' pos = undef
	parse '\350' pos = undef
	parse '\351' pos = undef
	parse '\352' pos = undef
	parse '\353' pos = undef
	parse '\354' pos = undef
	parse '\355' pos = undef
	parse '\356' pos = undef
	parse '\357' pos = undef
	parse '\360' pos = undef
	parse '\361' pos = undef
	parse '\362' pos = undef
	parse '\363' pos = undef
	parse '\364' pos = undef
	parse '\365' pos = undef
	parse '\366' pos = undef
	parse '\367' pos = undef
	parse '\370' pos = undef
	parse '\371' pos = undef
	parse '\372' pos = undef
	parse '\373' pos = undef
	parse '\374' pos = undef
	parse '\375' pos = undef
	parse '\376' pos = undef
	parse '\377' pos = undef