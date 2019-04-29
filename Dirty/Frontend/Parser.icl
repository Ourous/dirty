implementation module Dirty.Frontend.Parser

//import Dirty.Backend.Value, Dirty.Backend.Stack, Dirty.Backend.Number
import Dirty.Backend
import Dirty.Frontend.Arguments
import Dirty.Runtime.Instruction
from Dirty.Types import ::Point(..)
import Data.Matrix, Data.List
import StdDebug
//import Text
import StdEnv
import Regex

START_CHARS :== ['\016\017\020\021\022']
NUMBER_CHARS :== ['0123456789.:i-Ee']
NUMBER_REGEX :== regex "^-?(?:(?:\\d*\\.?\\d*(?:E-?\d+)?)|e)?(?::(?:\\d*\\.?\\d*(?:E-?\d+)?)|e)?(?:i(?:-?(?:(?:\\d*\\.?\\d*(?:E-?\d+)?)|e)?(?::(?:\\d*\\.?\\d*(?:E-?\d+)?)|e)?)?)?"


parseFile :: (Matrix Char) -> (Matrix Instruction, Vector Point)
parseFile m = ({{parse c {x=x,y=y} \\ c <-: r & x <- [0..]} \\ r <-: m & y <- [0..]}, start)
where
	start = {{x=x,y=y} \\ r <-: m & y <- [0..], c <-: r & x <- [0..] | isMember c START_CHARS}
	
	// "^(-?\\d*\\.?\\d*(?:(?::\\d*\\.*\\d*)?(?:i-?\\d*\\.?\\d*(?::\\d*\\.*\\d*)?)?)?)" should match valid numeric literals
	// will have to be manually constructed from smaller parse sections
	
	from_position_north pos = [m.[y,pos.x] \\ y <- [pos.y, pos.y-1 ..0] ++ [rows m-1, rows m-2 ..pos.y]]
	from_position_east pos = [m.[pos.y,x] \\ x <- [pos.x ..cols m-1] ++ [0 ..pos.x-1]]
	from_position_south pos = [m.[y,pos.x] \\ y <- [pos.y ..rows m-1] ++ [0 ..pos.y-1]]
	from_position_west pos = [m.[pos.y,x] \\ x <- [pos.x, pos.x-1 ..0] ++ [cols m-1, cols m-2 ..pos.x]]
	
	parse_literal_number pos // . associates first, then : and i in the order they appear (i multiplies whatever is in front of it by `i`, adjacency behind adds, : behind divides)
		= I_LITERAL_REGION (toValue n_num, n_end) (toValue e_num, e_end) (toValue s_num, s_end) (toValue w_num, w_end)
	where
		n_str = let base_num = (takeWhile (\e = isMember e NUMBER_CHARS) (from_position_north pos))
		in if(base_num > []) (snd3 (hd (match NUMBER_REGEX base_num))) []
		e_str = let base_num = (takeWhile (\e = isMember e NUMBER_CHARS) (from_position_east pos))
		in if(base_num > []) (snd3 (hd (match NUMBER_REGEX base_num))) []
		s_str = let base_num = (takeWhile (\e = isMember e NUMBER_CHARS) (from_position_south pos))
		in if(base_num > []) (snd3 (hd (match NUMBER_REGEX base_num))) []
		w_str = let base_num = (takeWhile (\e = isMember e NUMBER_CHARS) (from_position_west pos))
		in if(base_num > []) (snd3 (hd (match NUMBER_REGEX base_num))) []
		n_end = {pos&y=((rows m + pos.y + 1 - length n_str))rem(rows m)}
		e_end = {pos&x=((cols m + pos.x - 1 + length e_str))rem(cols m)}
		s_end = {pos&y=((rows m + pos.y - 1 + length s_str))rem(rows m)}
		w_end = {pos&x=((cols m + pos.x + 1 - length w_str))rem(cols m)}
		sub_parse_number :: [Char] -> Number
		sub_parse_number str = fromString (toString str)
		n_num = sub_parse_number n_str
		e_num = sub_parse_number e_str
		s_num = sub_parse_number s_str
		w_num = sub_parse_number w_str
		
	find_matching_pair_dist str this other
		= 1 + sum [1 \\ _ <- takeWhile (\e = sum[1 \\ c <- e | c == this] > sum[1 \\ c <- e | c == other]) (drop 2 (inits str))]

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
		n_str = takeWhile ((<>) '\047') (tl (from_position_north pos))
		e_str = takeWhile ((<>) '\047') (tl (from_position_east pos))
		s_str = takeWhile ((<>) '\047') (tl (from_position_south pos))
		w_str = takeWhile ((<>) '\047') (tl (from_position_west pos))
		n_end = {pos&y=((pos.y - 1 - length n_str))}
		e_end = {pos&x=((pos.x + length e_str))+1}
		s_end = {pos&y=((pos.y + length s_str))+1}
		w_end = {pos&x=((pos.x - 1 - length w_str))}
		
	parse '\050' pos = I_ALWAYS_LOOP_WEST other
	where
		dist = find_matching_pair_dist (from_position_east pos) '\050' '\051'
		other = {pos&x=(pos.x + dist) rem (cols m)}
		
	parse '\051' pos = I_ALWAYS_LOOP_EAST other
	where
		dist = find_matching_pair_dist (from_position_west pos) '\051' '\050'
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
	
	parse '\073' pos = I_ENLIST_FULL_ARG
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
		dist = find_matching_pair_dist (from_position_west pos) '\175' '\173'
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
	parse '\251' pos = abort "\\251 unparseable"
	parse '\252' pos = abort "\\252 unparseable"
	parse '\253' pos = abort "\\253 unparseable"
	parse '\254' pos = I_SWAP_ARG_TOP
	parse '\255' pos = I_EXPLODE_TOP_ARG
	parse '\256' pos = I_WIPE_ARG
	parse '\257' pos = I_POP_ARG
	parse '\260' pos = I_WIPE_OUT
	parse '\261' pos = I_POP_OUT
	parse '\262' pos = abort "\\262 unparseable"
	parse '\263' pos = abort "\\263 unparseable"
	parse '\264' pos = I_SUBTRACT
	parse '\265' pos = I_MULTIPLY
	parse '\266' pos = I_DIVIDE
	parse '\267' pos = I_RECIPROCAL
	parse '\270' pos = I_SQUARE_ROOT
	parse '\271' pos = abort "\\271 unparseable"
	parse '\272' pos = abort "\\272 unparseable"
	parse '\273' pos = abort "\\273 unparseable"
	parse '\274' pos = abort "\\274 unparseable"
	parse '\275' pos = abort "\\275 unparseable"
	parse '\276' pos = abort "\\276 unparseable"
	parse '\277' pos = abort "\\277 unparseable"
	parse '\300' pos = abort "\\300 unparseable"
	parse '\301' pos = abort "\\301 unparseable"
	parse '\302' pos = abort "\\302 unparseable"
	parse '\303' pos = abort "\\303 unparseable"
	parse '\304' pos = abort "\\304 unparseable"
	parse '\305' pos = abort "\\305 unparseable"
	parse '\306' pos = abort "\\306 unparseable"
	parse '\307' pos = abort "\\307 unparseable"
	parse '\310' pos = abort "\\310 unparseable"
	parse '\311' pos = abort "\\311 unparseable"
	parse '\312' pos = abort "\\312 unparseable"
	parse '\313' pos = abort "\\313 unparseable"
	parse '\314' pos = abort "\\314 unparseable"
	parse '\315' pos = abort "\\315 unparseable"
	parse '\316' pos = abort "\\316 unparseable"
	parse '\317' pos = abort "\\317 unparseable"
	parse '\320' pos = abort "\\320 unparseable"
	parse '\321' pos = abort "\\321 unparseable"
	parse '\322' pos = abort "\\322 unparseable"
	parse '\323' pos = abort "\\323 unparseable"
	parse '\324' pos = abort "\\324 unparseable"
	parse '\325' pos = abort "\\325 unparseable"
	parse '\326' pos = abort "\\326 unparseable"
	parse '\327' pos = abort "\\327 unparseable"
	parse '\330' pos = abort "\\330 unparseable"
	parse '\331' pos = abort "\\331 unparseable"
	parse '\332' pos = abort "\\332 unparseable"
	parse '\333' pos = abort "\\333 unparseable"
	parse '\334' pos = abort "\\334 unparseable"
	parse '\335' pos = abort "\\335 unparseable"
	parse '\336' pos = abort "\\336 unparseable"
	parse '\337' pos = abort "\\337 unparseable"
	parse '\340' pos = abort "\\340 unparseable"
	parse '\341' pos = abort "\\341 unparseable"
	parse '\342' pos = abort "\\342 unparseable"
	parse '\343' pos = abort "\\343 unparseable"
	parse '\344' pos = abort "\\344 unparseable"
	parse '\345' pos = abort "\\345 unparseable"
	parse '\346' pos = abort "\\346 unparseable"
	parse '\347' pos = abort "\\347 unparseable"
	parse '\350' pos = abort "\\350 unparseable"
	parse '\351' pos = abort "\\351 unparseable"
	parse '\352' pos = abort "\\352 unparseable"
	parse '\353' pos = abort "\\353 unparseable"
	parse '\354' pos = abort "\\354 unparseable"
	parse '\355' pos = abort "\\355 unparseable"
	parse '\356' pos = abort "\\356 unparseable"
	parse '\357' pos = abort "\\357 unparseable"
	parse '\360' pos = abort "\\360 unparseable"
	parse '\361' pos = abort "\\361 unparseable"
	parse '\362' pos = abort "\\362 unparseable"
	parse '\363' pos = abort "\\363 unparseable"
	parse '\364' pos = abort "\\364 unparseable"
	parse '\365' pos = abort "\\365 unparseable"
	parse '\366' pos = abort "\\366 unparseable"
	parse '\367' pos = abort "\\367 unparseable"
	parse '\370' pos = abort "\\370 unparseable"
	parse '\371' pos = abort "\\371 unparseable"
	parse '\372' pos = abort "\\372 unparseable"
	parse '\373' pos = abort "\\373 unparseable"
	parse '\374' pos = abort "\\374 unparseable"
	parse '\375' pos = abort "\\375 unparseable"
	parse '\376' pos = abort "\\376 unparseable"
	parse '\377' pos = I_LOGARITHM