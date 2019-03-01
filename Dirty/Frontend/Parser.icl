implementation module Dirty.Frontend.Parser

import Dirty.Backend.Value, Dirty.Backend.Stack, Dirty.Backend.Number
import Dirty.Frontend.Arguments
import Dirty.Runtime.Instruction
from Dirty.Types import ::Point(..)
import Data.Matrix
import Text
import StdEnv

START_CHARS :== ['\016\017\020\021\022']
NUMBER_CHARS :== ['0123456789.:i-']

parseFile :: (Matrix Char) -> (Matrix Instruction, Vector Point)
parseFile m = ({{parse c {x=x,y=y} \\ c <-: r & x <- [0..]} \\ r <-: m & y <- [0..]}, start)
where
	start = {{x=x,y=y} \\ r <-: m & y <- [0..], c <-: r & x <- [0..] | isMember c START_CHARS}
	
	parse_literal_number pos // . associates first, then : and i in the order they appear (i multiplies whatever is in front of it by `i`, adjacency behind adds, : behind divides)
		= I_LITERAL_REGION (toValue n_num, n_end) (toValue e_num, e_end) (toValue s_num, s_end) (toValue w_num, w_end)
	where
		n_str = let base_num = reverse [m.[y,pos.x] \\ y <- [pos.y..rows m-1] ++ [0..pos.y-1]]
		in reverse (takeWhile (\e = isMember e NUMBER_CHARS) base_num)
		e_str = let base_num = [m.[pos.y,x] \\ x <- [pos.x..cols m-1] ++ [0..pos.x-1]]
		in reverse (takeWhile (\e = isMember e NUMBER_CHARS) base_num)
		s_str = let base_num = [m.[y,pos.x] \\ y <- [pos.y..rows m-1] ++ [0..pos.y-1]]
		in reverse (takeWhile (\e = isMember e NUMBER_CHARS) base_num)
		w_str = let base_num = reverse [m.[pos.y,x] \\ x <- [pos.x..cols m-1] ++ [0..pos.x-1]]
		in reverse (takeWhile (\e = isMember e NUMBER_CHARS) base_num)
		n_end = {pos&y=(abs(pos.y - length n_str)) rem (rows m)}
		e_end = {pos&x=(abs(pos.x + length e_str)) rem (cols m)}
		s_end = {pos&y=(abs(pos.y + length s_str)) rem (rows m)}
		w_end = {pos&x=(abs(pos.x - length w_str)) rem (cols m)}
		//handle_colons str = foldr (/) one [fromString (toString e) \\ e <- split [':'] str]
		sub_parse_number :: [Char] -> Number
		sub_parse_number str = fromString (toString str) // TODO
		n_num = sub_parse_number n_str
		e_num = sub_parse_number e_str
		s_num = sub_parse_number s_str
		w_num = sub_parse_number w_str

	parse '\000' pos = undef
	
	parse '\007' pos = undef
	
	parse '\016' pos = I_START_NORTH pos
	parse '\017' pos = I_START_EAST pos
	parse '\020' pos = I_START_SOUTH pos
	parse '\021' pos = I_START_WEST pos
	parse '\022' pos = I_START_RANDOM pos
	
	parse '\023' pos = I_MAYBE_GOTO_NORTH undef // TODO: bracket matching
	parse '\024' pos = I_MAYBE_GOTO_SOUTH undef
	
	parse '\026' pos = I_ALWAYS_LOOP_NORTH undef
	parse '\027' pos = I_ALWAYS_LOOP_SOUTH undef
	
	parse '\031' pos = I_MAYBE_LOOP_NORTH undef
	parse '\032' pos = I_MAYBE_LOOP_SOUTH undef
	
	parse '\034' pos = I_ALWAYS_GOTO_NORTH undef
	parse '\035' pos = I_ALWAYS_GOTO_EAST undef
	parse '\036' pos = I_ALWAYS_GOTO_SOUTH undef
	parse '\037' pos = I_ALWAYS_GOTO_WEST undef
	
	parse '\040' pos = undef
	parse '\041' pos = undef
	parse '\042' pos = undef
	parse '\043' pos = undef
	parse '\044' pos = undef
	parse '\045' pos = undef
	parse '\046' pos = undef
	parse '\047' pos // string
		= I_LITERAL_REGION (toValue (toStack n_str), n_end) (toValue (toStack e_str), e_end) (toValue (toStack s_str), s_end) (toValue (toStack w_str), w_end)
	where
		n_str = let
			base_str = reverse [m.[y,pos.x] \\ y <- [pos.y..rows m-1] ++ [0..pos.y-1]]
		in takeWhile ((<>) '\047') base_str
		e_str = let
			base_str = [m.[pos.y,x] \\ x <- [pos.x..cols m-1] ++ [0..pos.x-1]]
		in takeWhile ((<>) '\047') base_str
		s_str = let
			base_str = [m.[y,pos.x] \\ y <- [pos.y..rows m-1] ++ [0..pos.y-1]]
		in takeWhile ((<>) '\047') base_str
		w_str = let
			base_str = reverse [m.[pos.y,x] \\ x <- [pos.x..cols m-1] ++ [0..pos.x-1]]
		in takeWhile ((<>) '\047') base_str
		n_end = {pos&y=(abs(pos.y - length n_str)) rem (rows m)}
		e_end = {pos&x=(abs(pos.x + length e_str)) rem (cols m)}
		s_end = {pos&y=(abs(pos.y + length s_str)) rem (rows m)}
		w_end = {pos&x=(abs(pos.x - length w_str)) rem (cols m)}
		
	parse '\050' pos = undef
	parse '\051' pos = undef
	parse '\052' pos = undef
	parse '\053' pos = undef
	parse '\054' pos = undef
	
	parse '\055' pos = parse_literal_number pos
	parse '\056' pos = parse_literal_number pos
	
	parse '\057' pos = undef // slash
	
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
	
	parse '\073' pos = undef
	parse '\074' pos = undef
	parse '\075' pos = undef
	parse '\076' pos = undef
	parse '\077' pos = undef
	parse '\100' pos = undef
	parse '\101' pos = undef
	parse '\102' pos = undef
	parse '\103' pos = undef
	parse '\104' pos = undef
	parse '\105' pos = undef
	parse '\106' pos = undef
	parse '\107' pos = undef
	parse '\110' pos = undef
	parse '\111' pos = undef
	parse '\112' pos = undef
	parse '\113' pos = undef
	parse '\114' pos = undef
	parse '\115' pos = undef
	parse '\116' pos = undef
	parse '\117' pos = undef
	parse '\120' pos = undef
	parse '\121' pos = undef
	parse '\122' pos = undef
	parse '\123' pos = undef
	parse '\124' pos = undef
	parse '\125' pos = undef
	parse '\126' pos = undef
	parse '\127' pos = undef
	parse '\130' pos = undef
	parse '\131' pos = undef
	parse '\132' pos = undef
	parse '\133' pos = undef
	parse '\134' pos = undef
	parse '\135' pos = undef
	parse '\136' pos = undef
	parse '\137' pos = undef
	parse '\140' pos = undef
	parse '\141' pos = undef
	parse '\142' pos = undef
	parse '\143' pos = undef
	parse '\144' pos = undef
	parse '\145' pos = undef
	parse '\146' pos = undef
	parse '\147' pos = undef
	parse '\150' pos = undef
	parse '\151' pos = parse_literal_number pos // imaginary unit
	parse '\152' pos = undef
	parse '\153' pos = undef
	parse '\154' pos = undef
	parse '\155' pos = undef
	parse '\156' pos = undef
	parse '\157' pos = undef
	parse '\160' pos = undef
	parse '\161' pos = undef
	parse '\162' pos = undef
	parse '\163' pos = undef
	parse '\164' pos = undef
	parse '\165' pos = undef
	parse '\166' pos = undef
	parse '\167' pos = undef
	parse '\170' pos = undef
	parse '\171' pos = undef
	parse '\172' pos = undef
	parse '\173' pos = undef
	parse '\174' pos = undef
	parse '\175' pos = undef
	parse '\176' pos = undef
	parse '\177' pos = undef
	parse '\200' pos = undef
	parse '\201' pos = undef
	parse '\202' pos = undef
	parse '\203' pos = undef
	parse '\204' pos = undef
	parse '\205' pos = undef
	parse '\206' pos = undef
	parse '\207' pos = undef
	parse '\210' pos = undef
	parse '\211' pos = undef
	parse '\212' pos = undef
	parse '\213' pos = undef
	parse '\214' pos = undef
	parse '\215' pos = undef
	parse '\216' pos = undef
	parse '\217' pos = undef
	parse '\220' pos = undef
	parse '\221' pos = undef
	parse '\222' pos = undef
	parse '\223' pos = undef
	parse '\224' pos = undef
	parse '\225' pos = undef
	parse '\226' pos = undef
	parse '\227' pos = undef
	parse '\230' pos = undef
	parse '\231' pos = undef
	parse '\232' pos = undef
	parse '\233' pos = undef
	parse '\234' pos = undef
	parse '\235' pos = undef
	parse '\236' pos = undef
	parse '\237' pos = undef
	parse '\240' pos = undef
	parse '\241' pos = undef
	parse '\242' pos = undef
	parse '\243' pos = undef
	parse '\244' pos = undef
	parse '\245' pos = undef
	parse '\246' pos = undef
	parse '\247' pos = undef
	parse '\250' pos = undef
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