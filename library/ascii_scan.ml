

open Num
open Char
open String
open List
open Stream

open Basic

let isemicolon	= code ';'
let icolon	= code ':'
let icomma	= code ','
let idot	= code '.'
let ibar	= code '|'
let irparen	= code ')'
let ilparen	= code '('
let ircurly	= code '}'
let ilcurly	= code '{'
let irsquare	= code ']'
let ilsquare	= code '['


type scanner = 
	{ stream	: char t

	(* parameters *)
	; escape		: char list
	; whitespace		: char list

	(* state *)
	; mutable cchar		: char
	; mutable escapep	: bool  
	}


let explode str =
 let l = String.length str in

 let rec aux i = 
    if i = l then []
    else (get str i) :: aux (1+i)
  in

 aux 0


(* chars are reversed *)
let implode_rev chars =
 let l = (List.length chars) in
 let s = String.create l in
  let rest = ref chars in
  let i = ref (l - 1) in

  while not (!rest = [])
  do String.set s !i (hd !rest);
     i := !i - 1;
     rest := tl !rest
  done

 ; s



let make_scanner escape white stream =
 { stream = stream

 ; escape = explode escape
 ; whitespace = explode white

 ; cchar = (next stream)
 ; escapep = false
 }


let scan_bump s =
 s.escapep <- false;
 s.cchar <- next s.stream;
 ()


let scan_next s =
 scan_bump s;
 if s.cchar = '\\'  then (scan_bump s; s.escapep <- true);
 ()


let scan_at_eof_p s = try (empty s.stream; true) with _ -> false
let scan_escape_p s = s.escapep

let scan_cur_char s = s.cchar
let scan_at_char_p s c = s.cchar = c

let scan_char s c = 
 if (scan_at_char_p s c) 
    then scan_next s
    else error ["scanner"; "char"; (Char.escaped c); (Char.escaped s.cchar)] [] []
 ; ()


let scan_cur_byte s = code s.cchar
let scan_at_byte_p s c = (code s.cchar) = c

let scan_byte s c = 
 if (scan_at_byte_p s c) 
    then scan_next s
    else error ["scanner"; "char"; Char.escaped (chr c); Char.escaped s.cchar] [] []


let numeric_digits = ['0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9']

let scan_at_digit_p s =
  (mem s.cchar numeric_digits)	

let rec scan_whitespace s = 
 if (mem s.cchar s.whitespace)
    then (scan_next s; scan_whitespace s)
 ; ()


let scan_string s =
 let acc = ref [] in
 while not ((mem s.cchar s.escape) & (not s.escapep))
  do acc := s.cchar :: !acc; scan_next s
  done

 ; implode_rev !acc


(* arg digits are in reverse order *)
let digits_to_num digits = 
  let num10 = num_of_int 10 in
  let rec aux acc power digits = 
    aux (add_num (mult_num (power_num num10 power)
			   (num_of_string (Char.escaped (hd digits))))
		 acc)
	(succ_num power)
	(tl digits)

    in aux (num_of_int 0) (num_of_int 0) digits


let scan_num s =
 let acc = ref [] in
 while (scan_at_digit_p s)
  do acc := s.cchar :: !acc;  scan_next s
  done

 ; digits_to_num !acc


let scan_char_delimited_list s itemf ld rd sep =

 scan_char s ld;

 if (scan_at_char_p s rd) then []
 else let acc = ref [(itemf ())] in
	while (scan_at_char_p s sep)
	  do (scan_char s sep); acc := (itemf ()) :: !acc
	  done;
	
	if (scan_at_char_p s rd) then rev !acc
	else error ["scanner"; "delimited_list"; Char.escaped s.cchar; Char.escaped rd] [] []
    

let scan_delimited_list s itemf ld rd sep =

 scan_byte s ld;

 if (scan_at_byte_p s rd) then []
 else let acc = ref [(itemf ())] in
	while (scan_at_byte_p s sep)
	  do (scan_byte s sep); acc := (itemf ()) :: !acc
	  done;
	
	if (scan_at_byte_p s rd) then rev !acc
	else error ["scanner"; "delimited_list"; Char.escaped s.cchar; Char.escaped (chr rd)] [] []
    






