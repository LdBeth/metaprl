

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
	; mutable eofp		: bool
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
 (* print_string "make_scanner  "; *)
 let emptyp = try (empty stream; true) with _ -> false in
 { stream = stream

 ; escape = explode escape
 ; whitespace = explode white

 ; cchar = if emptyp then ' ' else next stream
 ; escapep = false
 ; eofp = emptyp
 }


let scan_stream_eof_p s = try (empty s.stream; true) with _ -> false

let scan_bump s =
 (* print_string (Char.escaped s.cchar); *)
 if s.eofp then error ["scanner"; "bump"; "eof"] [] []
 else if (scan_stream_eof_p s) then s.eofp <- true
 else
   (s.escapep <- false;
    s.cchar <- next s.stream;
    ())
 (* ; print_string (Char.escaped s.cchar); 
 print_newline () *)



let scan_next s =
 scan_bump s;
 if s.cchar = '\\'  then (scan_bump s; s.escapep <- true);
 ()


let scan_at_eof_p s = s.eofp (*or scan_stream_eof_p s *)

let scan_escape_p s = s.escapep

let scan_cur_char s = s.cchar
let scan_at_char_p s c = (not (scan_at_eof_p s)) & (s.cchar = c) & not (scan_escape_p s)

let scan_char s c = 
 if (scan_at_char_p s c) 
    then scan_next s
 else if (scan_at_eof_p s) 
    then  error ["scanner"; "char"; Char.escaped c; "eof"] [] []
 else error ["scanner"; "char"; (Char.escaped c); (Char.escaped s.cchar)] [] []
 ; ()


let scan_cur_byte s = code s.cchar
let scan_at_byte_p s c = (not (scan_at_eof_p s)) & (code s.cchar) = c  & not (scan_escape_p s)

let scan_byte s c = 
 if (scan_at_byte_p s c) 
    then scan_next s

   else if (scan_at_eof_p s) 
	    then  error ["scanner"; "char"; Char.escaped (chr c); "eof"] [] []
   else error ["scanner"; "char"; Char.escaped (chr c); Char.escaped s.cchar] [] []


let numeric_digits = ['0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9']

let scan_at_digit_p s =
  (not (scan_at_eof_p s))  & (mem s.cchar numeric_digits)	

let rec scan_whitespace s = 
 if (not (scan_at_eof_p s)) & (mem s.cchar s.whitespace)
    then (scan_next s; scan_whitespace s)
 ; ()


let scan_string s =
 (* print_string " sstr "; *)
 let acc = ref [] in
 while not (scan_at_eof_p s) & not ((mem s.cchar s.escape) & (not s.escapep))
  do acc := s.cchar :: !acc; scan_next s
  done

 ; (let ss = implode_rev !acc
    in (* print_string ss; print_newline();  *)
	ss)


(* arg digits are in reverse order *)
let digits_to_num digits = 
  let num10 = num_of_int 10 in
  let rec aux acc power digits = 
    if digits = [] then acc
    else aux (add_num (mult_num (power_num num10 power)
			        (num_of_string (Char.escaped (hd digits))))
		       acc)
 	     (succ_num power)
	     (tl digits)

    in aux (num_of_int 0) (num_of_int 0) digits


let scan_num s =
 let acc = ref [] in

 while (not (scan_at_eof_p s)) & (scan_at_digit_p s)
  do acc := s.cchar :: !acc;  scan_next s
  done
 
 ; digits_to_num !acc


let scan_char_delimited_list s itemf ld rd sep =

 scan_char s ld;

 if (scan_at_char_p s rd) then (scan_char s rd; [])
 else let acc = ref [(itemf ())] in
	while (scan_at_char_p s sep)
	  do (scan_char s sep); acc := (itemf ()) :: !acc
	  done;
	
	if (scan_at_char_p s rd) then (scan_char s rd; rev !acc)
	else if (scan_at_eof_p s) 
	  then  error ["scanner"; "delimited_list"; "eof"; Char.escaped rd] [] []
	else error ["scanner"; "delimited_list"; Char.escaped s.cchar; Char.escaped rd] [] []
    

let scan_delimited_list s itemf ld rd sep =

 scan_byte s ld;

 if (scan_at_byte_p s rd) then (scan_byte s rd; [])
 else let acc = ref [(itemf ())] in
	while (scan_at_byte_p s sep)
	  do (scan_byte s sep); acc := (itemf ()) :: !acc
	  done;
	
	if (scan_at_byte_p s rd) then (scan_byte s rd; rev !acc)
	else if (scan_at_eof_p s) 
	  then  error ["scanner"; "delimited_list"; "eof"; Char.escaped (chr rd)] [] []
	else error ["scanner"; "delimited_list"; Char.escaped s.cchar; Char.escaped (chr rd)] [] []
    






