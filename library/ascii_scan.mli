open Stream
open Num

  type scanner		

  val isemicolon : int
  val icolon	: int
  val icomma	: int
  val idot	: int
  val ibar	: int
  val irparen	: int
  val ilparen	: int
  val ircurly	: int
  val ilcurly	: int
  val irsquare	: int
  val ilsquare	: int

  val make_scanner	: string (* escape *) -> string (* whitespace *) -> char t -> scanner

  (* 
   *	scan_next : advances scanner.
   *	scan_bump : advances scanner to next byte.
   *
   *	Difference is that if next byte is escape then scan_next sets escape
   *	bit and advances again.
   *)
  val scan_next		: scanner -> unit
  val scan_bump		: scanner -> unit

  val scan_at_eof_p	: scanner -> bool
  val scan_escape_p	: scanner -> bool


  (* twould be better if scanner were last arg in following funcs to allow for 
     partial ap without lambdas 
     EG: let scan_lparen = scan_char '(
     not let scan_lparen s = scan_char s '('
   *)

  (* should fail if cur byte is not char *)
  val scan_cur_char	: scanner -> char
  val scan_at_char_p	: scanner -> char -> bool
  val scan_char		: scanner -> char -> unit

  (* should fail if cur byte is not byte *)
  val scan_byte		: scanner -> int (* byte *) -> unit
  val scan_cur_byte	: scanner -> int
  val scan_at_byte_p	: scanner -> int -> bool

  val scan_at_digit_p	: scanner -> bool
  val scan_whitespace	: scanner -> unit

  val scan_string	: scanner -> string
  val scan_num		: scanner -> num

  (* twould probably be better if item scanner took scanner as arg and was first
     arg to scan delimited list to allow definition of list scanners without having scanner
     EG: let scan_foo_list = scan_char_delimited_list (function s -> scan_foo s) '(' ';' ')'
   *)
  val scan_char_delimited_list	: scanner
					 -> (unit -> 'a)	(* item scanner		*)
					 -> char		(* left delimiter	*)
					 -> char		(* right delimiter	*)
					 -> char		(* seperator		*)
					 -> 'a list

  val scan_delimited_list	: scanner
					 -> (unit -> 'a)	(* item scanner		*)
					 -> int			(* left delimiter	*)
					 -> int			(* right delimiter	*)
					 -> int			(* seperator		*)
					 -> 'a list


