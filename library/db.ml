
open Term
open Basic
open Filename
open MathBus
open Mbterm
open Unix
open Ascii_scan



(* read_ascii_term *)

open Array
open List

open Opname

let mask_p mask code  = (((land) code mask) = mask)

let compression_code_p		= mask_p 0x80
let compression_add_byte_p	= mask_p 0xC0


let index_of_bytes b c = ((b land 0x0F) lsl 8) + c

let level_of_byte b = ((b land 0x30) lsr 4)

type catetype =   COpid  | CBinding | CParameter | COperator | CTerm

let catetypes = 
  let a = create 5 COpid in
    set a 0 COpid;
    set a 1 CBinding;
    set a 2 CParameter;
    set a 3 COperator;
    set a 4 CTerm;

    a


let type_of_byte b = get catetypes (b land 0x07)

type cate =
   Opid of string
 | Binding of string list
 | Parameter of param
 | Operator of operator
 | Term of term

type level =
	{ mutable items : cate array
	; mutable fill : int
	}

let level_array_growth = 256
let level_array_initial = (Opid "")

let new_level () = {items = (create level_array_growth level_array_initial); fill = 0 }

let level_add l itemf = 
  let fill = l.fill in
    (if fill = (Array.length l.items)
       then let nitems = create (fill + level_array_growth) level_array_initial in
         blit l.items 0 nitems 0 fill;
         l.items <- nitems
    );

    (* important that itemf not be called until after index allocated for item. *)
    l.fill = fill + 1;
    let item = (itemf ()) in
      set l.items fill item;
      item

  
let level_get l i = get l.items i

type lscanner = { scanner : scanner; mutable levels : level list
					; stb : string -> string list
					; stp : string -> string -> param
}

let new_lscanner scanner stb stp = { scanner = scanner; levels = []; stb = stb; stp = stp }

(* important that itemf not be called until after index allocated for item. *)
let rec levels_assign scanner code itemf =
 let levels = scanner.levels in
 let index = level_of_byte code in
  
  if (index >= (List.length levels)) 
     then ( scanner.levels <- flatten [ levels; [ new_level () ]]
	  ; levels_assign scanner code itemf)
     else level_add (nth levels index) itemf


let levels_lookup scanner level index =
 level_get (nth scanner.levels level) index
 

(* scanner includes levels *)

	 
let make_operator opid parameters =
  if opid = "!nuprl_light_implementation"
     then (mk_op (make_opname
			(map (function p -> 
				match dest_param p with
				 (String s) -> s 
				 |_ -> error ["read_term"; "operator"; "nuprl-light"; "opname"; "string"]
					     [] [])
			      (match dest_param (hd parameters) with
				ParmList pl -> pl
				 | _ -> error ["read_term"; "operator"; "nuprl-light"; "opname"] [] [])))
		(tl parameters))
     else mk_nuprl5_op ((make_param (String opid)) :: parameters)


let scan_binding scanner = (scanner.stb (scan_string scanner.scanner))


let rec scan_item stype scanner =
  match stype with
    COpid -> Opid (scan_string scanner.scanner)
  | CBinding -> Binding (scan_binding scanner)
  | CParameter -> Parameter (scan_parameter scanner)
  | COperator -> Operator (scan_operator scanner)
  | CTerm -> Term (scan_term scanner)

and scan_compressed code scanner =
 if (compression_add_byte_p code)
    then (scan_next scanner.scanner;
	 levels_assign scanner code (function () -> scan_item (type_of_byte code) scanner))
    else (scan_bump scanner.scanner;
	 let r = levels_lookup scanner
			       (level_of_byte code)
			       (index_of_bytes code (scan_cur_byte scanner.scanner)) in
	   scan_next scanner.scanner;
	   r)

and scan_parameter scanner =
 let code = (scan_cur_byte scanner.scanner) in
  if compression_code_p code
    then match (scan_compressed code scanner) with
	    Parameter p -> p
	|_ -> error ["read_term"; "parameter"] [] []
    else let s = (scan_string scanner.scanner) in
	  scan_byte scanner.scanner icolon;
	  scanner.stp s (scan_string scanner.scanner)

and scan_parameters scanner = 
  if scan_at_byte_p scanner.scanner ilcurly
     then scan_delimited_list scanner.scanner
			      (function () -> (scan_parameter scanner))
			      ilcurly ircurly icomma
     else []

and scan_operator scanner =
 let code = (scan_cur_byte scanner.scanner) in
  if compression_code_p code
    then match (scan_compressed code scanner) with
	    Operator op -> op
	|_ -> error ["read_term"; "operator"] [] []
    else (make_operator (scan_string scanner.scanner) (scan_parameters scanner))

and scan_bound_term scanner =
 let code = (scan_cur_byte scanner.scanner) in
  if compression_code_p code
    then (match (scan_compressed code scanner) with
	    Term term -> mk_bterm [] term
	  | Operator op -> mk_bterm [] (mk_term op (scan_bound_terms scanner))
	  | Opid opid ->
		(if scan_at_byte_p scanner.scanner ilcurly
		    then mk_bterm []
			 (mk_term (make_operator opid (scan_parameters scanner))
				  (scan_bound_terms scanner))
		 else if (scan_at_byte_p scanner.scanner ilparen)
		    then mk_bterm [] (mk_term (make_operator opid [])
						   (scan_bound_terms scanner))
		 else error ["read_term"; "bound term"; "opid"] [] [])
	  | Binding binding ->
		(if scan_at_byte_p scanner.scanner icomma
		    then mk_bterm (flatten (binding :: (scan_delimited_list
							scanner.scanner
							(function () -> (scan_binding scanner))
							icomma idot icomma)))
			          (scan_term scanner)
		 else if (scan_at_byte_p scanner.scanner idot)
		    then mk_bterm binding (scan_next scanner.scanner; scan_term scanner)
		 else error ["read_term"; "bound term"; "binding"] [] [])
	  |_ -> error ["read_term"; "bound term"] [] [])
    else if (scan_at_byte_p scanner.scanner idot)
	then (scan_next scanner.scanner; mk_bterm [""] (scan_term scanner))
    else let s = (scan_string scanner.scanner) in
	  (* should be match (scan_cur_byte scanner.scanner) with ... *)
	  if (scan_at_byte_p scanner.scanner icomma)
		then mk_bterm (flatten ((scanner.stb s)
					  :: (scan_delimited_list
						scanner.scanner
						(function () -> (scan_binding scanner))
						icomma idot icomma)))
				(scan_term scanner)
	  else if (scan_at_byte_p scanner.scanner idot)
		then ((scan_next scanner.scanner);
			 mk_bterm (scanner.stb s) (scan_term scanner))
	
	  else if (scan_at_byte_p scanner.scanner ilcurly)
		then mk_bterm [] (mk_term (make_operator s (scan_parameters scanner))
				  (scan_bound_terms scanner))
	  else if (scan_at_byte_p scanner.scanner ilparen)
		then mk_bterm [] (mk_term (make_operator s [])
				  (scan_bound_terms scanner))
	  else error ["read_term"; "bound term"; "lost"] [] []

and scan_bound_terms scanner = 
  if scan_at_byte_p scanner.scanner ilparen
     then scan_delimited_list scanner.scanner
			      (function () -> scan_bound_term scanner)
			      ilparen irparen isemicolon
     else []

and scan_term scanner =
 let code = scan_cur_byte scanner.scanner in
   if (compression_code_p code)
      then match (scan_compressed code scanner) with
	      Opid s ->		mk_term (make_operator s (scan_parameters scanner)) 
					(scan_bound_terms scanner)
	    | Operator op ->	mk_term op (scan_bound_terms scanner)
	    | Term term ->	term
	    |_ -> error ["read_term"; "term"] [] []
      else mk_term (scan_operator scanner) (scan_bound_terms scanner)
  


(*let read_static_level scanner 
 let ilevel = read_term scanner in
 le)
*)

let read_term_aux scanner stb stp =
 scan_term (new_lscanner scanner stb stp)


let string_to_trivial_term s stp = 
  read_term_aux
	(make_scanner "\\ \n\r\t()[]{}:;.," "\n\t\r " (Stream.of_string s))
	(function s -> error ["string_to_term"; "string_to_binding"] [] [])
	stp

(* db *)



type dbtable = (stamp * string, term) Hashtbl.t

let db_cache = (Hashtbl.create 7:dbtable)
let master_pathname = ref ""

let db_init master =
  let name = if String.get master (String.length master - 1) = '/' then master
  else String.concat "" [master; "/"] in
  master_pathname := name

 (*let {process_id = pid} = dest_stamp stamp in
 process_pathname := String.concat "" [name ; pid];
 mkdir !process_pathname 999*)

(*let db_query string =*)

let db_read stamp object_type =
  let {process_id = pid; seq = seq}  = dest_stamp stamp in
  let filename = String.concat ""
      [!master_pathname; "/"; pid; "/"; (string_of_int seq); "."; object_type] in
  let in_channel = try open_in filename with
    Sys_error e -> error (filename :: ["db_read"; "file"; "not"; "exist"]) [] [] in
  let term = term_of_mbterm (read_node in_channel) in
  close_in in_channel;
  Hashtbl.add db_cache (stamp, object_type) term;
  term

let db_write stamp object_type term =
  let {process_id = pid; seq = seq} = dest_stamp stamp in
  let filename = String.concat ""
      [!master_pathname; "/"; pid; (string_of_int seq); "."; object_type] in
  let descr = openfile filename [O_EXCL; O_WRONLY; O_CREAT] 999 in
  (write_node (mbterm_of_term term) (out_channel_of_descr descr));
  close descr


(* start db ascii*)

let ascii_special_header = "%"
let ash_length = String.length (ascii_special_header)

let is_first_char char string =
  (String.get string 0) = char

let level_expression_escape_string = "[ |']"

let incr_level_exp_n i le = 
   let { le_const = c; le_vars = vars } = dest_level le in
      let add1 lv = 
        let { le_var = v; le_offset = o } = dest_level_var lv in
            if o = 0 then mk_level_var v (o + i) else lv
      in
         mk_level (max i c) (List.map add1 vars)

let scan_level_expression scanner =
  let le = ref (mk_const_level_exp 0) in
  let rec scan_numbers s = 
    if (scan_whitespace s; scan_at_char_p s '\'') then 
      (scan_next s;
       le := incr_level_exp !le;
       scan_numbers s;
       s)
    else if (scan_whitespace s; scan_at_digit_p s) then
      (le := incr_level_exp_n (Num.int_of_num (scan_num s)) !le;
       scan_numbers s;
       s) 
    else s in
  let rec scan_atom s = 
     let scan_expression_q () = scan_expression s in
     if (scan_whitespace s; scan_at_byte_p s ilsquare) then
      (scan_char_delimited_list s scan_expression_q '[' ']' '|';
       scan_whitespace s; ())
    else if (scan_whitespace s; scan_at_digit_p s) then 
      (le := max_level_exp (mk_const_level_exp (Num.int_of_num (scan_num s))) !le; ())
    else (let v = scan_string s in
    scan_whitespace s; 
    le := max_level_exp (mk_var_level_exp v) !le); s 
   and scan_expression s2 = 
    scan_numbers (scan_atom s2);
    s2
  in scan_expression scanner; 
  !le

let make_le_scanner = make_scanner level_expression_escape_string "\n\t\r "

let mk_real_param_from_strings stp value ptype =
  match ptype with "n" -> (Number (Num.num_of_string value))
  | "time" -> (ParmList [(make_param (String "time"));
			  (make_param (Number (Num.num_of_string value)))])
  | "t" -> (Token value)
  | "s" -> (String value)
  | "q" -> (ParmList [(make_param (String "quote")); (make_param (String value))])
  | "b" -> (ParmList [(make_param (String "bool")); (make_param (Number (Num.num_of_string value)))])
  | "v" -> (Var value)
  | "o" -> let term = string_to_trivial_term value stp in
    (ObId (stamp_to_object_id (term_to_stamp term)))
  | "l" -> let level = 
      scan_level_expression (make_le_scanner (Stream.of_string value)) in 
    (ParmList [(make_param (Level level)); (make_param (String value))])
  | t -> failwith "unknown special op-param"
 
let mk_meta_param_from_strings value ptype =
  match ptype with "n" -> (MNumber value)
  | "t" -> (MToken value)
  | "s" -> (MString value)
  | "q" -> (ParmList [(make_param (String "quote")); (make_param (String value))])
  | "b" -> (ParmList [(make_param (String "bool")); (make_param (Number (Num.num_of_string value)))])
  | "v" -> (MVar value)
  | "l" -> (MLevel value)
  |  t -> failwith "unknown special meta op-param"
 
let extract_binding3 pl = 
  match (List.map dest_param pl) with
  (String "extended")::((String m)::((String v)::tl)) -> ["extended"; m; v]
 | t  -> failwith "extract binding 3"

let extract_binding2 pl = 
  match (List.map dest_param pl) with
  (String "extended")::((String v)::tl) -> ["extended"; v]
 |(String "display")::((String v)::tl) -> ["display"; v]
 | t  -> failwith "extract binding 2"

let extract_binding1 pl = 
  match (List.map dest_param pl) with
  (String v)::tl -> [v]
 | t  -> failwith "extract binding 1"

let string_to_bindings value = 
  let l = String.length value in
  if l > ash_length then 
    let v = String.sub value 0 ash_length in
    let l'= String.length v in 
    (if v = ascii_special_header then 
      let c = String.sub v 0 1 and v' = String.sub v 1 (l' - 1) in
      match c with 
	"A" -> ["nuprl5_implementation3"; "extended"; "abstraction"; v']
      | "D" -> ["nuprl5_implementation3"; "extended"; "display"; v']
      | "S" -> ["nuprl5_implementation2"; "extended"; v']
      | "d" -> ["nuprl5_implementation2"; "display"; v']
      | "a" -> ["nuprl5_implementation1"; v']
      | "%" -> [v']
      | t -> failwith "unknown special binding"
    else [value])
  else [value]


let rec string_to_parameter value ptype =
 let l = String.length value in
  if ash_length < l then 
    let v = String.sub value 0 ash_length in
    let l'= String.length v in 
    let pv = (if v = ascii_special_header then 
      let c = String.sub v 0 1 and v' = String.sub v 1 (l' - 1) in
      match c with 
	"A" -> (ParmList [(make_param (String "extended"));
			   (make_param (String "slot"));
			   (make_param (String ptype)); 
			   (make_param (Token v'))] (*type-of-meta-variable-id v'*)
		  )
      | "D" ->  (ParmList [(make_param (String "extended"));
			    (make_param (String "slot"));
			    (make_param (String ptype)); 
			    (make_param (Token v'))])
      | "S" ->  (ParmList [(make_param (String "extended"));
			    (make_param (String "slot"));
			    (make_param (String ptype))
			  ])
      | "d" ->  (ParmList [(make_param (String "display"));
			    (make_param (String "slot"));
			    (make_param (String ptype))
			  ])
      | "a" ->  (mk_meta_param_from_strings value ptype)
      | "%" ->  (mk_real_param_from_strings string_to_parameter v' ptype)
      | t -> failwith "unknown special op-param"
    else (mk_real_param_from_strings string_to_parameter value ptype))
    in make_param pv
  else make_param (mk_real_param_from_strings string_to_parameter  value ptype)

(* end db ascii*)


let read_term stream =
 read_term_aux
 	(make_scanner "\\ \n\r\t()[]{}:;.," "\n\t\r " stream)
	string_to_bindings
	string_to_parameter

 
let db_read_ascii stamp object_type =
  let {process_id = pid; seq = seq}  = dest_stamp stamp in
  let filename = String.concat ""
      [!master_pathname; "/"; pid; "/"; (string_of_int seq); "."; object_type] in
  let in_channel = try open_in filename with
    Sys_error e -> error (filename :: ["db_read"; "file"; "not"; "exist"]) [] [] in
  let term = read_term (Stream.of_channel in_channel) in
  close_in in_channel;
  term

(*
let db_lib_read stamp object_type =
  let sterm = stamp_to_term stamp and oterm = istring_term object_type in
   Orb.eval_args_to_term tid sterm [oterm]
*)


