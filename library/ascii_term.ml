
open Array
open List

open Opname
open Term

open Basic
open Ascii_scan

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

type lscanner = { scanner : scanner; mutable levels : level list}

let new_lscanner scanner = { scanner = scanner; levels = [] }

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


let scan_binding scanner = (string_to_bindings (scan_string scanner.scanner))


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
	  string_to_parameter s (scan_string scanner.scanner)

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
		then mk_bterm (flatten ((string_to_bindings s)
					  :: (scan_delimited_list
						scanner.scanner
						(function () -> (scan_binding scanner))
						icomma idot icomma)))
				(scan_term scanner)
	  else if (scan_at_byte_p scanner.scanner idot)
		then ((scan_next scanner.scanner);
			 mk_bterm (string_to_bindings s) (scan_term scanner))
	
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
 le
*)

let read_term scanner =
 scan_term (new_lscanner scanner)
