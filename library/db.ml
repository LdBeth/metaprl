
open Term
open Basic
open Filename
open MathBus
open Mbterm
open Unix
open Ascii_scan

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


(*db ascii*)
(*
let ascii_special_header = "%"
let ash_length = String.length (ascii_special_header)

let is_first_char char string =
  (String.get string 0) = char

let level_expression_escape_string = "\\ \n\t\'[]"

let scan_level_expression scanner =
  let le = ref (mk_const_level_exp 0) in
  let rec scan_expression s = 
    if (scan_at_byte_p s ilsquare) then
      (scan_delimited_list scan_expression s ilsquare irsquare ibar;
       scan_whitespace)
    else if numeric_digit_code_p (scan_cur_byte s) then 
      le := max_level_exp (mk_const_level_exp (scan_decimal_num s)) le
    else (let v = scan_string s in
    scan_whitespace; 
    le := max_level_exp (mk_var_level_exp v) le) in 
  scan_expression scanner;
  le

let mk_real_param_from_strings value ptype =
  match ptype with "n" -> (Number (Num.num_of_string value))
  | "time" -> (ParmList [(make_param (String "time"));
			  (make_param (Number (Num.num_of_string value)))])
  | "t" -> (Token value)
  | "s" -> (String value)
  | "q" -> (ParmList [(make_param (String "quote")); (make_param (String value))])
  | "b" -> (ParmList [(make_param (String "bool")); (make_param (Number (Num.num_of_string value)))])
  | "v" -> (Var value)
  | "o" -> let term = string_to_term value in
    (ObId (stamp_to_object_id (term_to_stamp term)))
  | "l" -> let level = 
      scan_level_expression (make_scanner level_expression_escape_string (stream_of_string value)) in 
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
 
let string_to_bindings value = 
  let l = String.length value in
  if l > ash_length then 
    let v = String.sub value 0 ash_length in
    let l'= String.length v in 
    (if v = ascii_special_header then 
      let c = String.sub v 0 1 and v' = String.sub v 1 (l' - 1) in
      match c with 
	"A" -> ["nuprl5_implementation3"; "extended"; "meta"; v']
      | "D" -> ["nuprl5_implementation3"; "extended"; "meta"; v']
      | "S" -> ["nuprl5_implementation2"; "extended"; v']
      | "d" -> ["nuprl5_implementation2"; "display"; v']
      | "a" -> ["nuprl5_implementation1"; v']
      | "%" -> [v']
      | t -> failwith "unknown special binding"
    else [value])
  else [value]


let string_to_parameter value ptype =
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
			    (make_param (String ptype));
			  ])
      | "a" ->  (mk_meta_param_from_strings value ptype)
      | "%" ->  (mk_real_param_from_strings v' ptype)
      | t -> failwith "unknown special op-param"
    else (mk_real_param_from_strings value ptype))
    in make_param pv
  else make_param (mk_real_param_from_strings value ptype)

*)
(*end ascii*)

(*
let db_lib_read stamp object_type =
  let sterm = stamp_to_term stamp and oterm = istring_term object_type in
   Orb.eval_args_to_term tid sterm [oterm]
*)
