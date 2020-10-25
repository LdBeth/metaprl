(*
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/htmlman/default.html or visit http://metaprl.org/
 * for more information.
 *
 * Copyright (C) 1998 Lori Lorigo, Richard Eaton, Cornell University
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * Authors: Lori Lorigo, Richard Eaton
 *)
open Lm_debug
open Lm_symbol
open Lm_num

open Term_sig
open Refiner.Refiner.Term
open Refiner.Refiner.TermType
open Refiner.Refiner.TermMan
open Basic
open MathBus
open Mbterm
open Unix
open Ascii_scan

(* read_ascii_term *)

open Array
open List

open Opname

(************************************************************************
 * Compatibility layer for abstract vars.
 *)
let mk_bterm bvars t =
   mk_bterm (List.map Lm_symbol.add bvars) t

let mk_var_level_exp v =
   mk_var_level_exp (Lm_symbol.add v)

(************************************************************************
 * Original code
 *)

let _ =
   show_loading "Loading Db%t"

let mask_p mask code  = (((land) code mask) = mask)

let compression_code_p                = mask_p 0x80
let compression_add_byte_p        = mask_p 0xC0

let make_term_scanner = make_scanner "\\ \n\r\t()[]{}:;.," "\n\t\r "

let myscanner = ref (make_term_scanner (Stream.of_string "lori"))

let index_of_bytes b c = ((b land 0x0F) lsl 8) + c

let level_of_byte b = ((b land 0x30) lsr 4)

type catetype =   COpid  | CBinding | CParameter | COperator | CTerm | CNumeral

let catetypes =
  let a = make 6 COpid in
    set a 0 COpid;
    set a 1 CBinding;
    set a 2 CParameter;
    set a 3 COperator;
    set a 4 CTerm;
    set a 5 CNumeral;
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

let new_level n = {items = (make n level_array_initial); fill = 0 }

let level_allocate_slot l =
  let fill = l.fill in
    (if inteq fill (Array.length l.items)
       then let nitems = make (fill + level_array_growth) level_array_initial in
         blit l.items 0 nitems 0 fill;
         l.items <- nitems
    );

        (* print_string " fill "; print_string (string_of_int fill); print_newline(); *)

    (* important that itemf not be called until after index allocated for item. *)
    l.fill <- fill + 1;
    fill

let level_add l itemf =
 let index = (level_allocate_slot l) in
 let item = (itemf ()) in
   set l.items index item;
      item

let level_assign l item =
  (*
  (match item with
   Parameter p -> print_string "Parameter"
   | Operator p -> print_string "Operator"
   | Term p -> print_string "Term"
   | Binding p -> print_string "Binding"
   | Opid p -> print_string "Opid");
  *)
  set l.items (level_allocate_slot l) item

let level_get l i = get l.items i

type lscanner = { scanner : scanner
                ; mutable levels : level list
                ; stb : string -> string list
                ; stp : string -> string -> param
                }

let new_lscanner scanner stb stp = { scanner = scanner; levels = []; stb = stb; stp = stp }

let add_new_level_aux n scanner = scanner.levels <- flatten [ scanner.levels; [ new_level n ]]
let add_new_level = add_new_level_aux level_array_growth

(* important that itemf not be called until after index allocated for item. *)
let rec levels_assign scanner code itemf =
 let levels = scanner.levels in
 let index = level_of_byte code in

  (* print_string ("Adding to level "); print_string (string_of_int code); *)

  if (index >= (List.length levels))
     then ( add_new_level scanner
          ; levels_assign scanner code itemf)
     else level_add (nth levels index) itemf

let levels_lookup scanner level index =
 level_get (nth scanner.levels level) index

(* scanner includes levels *)
let token s = Token (mk_opname s nil_opname)
let dst_token s = fst (dst_opname s)

let make_operator opid parameters =
  if stringeq opid "!metaprl_implementation"
     then (mk_op (make_opname
                        (map (function p ->
                                match dest_param p with
                                 (String s) -> s
                                 |_ -> error ["read_term"; "operator"; "nuprl-light"; "opname"; "string"]
                                             [] [])
                              (match dest_param (hd parameters) with
                                ParamList pl -> pl
                                 | _ -> error ["read_term"; "operator"; "nuprl-light"; "opname"] [] [])))
                (tl parameters))
     else Nuprl5.mk_nuprl5_op ((make_param (token opid)) :: parameters)

let rec scan_item stype scanner =
  match stype with
    COpid -> Opid (scan_string scanner.scanner)
  | CBinding -> (* print_string " scan compressed binding "; *) Binding (scan_binding scanner)
  | CParameter -> Parameter (scan_parameter scanner)
  | COperator -> Operator (scan_operator scanner)
  | CTerm -> Term (scan_term scanner)
  | CNumeral -> Parameter (scan_numeral_parameter scanner)

and scan_numeral_parameter scanner =

 (*error ["break"] [] []; *)
 let mp256 = num_of_int 256 and mp0 = num_of_int 0 and mp1 = num_of_int 1 in

 let code = scan_cur_byte scanner.scanner in
 (*print_string " code = "; print_string (string_of_int code);*)
  let rec aux i mpexp acc =

   if i = 0
      then acc
   else (scan_bump scanner.scanner;
         let c = scan_cur_byte scanner.scanner in
         (
         aux (i - 1) (mult_num mpexp mp256)
               (add_num acc (mult_num mpexp (num_of_int c)))))

  in

  let value = aux code mp1 mp0 in

             (* print_string " scanned numeral ";
         print_string (string_of_num value);
         scan_whitespace !myscanner ;
         scan_bump scanner.scanner;
         scan_whitespace !myscanner ;
         print_string " scanned bump ";*)

         scan_next scanner.scanner;
         scan_byte scanner.scanner icolon;

          (*let c = scan_cur_byte scanner.scanner in
          print_string " code = "; print_string (string_of_int c);
          scan_whitespace !myscanner ;
          if scan_at_byte_p scanner.scanner icolon then scan_bump scanner.scanner;
         scan_whitespace !myscanner ;*)

        myscanner := scanner.scanner;
     let ptype = scan_string scanner.scanner in
      match ptype with "n" -> make_param (Number value)
      | "time" -> make_param (ParamList [(make_param (token "time"));
                          (make_param (Number value))])
      | "ime" -> make_param (ParamList [(make_param (token "time"));
                          (make_param (Number value))])
      | _ -> error ["scan_numeral_parameter"; ptype] [] []

(* unused
and scan_numeral_parameter_old scanner =
 let l = scan_cur_byte scanner.scanner in
 let mp256 = num_of_int 256 in

  let rec aux i acc =
   scan_next scanner.scanner;
   if i = 0
      then acc
      else aux (i - 1) (add_num (mult_num acc mp256) (num_of_int (scan_cur_byte scanner.scanner)))
   in

  let n = aux l mp256 in
    scan_byte scanner.scanner icolon;
    let ptype = (scan_string scanner.scanner) in
      match ptype with "n" -> make_param (Number n)
      | "time" -> make_param (ParamList [(make_param (token "time"));
                          (make_param (Number n))])
      | _ -> error ["scan_numeral_parameter_old"; ptype] [] []

and scan_compressed_new code scanner =
 if (compression_add_byte_p code)
    then let ctype = (type_of_byte code) in
          scan_next scanner.scanner;
          levels_assign scanner code (function () -> scan_item ctype scanner)
    else (scan_bump scanner.scanner;
         (* print_string " scan compressed index "; *)
         let r = levels_lookup scanner
                               (level_of_byte code)
                               (index_of_bytes code (scan_cur_byte scanner.scanner)) in
           scan_next scanner.scanner;
           r)
*)

and scan_compressed code scanner =
 if compression_add_byte_p code
    then let ctype = type_of_byte code in
          scan_next scanner.scanner;
          if ctype = CNumeral
            then (let p = Parameter (scan_numeral_parameter scanner) in p)
            else ((* print_string " scan compressed add "; *)
                  levels_assign scanner code (function () -> scan_item ctype scanner))
    else (scan_bump scanner.scanner;
         let r = levels_lookup scanner
                               (level_of_byte code)
                               (index_of_bytes code (scan_cur_byte scanner.scanner)) in
           scan_next scanner.scanner;
           r)

and scan_binding scanner =
 let code = scan_cur_byte scanner.scanner in
  if compression_code_p code
    then match (scan_compressed code scanner) with
            Binding sl  -> sl
          |_ -> error ["read_term"; "binding"] [] []
    else scanner.stb (scan_string scanner.scanner)

and scan_parameter scanner =
 let code = scan_cur_byte scanner.scanner in
  (* print_string "code = "; print_string (string_of_int code); *)
  if compression_code_p code
    then match (scan_compressed code scanner) with
        Parameter p -> p
        | item ->

  ((match item with
   Parameter p -> print_string "Parameter"
   | Operator p -> print_string "Operator"
   | Term p -> print_string "Term"
   | Binding p -> print_string "Binding"
   | Opid p -> print_string p; print_string "Opid");

   error ["scan_parameter"; "not"] [] []
   )

  else let s = scan_string scanner.scanner in
          scan_byte scanner.scanner icolon;
          (*myscanner := scanner.scanner;*)
          scanner.stp s (scan_string scanner.scanner)

and scan_parameters scanner =
  if scan_at_byte_p scanner.scanner ilcurly
     then scan_delimited_list scanner.scanner
                              (function () -> (scan_parameter scanner))
                              ilcurly ircurly icomma
     else []

and scan_operator scanner =
 (* print_string " sop "; *)
 let code = scan_cur_byte scanner.scanner in
  if compression_code_p code
    then match (scan_compressed code scanner) with
            Operator op -> op
          | Opid s  -> (make_operator s (scan_parameters scanner))
          |_ -> error ["read_term"; "operator"] [] []

        (*
        | Binding sl  -> error (flatten [["read_term"; "operator"; "binding"]; sl]) [] []
        | Parameter p -> error ["read_term"; "operator"; "op"] [] [mk_term (make_operator "fu" [p])[]]
        | Term t  -> error ["read_term"; "operator"; "term"] [] [t]
        *)

    else let opid = scan_string scanner.scanner in
          let parms = scan_parameters scanner in
            (make_operator opid parms)

and scan_bound_term scanner =
 let code = (scan_cur_byte scanner.scanner) in
  if compression_code_p code
    then (match (scan_compressed code scanner) with
            Term term -> mk_bterm [] term
          | Operator op -> mk_bterm [] (mk_term op (scan_bound_terms scanner))
          | Opid opid ->
                (if scan_at_byte_p scanner.scanner ilcurly
                    then mk_bterm []
                          (let op = (make_operator opid (scan_parameters scanner)) in
                             mk_term op (scan_bound_terms scanner))
                 else if (scan_at_byte_p scanner.scanner ilparen)
                    then mk_bterm [] (mk_term (make_operator opid [])
                                                   (scan_bound_terms scanner))
                 else error ["read_term"; "bound term"; "opid"] [] [])
          | Binding binding ->
                (if scan_at_byte_p scanner.scanner icomma
                    then
                        let bindings = (flatten (binding :: (scan_delimited_list
                                                        scanner.scanner
                                                        (function () -> (scan_binding scanner))
                                                        icomma idot icomma)))
                          in mk_bterm bindings (scan_term scanner)
                 else if (scan_at_byte_p scanner.scanner idot)
                    then mk_bterm binding (scan_next scanner.scanner; scan_term scanner)
                 else error ["read_term"; "bound term"; "binding"] [] [])
          |_ -> error ["read_term"; "bound term"] [] [])
    else if (scan_at_byte_p scanner.scanner idot)
        then (scan_next scanner.scanner; mk_bterm [""] (scan_term scanner))
    else let s = (scan_string scanner.scanner) in
          (* should be match (scan_cur_byte scanner.scanner) with ... *)
          if (scan_at_byte_p scanner.scanner icomma)
                then let bindings = (flatten ((scanner.stb s)
                                          :: (scan_delimited_list
                                                scanner.scanner
                                                (function () -> (scan_binding scanner))
                                                icomma idot icomma))) in
                        mk_bterm bindings (scan_term scanner)
          else if (scan_at_byte_p scanner.scanner idot)
                then ((scan_next scanner.scanner);
                         mk_bterm (scanner.stb s) (scan_term scanner))

          else if (scan_at_byte_p scanner.scanner ilcurly)
                then mk_bterm []
                        (let op = (make_operator s (scan_parameters scanner)) in
                         let bterms = (scan_bound_terms scanner) in
                           (mk_term op bterms))

          else if (scan_at_byte_p scanner.scanner ilparen)
                then mk_bterm [] (mk_term (make_operator s [])
                                  (scan_bound_terms scanner))
          else error ["read_term"; "bound term"; "lost"] [] []

and scan_bound_terms scanner =
  (* print_string " sbt "; *)
  if scan_at_byte_p scanner.scanner ilparen
     then scan_delimited_list scanner.scanner
                              (function () -> scan_bound_term scanner)
                              ilparen irparen isemicolon
     else []

and scan_term scanner =
 (* print_string " st "; *)

 let code = scan_cur_byte scanner.scanner in
   if compression_code_p code
      then match (scan_compressed code scanner) with
              Opid s ->                let op = (make_operator s (scan_parameters scanner)) in
                                  mk_term op (scan_bound_terms scanner)
            | Operator op ->        mk_term op (scan_bound_terms scanner)
            | Term term ->        term
            |_ -> error ["read_term"; "term"] [] []
      else let op = scan_operator scanner in
                let bterms = (scan_bound_terms scanner) in
                        mk_term op bterms

let read_term_aux scanner stb stp =
 scan_term (new_lscanner scanner stb stp)

let string_to_trivial_term s stp =
  read_term_aux
        (make_term_scanner (Stream.of_string s))
        (function s -> error ["string_to_term"; "string_to_binding"] [] [])
        stp

(* db *)

type dbtable = (stamp * string, term) Hashtbl.t

let db_cache = (Hashtbl.create 7:dbtable)
let master_pathname = ref ""

let asciip = ref true

(*let db_query string =*)

let with_open_db_file f name ext =
  let filename = String.concat ""
      [!master_pathname; name; "."; ext] in
  let in_channel = try open_in filename with
    Sys_error e -> error (filename :: ["db_open"; name; ext]) [] [] in

    let x = (try (f in_channel) with e -> close_in in_channel; raise e) in
        close_in in_channel;
        x

let with_open_file f stamp otype =
  let {process_id = pid; seq = seq; _}  = dest_stamp stamp in
  let filename = String.concat ""
      [!master_pathname; pid; "/"; "data"; "/"; (string_of_int seq); "."; otype] in
  let in_channel = try open_in filename with
    Sys_error e -> error (filename :: ["db_read"; "file"; "not"; "exist"]) [] [] in

    let x = (try (f in_channel) with e -> close_in in_channel; raise e) in
        close_in in_channel;
        x

let db_read_aux =
  with_open_file
   (function in_channel -> term_of_mbterm (read_node in_channel))

(* start db ascii*)

let ascii_special_header = "%"
let ash_length = String.length (ascii_special_header)

(* unused
let is_first_char char string = chareq (String.get string 0) char
*)

let level_expression_escape_string = "[ |']"

let incr_level_exp_n i le =
   let { le_const = c; le_vars = vars } = dest_level le in
      let add1 lv =
        let { le_var = v; le_offset = o } = dest_level_var lv in
            if (inteq o 0) then mk_level_var v (o + i) else lv
      in
         mk_level (max i c) (List.map add1 vars)

let scan_level_expression scanner =
  let le = ref (mk_const_level_exp 0) in
  let rec scan_numbers s =
    if (scan_whitespace s; scan_at_char_p s '\'') then
      (scan_next s;
       le := incr_level_exp !le;
       scan_numbers s)
    else if (scan_whitespace s; scan_at_digit_p s) then
      (le := incr_level_exp_n (Lm_num.int_of_num (scan_num s)) !le;
       scan_numbers s)
  in
  let rec scan_atom s =
     let scan_expression_q () = scan_expression s in
     if (scan_whitespace s; scan_at_byte_p s ilsquare) then
      let _ = scan_char_delimited_list s scan_expression_q '[' ']' '|'
      in scan_whitespace s
    else if (scan_whitespace s; scan_at_digit_p s) then
      (le := max_level_exp (mk_const_level_exp (Lm_num.int_of_num (scan_num s))) !le 0; ())
    else (let v = scan_string s in
    scan_whitespace s;
    le := max_level_exp (mk_var_level_exp v) !le 0); s
   and scan_expression s2 =
    scan_numbers (scan_atom s2);
    s2
  in
    let _ = scan_expression scanner
    in !le

let make_le_scanner = make_scanner level_expression_escape_string "\n\t\r "

let mk_real_param_from_strings stp value ptype =
  match ptype with
    "n" -> (Number (Lm_num.num_of_string value))
  | "time" -> (ParamList [(make_param (token "time"));
                          (make_param (Number (Lm_num.num_of_string value)))])
  | "t" -> (token value)
  | "s" -> (String value)
  | "q" -> (ParamList [(make_param (token "quote")); (make_param (token value))])
  | "b" -> ( ParamList [ (make_param (token "bool"))
                        ; if (stringeq value "false") || (stringeq value "F") then
                          make_param (Number (Lm_num.num_of_int 0))
                          else if (stringeq value "true") || (stringeq value "T") then
                          make_param (Number (Lm_num.num_of_int 1))
                          else error ["real_parameter_from_string"; value] [] []
                      ])
  | "v" -> (Var (Lm_symbol.add value))
  | "l" -> let level = scan_level_expression (make_le_scanner (Stream.of_string value)) in
    (ParamList [(make_param (token "nuprl5_level_expression")); (make_param (MLevel level)); (make_param (String value))])
  | "oid" -> let term = string_to_trivial_term value stp in
    (ObId (stamp_to_object_id (term_to_stamp term)))
  | "o" -> let term = string_to_trivial_term value stp in
    (ObId (stamp_to_object_id (term_to_stamp term)))
  | t -> failwith (String.concat "  " ["unknown special op-param"; ptype; value])

let mk_meta_param_from_strings value ptype =
  match ptype with
    "n" -> (MNumber (Lm_symbol.add value))
  | "t" -> (MToken (Lm_symbol.add value))
  | "s" -> (MString (Lm_symbol.add value))
  | "q" -> (ParamList [(make_param (token "quote")); (make_param (token value))])
  | "b" -> (ParamList [(make_param (token "bool")); (make_param (Number (Lm_num.num_of_string value)))])
  | "v" -> (Var (Lm_symbol.add value))
  | "l" -> let level =
      scan_level_expression (make_le_scanner (Stream.of_string value)) in
    (ParamList [(make_param (token "nuprl5_level_expression")); (make_param (MLevel level)); (make_param (String value))])
  |  t -> failwith "unknown special meta op-param"

let extract_binding3 pl =
  match pl with
  (Token ext)::((Token m)::((Token v)::tl)) when dst_token ext = "extended" -> ["extended"; dst_token m; dst_token v]
 | t  -> failwith "extract binding 3"

let extract_binding2 pl =
  match pl with
  (Token ext)::((Token v)::tl) when dst_token ext = "extended" -> ["extended"; dst_token v]
 |(Token disp)::((String v)::tl) when dst_token disp = "display" -> ["display"; v]
 | t  -> failwith "extract binding 2"

(* unused
let extract_binding1 pl =
  match pl with
  (String v)::tl -> [v]
 | t  -> failwith "extract binding 1"
*)

let string_to_bindings value =

  let l = String.length value in
  if l > ash_length then
    let v = Lm_string_util.sub "Db.string_to_bindings" value 0 ash_length in
    (if stringeq v ascii_special_header then
      let c = Lm_string_util.sub "Db.string_to_bindings" value 1 1 and v' = Lm_string_util.sub "Db.string_to_bindings" value 2 (l - 2) in
      match c with
        "A" -> ["nuprl5_implementation3"; "extended"; "abstraction"; v']
      | "D" -> ["nuprl5_implementation3"; "extended"; "display"; v']
      | "S" -> ["nuprl5_implementation2"; "extended"; v']
      | "d" -> ["nuprl5_implementation2"; "display"; v']
      | "a" -> ["nuprl5_implementation1"; v']
      | "%" -> [(Lm_string_util.sub "Db.string_to_bindings" value 1 (l - 1)) ]
      | t -> failwith "unknown special binding"
    else [value])
  else [value]

let rec string_to_parameter s ptype =
  (*if s = "!stamp" then failwith "stamp";*)
  let len = String.length s in
    if (len < 2 || not (chareq '%' (Lm_string_util.get "Db.string_to_parameter" s 0)))
     then make_param (mk_real_param_from_strings string_to_parameter s ptype)
    else
     let ss = (Lm_string_util.sub "Db.string_to_parameter" s 2 (len -2)) in
      make_param
       (match (Lm_string_util.get "Db.string_to_parameter" s 1) with
          '%' -> (mk_real_param_from_strings string_to_parameter (Lm_string_util.sub "Db.string_to_parameter" s 1 (len - 1)) ptype)
        | 'A' -> (ParamList   [ make_param (token "extended")
                              ; make_param (token "abstraction")
                              ; make_param (token ptype)
                              ; make_param (token ss)
                              ])
        | 'D' ->  (ParamList  [ make_param (token "extended")
                              ; make_param (token "display")
                              ; make_param (token ptype)
                              ; make_param (token ss)
                              ])
        | 'S' ->  (ParamList  [ make_param (token "extended")
                              ; make_param (token "slot")
                              ; make_param (token ptype)
                              ])
        | 'd' ->  (ParamList  [ make_param (token "display")
                              ; (make_param (mk_meta_param_from_strings ss ptype))
                              ])
        | 'a' ->  (mk_meta_param_from_strings ss ptype)
        | _ -> error ["string_to_parameter"; s; ptype][][])

let make_session_scanner stream =
  new_lscanner
        (make_term_scanner stream)
        string_to_bindings
        string_to_parameter

let extract_level_string_updates level inparms =
   let parms = ref (dest_params inparms) in
      while not (nullp !parms )
      do (match (hd !parms) with
             Token s ->
                let s = dst_token s in
                   (try
                       if stringeq s "nuprl5_implementation3"
                       then (level_assign level (Binding (extract_binding3 (tl !parms)))
                             ; parms := tl (tl (tl !parms)))
                       else if stringeq s "nuprl5_implementation2"
                       then (level_assign level (Binding (extract_binding2 (tl !parms)))
                             ; parms := tl (tl !parms))
                       else if stringeq s "nuprl5_implementation1"
                       then (level_assign level (Binding (extract_binding2 (tl !parms)))
                             ; parms := tl !parms)
                       else level_assign level (Opid s)
                    with _ -> level_assign level (Opid s))
           | Var s -> level_assign level (Binding [string_of_symbol s])
           | _ -> error ["level_read"; "strings"] [] [])
         ; parms := (tl !parms)
      done

let idata_persist_param = make_param (token "!data_persist")
(* unused
let idata_persist_inline_param = make_param (token "!data_persist_inline")
*)

let idata_persist_term_p t =
 match dest_term t with
   { term_op = op; term_terms = [istamp] }
      -> (match dest_op op with
           { op_name = opname; op_params = [id; ftype] } when parmeq id idata_persist_param
            -> true
         |_ -> false)
 |_ -> false

let stamp_of_idata_persist_term t =
 (* print_string "soipt "; *)
 match dest_term t with
   { term_op = op; term_terms = [istamp] }
      -> (match dest_op op with
           { op_name = opname; op_params = [id; ftype] } when parmeq id idata_persist_param
          -> term_to_stamp (term_of_unbound_term istamp)
     |_ -> error ["stamp_of_idata_persist_file"][][t])
   |_ -> error ["stamp_of_idata_persist_file"][][t]

let stamp_and_type_of_idata_persist_term t =
 match dest_term t with
   { term_op = op; term_terms = [istamp] }
      -> (match dest_op op with
           { op_name = opname; op_params = [id; ftype] } when parmeq id idata_persist_param
          -> ((term_to_stamp (term_of_unbound_term istamp)), dest_token_param ftype)
     |_ -> error ["stamp_and_type_of_idata_persist_file"][][t])
   |_ -> error ["stamp_and_type_of_idata_persist_file"][][t]

(* unused
let with_open_persist_file f t =
 match dest_term t with
   { term_op = op; term_terms = [istamp] }
      -> (match dest_op op with
           { op_name = opname; op_params = [id; ftype] } when parmeq id idata_persist_param

         -> with_open_file (function in_channel ->
                          f (make_session_scanner (Stream.of_channel in_channel)))
                (term_to_stamp (term_of_unbound_term istamp))
                (dest_token_param ftype)

     |_ -> error ["open_persist_file"][][t])
   |_ -> error ["open_persist_file"][][t]
*)

let with_open_pid_file f stamp otype =
  let {process_id = pid; seq = seq; _}  = dest_stamp stamp in
  let filename = String.concat ""
      [!master_pathname; pid; "/"; (string_of_int seq); "."; otype] in
  let in_channel = try open_in filename with
    Sys_error e -> error (filename :: ["db_read"; "file"; "not"; "exist"]) [] [] in

    let x = (try (f in_channel) with e -> close_in in_channel; raise e) in
        close_in in_channel;
        x

let with_open_static_file f t =
 match dest_term t with
   { term_op = op; term_terms = [istamp] }
      -> (match dest_op op with
           { op_name = opname; op_params = [id; ftype] } when parmeq id idata_persist_param

         -> with_open_pid_file (function in_channel ->
                          f (make_session_scanner (Stream.of_channel in_channel)))
                (term_to_stamp (term_of_unbound_term istamp))
                (dest_token_param ftype)

     |_ -> error ["open_static_file"][][t])
   |_ -> error ["open_static_file"][][t]

let index_of_il_term t =
  match dest_term t with
   { term_op = op; term_terms = _ }
   -> (match dest_op op with  { op_name = opname; op_params = [id; index] }
      -> (match (dest_param index) with
          Number n -> Lm_num.int_of_num n
          |_ -> error ["!l_term" ; "not"][][t])
      |_ -> error ["!l_term" ; "not"][][t])

let index_of_ilevel_term t =
  match dest_term t with
   { term_op = op; term_terms = _ }
   -> (match dest_op op with  { op_name = opname; op_params = [id; index; size] }
      -> (match (dest_param index) with
          Number n -> Lm_num.int_of_num n
          |_ -> error ["!level_term" ; "not"][][t])
      |_ -> error ["!level_term" ; "not"][][t])

let size_of_ilevel_term t =
 match dest_term t with
   { term_op = op; term_terms = _ }
   -> (match dest_op op with { op_name = opname; op_params = [id; index; size] }
   -> (match (dest_param size) with
         Number n -> Lm_num.int_of_num n
         |_ -> error ["!level_term" ; "not"][][t])
     |_ -> error ["!level_term" ; "not"][][t])

(*
 *        have assoc table of indices to levels and persist terms.
 *
 *        <loaded>        : {stamp; int; levels} list
 *
 *        <disk>                : {int; term(* persist-term *)} list
 *
 *)

let disk_levels = ref ([] : ((int * term) list))

let loaded_levels = ref ([] : (int * (stamp * level list)) list)

let disk_levels_assoc i =
 (assoc i !disk_levels)

let loaded_level_find_index i =
 snd (assoc i !loaded_levels)

let loaded_level_find_stamp s =
  let found = ref [] in
    if (exists (function ll ->
             let (i, (stamp, levels)) = ll in
                if (equal_stamps_p s stamp)
                   then (found := levels; true)
                   else false)
        !loaded_levels)
     then !found
     else raise Not_found

let loaded_levels_update index stamp levels =
 loaded_levels := (index, (stamp, levels)) :: !loaded_levels

let rec read_levels term index =
 (* print_string "read_levels "; Mbterm.print_term term; *)
 if idata_persist_term_p term
    then  let stamp = (stamp_of_idata_persist_term term) in
          try (loaded_level_find_stamp stamp)
          with _ -> let levels = read_static_level term in
                      (loaded_levels_update index stamp levels
                      ; levels)
   else (unconditional_error_handler
          (function () -> level_find (index_of_il_term term))
          (function t -> error ["read_levels"; "unknown"] [] [t; term]))

and level_find index =
    try (loaded_level_find_index index) with _ -> read_levels (disk_levels_assoc index) index

and read_static_level t = with_open_static_file read_static_level_aux t

and read_static_level_aux scanner =

 let ilevel = (session_read_term scanner) in
 let lindex = (index_of_ilevel_term ilevel) in

   add_new_level_aux (size_of_ilevel_term ilevel) scanner;

   let level = nth scanner.levels lindex in

     (* parameters_of_term will include the embedded nuprl5 opid as first parameter
        since this is a nuprl5_implementation term
     *)
     let _ = extract_level_string_updates level (tl (parameters_of_term (session_read_term scanner))) in

     (*        ditto *)
     List.iter (function p -> level_assign level (Parameter p))
         (tl (parameters_of_term (session_read_term scanner)));

     List.iter (function opt -> level_assign level (Operator (operator_of_term (term_of_unbound_term opt))))
         (bound_terms_of_term (session_read_term scanner));

     List.iter (function bterm -> level_assign level (Term (term_of_unbound_term bterm)))
         (bound_terms_of_term (session_read_term scanner));

    scanner.levels

and session_read_term scanner =

  if (scan_at_char_p scanner.scanner 'l')
    then (scan_char scanner.scanner 'l';
          scanner.levels <- read_levels (scan_term
                                              (* forget current levels *)
                                              (new_lscanner scanner.scanner scanner.stb scanner.stp))
                                        (-1);
            scan_char scanner.scanner 'l';
          session_read_term scanner )
  else if (scan_at_char_p scanner.scanner 't')
    then (scan_char scanner.scanner 't';
          let t = scan_term scanner in
           (* print_newline(); print_string " after scan term "; Mbterm.print_term t; *)
           scan_char scanner.scanner 't';
           t)
  else error ["session"; "read_term"; Char.escaped (scan_cur_char scanner.scanner)] [][]

let session_maybe_read_term scanner =
  if (scan_at_eof_p scanner.scanner) then None
  else Some (session_read_term scanner)

let db_read_ascii stamp otype =
 (* Mbterm.print_term (stamp_to_term stamp); print_string otype; *)
 with_open_file
   (function in_channel ->
     session_read_term (make_session_scanner (Stream.of_channel in_channel)))
   stamp otype

(* init disk_levels *)
let read_disk_levels () =
 with_open_db_file
  (function in_channel ->
    let scanner = (make_session_scanner (Stream.of_channel in_channel)) in
     let rec aux () =
       let m = session_maybe_read_term scanner in
        match m with
           None -> []
        | Some term ->         (* (Mbterm.print_term term; *)
                        match dest_term term with
                        { term_op = op; term_terms = [li; dp]}
                         -> (match dest_op (operator_of_term (term_of_unbound_term li)) with
                              { op_name = _; op_params = [id; index]}
                              -> (dest_int_param index, term_of_unbound_term dp) :: aux()
                             |_ -> error ["read_disk_levels"][][term])
                         |_ -> error ["read_disk_levels"][][term]

        in disk_levels := aux ())
  "levels" "lst"

(* todo : might not be a bad idea to do both for a while and compare answers. *)

(*
let db_lib_read stamp object_type =
  let sterm = stamp_to_term stamp and oterm = istring_term object_type in
   orb.eval_args_to_term tid sterm [oterm]
*)

let db_read_mathbus stamp object_type =
 let term = db_read_aux stamp object_type in
   Hashtbl.add db_cache (stamp, object_type) term;
   term

let db_read stamp otype =
 if !asciip
    then db_read_ascii stamp otype
    else db_read_mathbus stamp otype

let db_write stamp object_type term =
  let {process_id = pid; seq = seq; _} = dest_stamp stamp in
  let filename = String.concat ""
      [!master_pathname; pid; (string_of_int seq); "."; object_type] in
  let descr = openfile filename [O_EXCL; O_WRONLY; O_CREAT] 999 in
  (write_node (mbterm_of_term term) (out_channel_of_descr descr));
  close descr

let db_init master ascp =
  asciip := ascp;
  let name = if (chareq (String.get master (String.length master - 1)) '/') then master
             else String.concat "" [master; "/"] in
   master_pathname := name;

   if !asciip then read_disk_levels ()

 (*let {process_id = pid} = dest_stamp stamp in
 process_pathname := String.concat "" [name ; pid];
 mkdir !process_pathname 999*)

let string_to_term s =
  scan_term (make_session_scanner (Stream.of_string s))

let session_string_to_term s =
  session_read_term (make_session_scanner (Stream.of_string s))
