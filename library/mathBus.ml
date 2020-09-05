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

(*ocaml mathbus implementation*)

open Lm_debug

open Lm_num
open Registry
open Lint32

let _ =
   show_loading "Loading MathBus%t"

type mbnode = Mbint of lint32 | Mnode of mbterm
 and mbterm = mbnode array ;;

let use_unicode = ref false

let minimum_global_numeric_label = create 0X0000
let maximum_global_numeric_label = lbor (lbsl (create 0X00FF) 16) (create 0XFFFF)
let minimum_local_numeric_label = lbsl (create 0X0100) 16
let maximum_local_numeric_label = lbor (lbsl (create 0XFFFF) 16) (create 0XFFFF)

(* ;; In this reference implementation, the local labels are allocated
   ;; from the maximum down, however this is not necessary in all
   ;; implementations.  The precise allocations of local labels should
   ;; be transparent to the user.

   ;; All of the information about labels is contained in the registry.
   *)
let next_local_label = ref maximum_local_numeric_label
let symbolic_label num = registry_lookup_identifier "StringId" num ;;
let numeric_label string = registry_lookup_value string "StringId" ;;

let mbs_Attributes = ref (create 0X0000);;
let mbs_String = ref (create 0X0000);;
let mbs_LongInteger = ref (create 0X0000);;
let mbs_Token = ref (create 0X0000);;

let assign_mbs_vals () =
  mbs_Attributes := numeric_label "Attributes";
  mbs_String := numeric_label "String";
  mbs_LongInteger := numeric_label "LongInteger";
  mbs_Token := numeric_label "Token"

;;

(* ;; Subterm_Types returns the subtypes of a node ie. which sub_terms are
   ;; 32_bit integers and which are nodes themselves. The meansings are:
   ;;
   ;; <n>  non_negative integer: the number of "leading" sub_terms that
   ;;      are 32_bit integers.
   ;; _1   All of the sub_terms are 32_bit integers
   ;; _2   The first sub_term is a node and the rest are 32_bit integers
   ;; _3   The even indexed sub_terms are 32_bit integers
   ;; _4   The odd indexed sub_terms are 32_bit integers
   ;; _5   The first sub_term is a string identifier and the remaining
   ;;      sub_terms are references to nodes.
   ;; _6   The even indexed sub_terms are string identifiers and the odd
   ;;      indexed sub_terms are references to nodes.
   ;; _7   The odd indexed sub_terms are string identifiers and the even
   ;;      indexed sub_terms are references to nodes.
*)

let subterm_types num =
  registry_lookup_value (registry_lookup_identifier "StringId" num) "SubTypes"

(*
  ;; The following routine creates a new local string identifer, if
  ;; needed, filling in the sub_types information, if provided, in the
  ;; registry.
*)
(* unused
let declare_local_stringId symlabel subtypes =
  try (registry_lookup_value symlabel "StringId") with
  Not_found ->
      let numlabel = !next_local_label in
      if blt numlabel minimum_local_numeric_label then
	failwith "Ran out of local_labels"
      else
	begin
	  registry_store_local symlabel "StringId" numlabel;
	  bdecr next_local_label;
	  if bgt subtypes (create (-7)) then
	    failwith "Invalid SubTypes field"
	  else registry_store_local symlabel "SubTypes" subtypes

	end;

      numlabel ;;
*)

let mbnode_labelq node = match node.(0) with
  Mbint i -> i
| Mnode n -> failwith "first element of node not integer"

let mbnode_label node =
  let i = mbnode_labelq node in
  if bequal i !mbs_Attributes then
    (match node.(1) with
      Mbint b -> failwith "node following attributes is an int"
    | Mnode n -> mbnode_labelq n)
  else i

let mbnode_nSubtermsq node = Array.length node - 1
let mbnode_nSubterms node =
  mbnode_nSubtermsq
    (let i = mbnode_labelq node in
    if bequal i !mbs_Attributes then
      (match node.(1) with
	Mbint b -> failwith "node following attributes is an int"
      | Mnode n -> n)
    else node)

let mbnode_subtermq node i =Array.get node i ;;
let mbnode_subterm node index =
  mbnode_subtermq
    (let i = mbnode_labelq node in
    if bequal i !mbs_Attributes then
      (match node.(1) with
	Mbint b -> failwith "node following attributes is an int"
      | Mnode n -> n)
    else node)
    index

(* unused
let mbnode_set_subterm node id v =
  Array.set (let i = mbnode_labelq node in
  if bequal i !mbs_Attributes then
    (match node.(1) with
      Mbint b -> failwith "node following attributes is an int"
    | Mnode n -> n)
  else node)
    id v
*)

let make_mbnode numlabel length =
  let node = (Array.make (1 + length) (Mbint (create 0)):mbterm) in
  Array.set node 0 (Mbint (numlabel));
  node

let mbnode numlabel args=
  let node = make_mbnode numlabel (List.length args) in
  let rec maux i ls=
    (match ls with
      [] -> ()
    | hd::tl -> (Array.set node i (Mnode hd);
		 maux (i + 1) tl)) in
  maux 1 args;
  node

(*
;; The following macro should logically appear in the section on
;; iterators, but it is used much earlier.

;; Binds SUBTYPES to the sub_types of the node,
;; also binds TYPE for each sub_term.
;; TYPE = nil for node reference
;; TYPE = :32bit for 32_bit integer
;; TYPE = :stringId for string identifier
*)
let loop_over_subterms node f =
  let b = mbnode_label node in
  let len = mbnode_nSubterms node in
  let alternate first second =
    for i = 1  to (len+1) do
      f i first;
      if i >= len then
	f (i+ 1) second; done in

   let c = (subterm_types b) in
    (if bequal c (create 0) then		 (* All references to nodes*)
      for i = 1 to len
      do f i None done
    else if bequal c (create (-1)) then		 (*All sub_terms are 32_bit ints*)
      for i = 1 to len
      do f i (Some "32bit") done
    else if bequal c (create (-2)) then
      (f 1 None;
       for i = 2 to len
       do f i (Some "32bit") done)
    else if bequal c (create (-3)) then
      alternate None (Some "32bit")
    else if bequal c (create (-4)) then
      alternate (Some "32bit") None
    else if bequal c (create (-5)) then
      (f 1 (Some "stringId");
       for i = 2 to len
       do f i None done)
    else if bequal c (create (-6)) then
      alternate  None (Some "stringId")
    else if bequal c (create (-7)) then
      alternate (Some "stringId") None
    else if blt c (create (-7)) then
      failwith "Illegal SUBTYPES field" else let (a, x) = dest_lint32 c in
      (for i = 1 to x
      do f i (Some "32bit") done;
       for i = (1 + x) to len
       do f i None done))

(* unused
 let maximum_integern = num_of_int 1073741823 ;;
*)

(* call with num
let mb_number num =
  let a = (abs_num num) and b = (num_of_int 1000000000) in
  if a </ b then
    let node = make_mbnode !mbs_LongInteger 1 in
    Array.set node 1 (Mbint (mk_bint (int_of_num num)));
    node
  else
    let node = make_mbnode !mbs_LongInteger 2 in
    let base = (a//1000000000) in
    Array.set node 1 (if num >=/ (num_of_int 0) then
      (Mbint (mk_bint base))
    else (Mbint (bminus (mk_bint 0) base)));
    Array.set node 2 (Mbint (mk_bint (a mod 1000000000)));
    node
*)

let mb_number num =
   (*print_string "m n "; print_string (string_of_num num);*)
  let a = abs_num num and b = num_of_int 1000000000 in
  let rec loop c l =
    if Lm_num.lt_num c b then
      (Mbint (mk_bint (int_of_num c)))::l
    else loop (quo_num c b) ((Mbint (mk_bint (int_of_num (mod_num c b))))::l) in
  let ints = try (loop a []) with _ ->
      (print_string "mb_number "; print_string (string_of_num a); print_string " quo "; print_string (string_of_num a); print_string " "; print_string (string_of_num b); print_string " = "; print_string (string_of_num (quo_num a b)); print_string " mod "; print_string (string_of_num (mod_num a b)); failwith "foo")
  in
  let length = List.length ints in
  let node = make_mbnode !mbs_LongInteger length in
  let rec assign i l =
    if l = [] then ()
    else
      (Array.set node i (List.hd l);
       assign (i+1) (List.tl l)) in
  assign 1 ints;
  (if Lm_num.lt_num num (num_of_int 0) then
    let bval = (match node.(1) with
      Mbint b -> b
    | Mnode n -> failwith "node ") in
    Array.set node 1
      (Mbint (bminus (mk_bint 0) (dest_bint bval))));
  node

let mb_numberq num label =
  let a = abs_num num and b = num_of_int 1000000000 in
  let rec loop c l =
    if Lm_num.lt_num c b then
      (Mbint (mk_bint (int_of_num num)))::l
    else loop (quo_num c b) ((Mbint (mk_bint (int_of_num (mod_num c b))))::l) in
  let ints = loop a [] in
  let length = List.length ints in
  let node = make_mbnode label length in
  let rec assign i l =
    if l = [] then ()
    else
      (Array.set node i (List.hd l);
       assign (i+1) (List.tl l)) in
  assign 0 ints;
  (if Lm_num.lt_num num (num_of_int 0) then
    let bval = (match node.(1) with
      Mbint b -> b
    | Mnode n -> failwith "node") in
    Array.set node 1
      (Mbint (bminus (mk_bint 0) (dest_bint bval))));
  node

(* call with regular integer*)
let mb_integer int =
  let a = abs int in
  if a < 1000000000 then
    let node = make_mbnode !mbs_LongInteger 1 in
    Array.set node 1 (Mbint (mk_bint int));
    node
  else
    let node = make_mbnode !mbs_LongInteger 2 in
    let base = (a/1000000000) in
    Array.set node 1 (if int >= 0 then
      (Mbint (mk_bint base))
    else (Mbint (bminus (mk_bint 0) base)));
    Array.set node 2 (Mbint (mk_bint (a mod 1000000000)));
    node

(* call with bigint*)
(* unused
let mb_integerb int =
  let node = make_mbnode !mbs_LongInteger 1 in
    Array.set node 1 (Mbint int);
    node
*)

let mb_integerq int label =
  let a = (abs int) in
  if a < 1000000000 then
    let node = make_mbnode label 1 in
    Array.set node 1 (Mbint (mk_bint int));
    node
  else
    let node = make_mbnode label 2 in
    let base = a/1000000000 in
    Array.set node 1 (if int >= 0 then
      (Mbint (mk_bint base))
    else (Mbint (bminus (mk_bint 0) base)));
    Array.set node 2 (Mbint (mk_bint (a mod 1000000000)));
    node

 (*return bigint
let integer_value node=
  let base = integer_valueb node in
	 let neg = blt base (create 0) in

	let rec loop b i =
	 if i >= (1+ (mbnode_nSubterms node)) then b
	   else loop ((b* 1000000000)+ (mbnode_subtermq node i)) (i + 1) in
	 loop (if neg then (bminus (create 0) base) else base) 2;
    if neg then
	(*bminus (create 0)*) b
	 else b

let integer_valueb node=
  match (mbnode_subtermq node 1) with
    Mbint b -> b
  | Mnode n -> failwith "integer_value"
*)

let number_value node=
  let nsubterms = mbnode_nSubterms node in
  match (mbnode_subtermq node 1) with
    Mbint b -> let base = num_of_int (dest_bint b) in if nsubterms = 1 then base
    else (match (mbnode_subtermq node 2) with
      Mbint c -> let int = Lm_num.add_num (Lm_num.mult_num (abs_num base) (num_of_int 1000000000)) (num_of_int (dest_bint c)) in
      if Lm_num.ge_num base (num_of_int 0) then int else (Lm_num.sub_num (num_of_int 0) int)
    | Mnode n -> failwith "integer_value")
  | Mnode n -> failwith "integer_value"

let integer_value node=
  let nsubterms = (mbnode_nSubterms node) in
  match (mbnode_subtermq node 1) with
    Mbint b -> let base = int_of_lint32 b in if nsubterms = 1 then base
    else (match (mbnode_subtermq node 2) with
      Mbint c -> let int = ((abs base) * 1000000000) + (int_of_lint32 c) in
      if base >= 0 then int else (-int)
    | Mnode n -> failwith "integer_value")
  | Mnode n -> failwith "integer_value"


(* unused
let ttbit_sign_bit = lbsl (create 1) 31
let ttbit_twos_complement = lbsl (create 1) 32
*)

(* The following packs ASCII strings into 32_bit words, two characters
;; to a word.  So, every other byte is 0.  This is leave room for the
;; Unicode encoding that I will put in soon.*)


let mb_stringq_with_unicode s num_id =
  let len = String.length s in
  let node = make_mbnode num_id (((1+ len) asr 1) + 1) in
  Array.set node 1 (Mbint (create len));
  for i = 0 to (len/2)
  do let j = 2 + i in
  if j <= (((1+ len) asr 1) + 1)
  then match node.(j) with
    Mbint b -> Array.set node j
	(Mbint (if (1+ (i * 2))< len then
	  lbor (lbsl (create (Char.code (Lm_string_util.get "MathBus.mb_stringq" s (2 * i)))) 16)
	    (create (Char.code  (Lm_string_util.get "MathBus.mb_stringq" s (1+ (2 * i)))))
	else lbsl (create (Char.code (Lm_string_util.get "MathBus.mb_stringq" s (2 * i)))) 16))
  | Mnode n -> failwith "mb_string";

  done;
  node

let mb_stringq s num_id =
  if !use_unicode then mb_stringq_with_unicode s num_id
  else let len = String.length s in
  let m = len mod 4 in let d = (len - m) / 4 in let a = if (m = 0) then (1 + d) else (2 + d)
  in let node = make_mbnode num_id a in
  Array.set node 1 (Mbint (create len));
  let rec loop i j =
  if (1 + i) > len then () else
  (match node.(j) with
    Mbint b -> (Array.set node j
	(Mbint
	  (lbor (lbor (lbsl (create (Char.code (Lm_string_util.get "MathBus.mb_string" s i))) 24)
	             (if (i + 2) > len then (create 0)
			else (lbsl (create (Char.code (Lm_string_util.get "MathBus.mb_string" s (1 + i)))) 16)))
	       (lbor (if (i + 3) > len then (create 0) else (lbsl (create (Char.code (Lm_string_util.get "MathBus.mb_string" s (2 + i)))) 8))
		     (if (i + 4) > len then (create 0) else (create (Char.code (Lm_string_util.get "MathBus.mb_string" s (3 + i)))))))); loop (i + 4) (1 + j))
  | Mnode n -> failwith "mb_string") in
  loop 0 2;

  node

let mb_string s =
  if !use_unicode then mb_stringq_with_unicode s !mbs_String
  else mb_stringq s !mbs_String

let string_value_with_unicode node =
  match (mbnode_subtermq node 1) with
    Mbint b -> let (x, y) = dest_lint32 b in
    let str = if y < 0 then failwith "string_value" else Lm_string_util.create "MathBus.string_value" y in
    let rec loop i j =
      if (i >= y) || (j > (mbnode_nSubterms node)) then str else
      (match node.(j) with
	Mbint c -> let (a, b) = dest_lint32 c in
	(Lm_string_util.set "MathBus.string_value_with_unicode" str i (Char.chr a);
	 if (i + 1) < y then
	   Lm_string_util.set "MathBus.string_value_with_unicode" str (i + 1) (Char.chr b);
	 loop (i + 2) (j + 1))
      | Mnode n -> failwith "string_value") in
    Bytes.to_string (loop 0 2)
  | Mnode n -> failwith "string_value"


let string_value node =
  if !use_unicode then string_value_with_unicode node
  else match (mbnode_subtermq node 1) with
    Mbint b -> let (x, y) = dest_lint32 b in
    let str = if y < 0 then failwith "string_value" else Lm_string_util.create "MathBus.string_value" y in
    let rec loop i j =
      if (i >= y) || (j > (mbnode_nSubterms node)) then str else
      (match node.(j) with
	Mbint c -> let (a, b) = dest_lint32 c in let a1 = (a asr 8) land 0xFF and a2 = a land 0xFF and b1 = (b asr 8) land 0xFF and b2 = b land 0xFF in
	(Lm_string_util.set "MathBus.string_value" str i (Char.chr a1);
	 if (i + 1) < y then
	   Lm_string_util.set "MathBus.string_value" str (i + 1) (Char.chr a2);
	 if (i + 2) < y then
	   Lm_string_util.set "MathBus.string_value" str (i + 2) (Char.chr b1);
	 if (i + 3) < y then
	   Lm_string_util.set "MathBus.string_value" str (i + 3) (Char.chr b2);
	 loop (i + 4) (j + 1))
      | Mnode n -> failwith "string_value") in
    Bytes.to_string (loop 0 2)
  | Mnode n -> failwith "string_value"

(*prints to standard output*)
let rec print_node node =
  let b = mbnode_label node in
  if bequal b !mbs_LongInteger then
    begin
      print_char '{';
      print_string (string_of_num (number_value node));
      print_char '}'
    end
  else if (bequal b !mbs_String) || (bequal b !mbs_Token) then
    print_string (string_value node)
  else
    begin
      print_char '(';
      print_string (symbolic_label b);
      let loop i stype =
      	match stype with
	  None -> (match (mbnode_subtermq node i) with
	    Mnode n -> (print_char '('; print_node n; print_char ')')
	  | Mbint b -> failwith "print node bint")
      	| Some s ->
	    if s = "32bit" then
	      (match (mbnode_subtermq node i) with
	      	Mbint b -> (print_string " ("; print_lint32 b; print_string ")")
	      | Mnode n -> failwith "print node bint")
      	    else (match (mbnode_subtermq node i) with
	      Mbint b -> (print_char '('; print_string (registry_lookup_identifier "StringId" b); print_char ')')

      	    | Mnode n -> failwith "print node bint") in
      loop_over_subterms node loop
    end



(*;;; To avoid problems with operating system differences, the byte
;;; stream generated can be restricted to consist solely of ASCII
;;; printing characters.  This format uses a radix 64 representation
;;; for the words, encoded as follows: *)

let byte_count = ref 0

let base64_translation_list =
    [ 0,'A';  1,'B';  2,'C';  3,'D';  4,'E';  5,'F';  6,'G';  7,'H';
      8,'I';  9,'J'; 10,'K'; 11,'L'; 12,'M'; 13,'N'; 14,'O'; 15,'P';
     16,'Q'; 17,'R'; 18,'S'; 19,'T'; 20,'U'; 21,'V'; 22,'W'; 23,'X';
     24,'Y'; 25,'Z'; 26,'a'; 27,'b'; 28,'c'; 29,'d'; 30,'e'; 31,'f';
     32,'g'; 33,'h'; 34,'i'; 35,'j'; 36,'k'; 37,'l'; 38,'m'; 39,'n';
     40,'o'; 41,'p'; 42,'q'; 43,'r'; 44,'s'; 45,'t'; 46,'u'; 47,'v';
     48,'w'; 49,'x'; 50,'y'; 51,'z'; 52,'0'; 53,'1'; 54,'2'; 55,'3';
     56,'4'; 57,'5'; 58,'6'; 59,'7'; 60,'8'; 61,'9'; 62,'+'; 63,'/']

let base64_by_num_table = Array.make 64 'a';;
let base64_by_char_table = Array.make 128 (-2);;
let base64_char_count = ref 0 ;;
let mask_table = Array.make 30 0 ;; (*LAL*)

let initialize_base64 =
  let f (num, char) = Array.set base64_by_num_table num char;
    Array.set base64_by_char_table (Char.code char) num in
   List.iter f base64_translation_list;

  for i = 0 to 29 do
    Array.set mask_table i ((1 lsl i) - 1) done;
  base64_char_count := 0

let graphic_char_p c=
  let (m, l) = List.split base64_translation_list in
  List.mem c l

let num2pascii num = base64_by_num_table.(num)

let pascii2num char=
  if graphic_char_p char then
    base64_by_char_table.((Char.code char)) else (-1)


let write_base64_char char stream =
  let rec try_out ch =
   try (output_char stream ch) with
   Sys_error e(*"Try again"*) ->
     (Unix.sleep 1; try_out ch) in
  if !base64_char_count = 0  then
    (try_out '\n';
    base64_char_count := 64);
         (* print_char char;*)
  try_out char;
  decr base64_char_count


let print_base64_char char =
  if !base64_char_count = 0  then
    (print_char '\n';
     base64_char_count := 64);
  print_string "char is ";
  print_char char;
  decr base64_char_count

let print_base64_num num =
    print_string "num is ";
    print_int num;
    print_base64_char (num2pascii num)

let write_base64_num num stream =
   write_base64_char (num2pascii num) stream


let stream_mode = "base64"

(* unused
let base64_obuffer = ref  0
let base64_ocount = ref 0 *)
let buffer = ref 0
let cnt = ref 0

let flush_buffer buffer stream cnt =
  (*for i = 1 to 10000000
      do (); done; flush stream;  *)
  match !cnt with
    0 -> ()
  | 1 ->
      (write_base64_num ((!buffer asr 2) land 0x3F) stream;
       write_base64_num ((!buffer lsl 4) land 0x3F) stream;
       write_base64_char '=' stream;
       write_base64_char '=' stream;
       cnt:= 0;
       byte_count:= !byte_count + 4;
       buffer:= 0)
  | 2 ->
      (write_base64_num ((!buffer asr 10) land 0x3F)
	 stream;
       write_base64_num ((!buffer asr 4) land 0x3F)
	 stream;
       write_base64_num ((!buffer lsl 2) land 0x3F)
	 stream;
       write_base64_char '=' stream;
       cnt:= 0;
       byte_count:= !byte_count + 4;
       buffer:= 0)
  |_-> (write_base64_num ((!buffer asr 18) land 0x3F) stream;
	 write_base64_num ((!buffer asr 12) land 0x3F) stream;
	 write_base64_num ((!buffer asr 6) land 0x3F) stream;
	 write_base64_num (!buffer land 0x3F) stream;
	 cnt:= 0;
	 byte_count:= !byte_count + 4;
	 buffer:= 0)

let flush_bufferp buffer cnt =
  match !cnt with
    0 -> ()
  | 1 ->
      (print_base64_num ((!buffer asr 2) land 0x3F);
       print_base64_num ((!buffer lsl 4) land 0x3F);
       print_base64_char '=' ;
       print_base64_char '=' ;
       cnt:= 0;
       byte_count:= !byte_count + 4;
       buffer:= 0)
  | 2 ->
      (print_base64_num ((!buffer asr 10) land 0x3F);
       print_base64_num ((!buffer asr 4) land 0x3F);
       print_base64_num ((!buffer lsl 2) land 0x3F);
       print_base64_char '=' ;
       cnt:= 0;
       byte_count:= !byte_count + 4;
       buffer:= 0)
  |_->  (print_base64_num ((!buffer asr 18) land 0x3F);
	 print_base64_num ((!buffer asr 12) land 0x3F);
	 print_base64_num ((!buffer asr 6) land 0x3F);
	 print_base64_num (!buffer land 0x3F);
	 cnt:= 0;
	 byte_count:= !byte_count + 4;
	 buffer:= 0)

let write_byte64 byte stream =
  buffer := (!buffer lsl 8) lor (byte land 0xFF);
  incr cnt;
  if !cnt = 3 then flush_buffer buffer stream cnt

let print_byte64 byte =
  buffer := (!buffer lsl 8) lor (byte land 0xFF);
  incr cnt;
  if !cnt = 3 then flush_bufferp buffer cnt

 (*;;; Byte stream output*)
let write_32bit num stream =
  let (a, b) = dest_lint32 num in
  match stream_mode  with
    "binary_byte" -> failwith "not yet ready binary"
	  (*write_char logand ash num _24 0XFF stream
	    write_char logand ash num _16 0XFF stream
	    write_char logand ash num _8 0XFF stream
	    write_char logand num 0XFF stream
	    incf byte_count 4*)
  | "base64" ->
      if blte num (create 0xFC) then
	write_byte64 b stream
      else if blte num (create 0xFFFF) then
	(write_byte64 0xFD stream;
	 write_byte64 (b asr 8) stream;
	 write_byte64 (0xFF land b) stream)
      else if a <= 0xFF then
        (write_byte64 0xFE stream;
         write_byte64 a stream;
         write_byte64 (b asr 8) stream;
         write_byte64 (0xFF land b) stream)
      else (write_byte64 0xFF stream;
	     write_byte64 (a asr 8) stream;
	     write_byte64 (0xFF land a) stream;
	     write_byte64 (b asr 8) stream;
	     write_byte64 (0xFF land b) stream)

  | "debug_byte" -> failwith "debug"
	  (*format stream "[~16R ~16R ~16R ~16R]"
	    logand ash num _24 0XFF logand ash num _16 0XFF
	    logand ash num _8 0XFF logand num 0XFF
	    incf byte_count 4*)
  |_-> failwith "Don't know how to write streams in mode"

let print_32bit  num =
  let (a, b) = dest_lint32 num in
  match stream_mode with
    "binary_byte" -> failwith "not yet ready binary"
	  (*write_char logand ash num _24 0XFF stream
	    write_char logand ash num _16 0XFF stream
	    write_char logand ash num _8 0XFF stream
	    write_char logand num 0XFF stream
	    incf byte_count  4*)
  | "base64" ->
      if blte num (create 0xFC) then
	print_byte64 b
      else if blte num (create 0xFFFF) then
	(print_byte64 0xFD ;
	 print_byte64 (b asr 8) ;
	 print_byte64 (0xFF land b) )
      else if b <= 0xFF then
        (print_byte64 0xFE ;
         print_byte64 a ;
         print_byte64 (b asr 8) ;
         print_byte64 (0xFF land b) )
      else (print_byte64 0xFF ;
	     print_byte64 (a asr 8) ;
	     print_byte64 (0xFF land a) ;
	     print_byte64 (b asr 8) ;
	     print_byte64 (0xFF land b) )

  | "debug_byte" -> failwith "debug"
	  (*format stream "[~16R ~16R ~16R ~16R]"
	    logand ash num _24 0XFF logand ash num _16 0XFF
	    logand ash num _8 0XFF logand num 0XFF
	    incf byte_count  4*)
  |_-> failwith "Don't know how to write streams in mode"


let base64_ibuffer = ref 0
let base64_icount =ref 0

let rec base64_read_char stream =
  let n = (pascii2num  (input_char stream) ) in
    (*print_int n;*)
  if n < 0 then base64_read_char stream
  else
    (base64_ibuffer := (!base64_ibuffer lsl 6) lor n;
     base64_icount:= !base64_icount + 6)


let base64_read_8bit stream =
  base64_read_char stream;
  if !base64_icount < 8 then
    base64_read_char stream;
  let temp = !base64_ibuffer in
  base64_icount:= !base64_icount - 8;
  base64_ibuffer:= !base64_ibuffer land (mask_table.(!base64_icount));
  let t = temp asr !base64_icount in  (* print_int t;*)
  t

let read_32bit stream =
  match stream_mode  with
    "binary_byte" -> failwith "not yet read binary"
          (*logior
	    ash read_char stream 24
	    ash read_char stream 16
	    ash read_char stream 8
	    read_char stream*)
  | "base64" ->

      let char = base64_read_8bit stream in

      (match char with
	0xFF -> ((*print_string "0xff";*)
	  let a1 = base64_read_8bit stream in
	  let a2 = base64_read_8bit stream in
	  let a3 = base64_read_8bit stream in
	  let a4 = base64_read_8bit stream in
	  make_lint32 (((a1 lsl 8) lor a2), ((a3 lsl 8) lor a4)))
      | 0xFE -> (
	  let a1 = base64_read_8bit stream in
	  let a2 = base64_read_8bit stream in let a3 = base64_read_8bit stream in
	   make_lint32 (a1, ((a2 lsl 8) lor a3)))
      | 0xFD -> (
	  let a1 =(base64_read_8bit stream) in
	  let a2 =(base64_read_8bit stream) in
	  create ((a1 lsl 8) lor
		   a2))
      | _-> create char)
  |_-> failwith "Don't know how to read streams in mode"


 (*
;; Compute the maximum depth and width of a tree.
let depth_and_width node =

  let rec aux n d w =
	let nsubterms = mbnode_nSubterms n in
		 let aux n d (max w nsubterms) =
		 let f i type = when null type
		       setq depth
			     max node_iterate1 mbnode_subtermq node i
						 #'func context
				 in
		   loop_over_subterms node f
		       depth
		   1+ depth
      values node_iterate node #'func max_width
*)
let header_num = lbor (lbsl (create 0XFADE) 16) (create 0XBAC0)
let trailer_num = lbor (lbsl (create 0XABCD) 16) (create 0XEF00)

let rec rwrite_node node stream =
  let op = mbnode_labelq node in
  let nSubterms = (create (mbnode_nSubterms node)) in
  (write_32bit op stream;
  write_32bit nSubterms stream;
  let loop i typ =
    (match typ with
      None -> (match (mbnode_subtermq node i) with
	Mnode n ->
	  rwrite_node n stream
      | Mbint b -> failwith "rwrite1")
    |  Some s ->(match (mbnode_subtermq node i) with
	Mbint b2 ->
	  write_32bit b2 stream
      | Mnode n2 -> failwith "rwrite2"))
  in
  loop_over_subterms node loop)




let write_node node stream =
  (byte_count := 0;
   cnt:= 0;
   buffer:= 0;
     (*;; Write header information*)
   write_32bit header_num stream;

   rwrite_node node stream;

     (*;; Write trailer information
       ;; FIXTHIS: Need to compute the check_sum someplace...*)
   write_32bit trailer_num stream;

   if stream_mode = "base64" then
     flush_buffer buffer stream cnt;
   base64_char_count := 0)

(* unused
let stringID_translation = Hashtbl.create 7
*)

let rec read_node_internal stream =
  let op = read_32bit stream and
      (a, b) = (dest_lint32 (read_32bit stream)) in
  let node = make_mbnode op b in
  (let loop i stype =
    Array.set node i
      (match stype with
	None -> Mnode (read_node_internal stream)
      | Some s -> if s = "32bit"  then
	  Mbint (read_32bit stream) else
	  let b = read_32bit stream in
		     (* or gethash b  stringID_translation *)
	  Mbint b)
  in
  loop_over_subterms node loop;

      (*if op = !mbs_RegistryStoreLocal then
      	cond eql mbnode_subtermq node 2 rsl_stringId
	let new_op declare_local_stringId
	string_value mbnode_subtermq node 3
	unless eql new_op op
	setf gethash op  stringId_translation  new_op
	eql mbnode_subtermq node 2  rsl_subtypes
	Error "not yet implemented"*)
  node)

let read_node stream =
  (*
     ;;;;;;;;;
     ;; Handle header
     ;;;;;;;;*)
  base64_ibuffer  := 0;
  base64_icount := 0;

  let (a, b) = dest_lint32 (read_32bit stream) in (* print_int a; print_int b;*)
  if not (bequal (make_lint32 (a, b)) header_num) then
    failwith "Bad header word for MathBus stream" else
     (*;; This implementation doesn't need to know the depth or width of the
       ;; node being constructed a priori.*)


    (let node = read_node_internal stream in
       (*;; Trailer*)
    let _ = read_32bit stream in
    node)



