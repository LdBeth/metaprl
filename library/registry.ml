 
 
open Int32
(* In this simple implementation, a registry is a collection of hash
   * tables, one or two for each registry type.  There are two
   * registries, one for the global information, and one for the local
   * information.
   *)

type tb = Uni of (string, int32) Hashtbl.t
  | Bi of (string, int32) Hashtbl.t * (int32, string) Hashtbl.t
type regtb = (string, tb) Hashtbl.t
      
let global_registry = (Hashtbl.create 3:regtb)
let local_registry = (Hashtbl.create 3:regtb)

let registry_types = ref []

(*
 * Allow the registry file to be specified in an environment variable.
 *)
let registry_file_ref = ref (*Env_arg.string 
                           "registry"*)
                           "/amd/noon/y/nuprl/nuprl4i/nuprl5L/nuprl-light/library/reg-file-test.txt"
                          (* "The registry file defines the MathBus syntax"
                           (fun _ s v -> s := v)*)

let registry_file = !registry_file_ref

let define_registry_type label bidirectional = 
  if (List.mem label !registry_types) then ()
  else 
    begin
      registry_types := label::!registry_types;
      Hashtbl.add local_registry label
	(if bidirectional then
	  Bi ((Hashtbl.create 10), (Hashtbl.create 10))
	else Uni (Hashtbl.create 10));
      
      Hashtbl.add global_registry label 
	(if bidirectional then
	  Bi ((Hashtbl.create 10), (Hashtbl.create 10))
      	else Uni (Hashtbl.create 10));
      
    end;;
      

define_registry_type "StringId" true;;
define_registry_type "SubTypes" false;;

let clear_registry globalp localp =
  begin
    if globalp then
      Hashtbl.clear global_registry
    else ();
    if localp then 
      Hashtbl.clear local_registry
    else ()
  end

let registry_lookup_value id regtype =
  match (Hashtbl.find local_registry regtype) with
    Uni h -> (try Hashtbl.find h id with
      Not_found -> (match (Hashtbl.find global_registry regtype) with
      	Uni h -> Hashtbl.find h id
      | Bi (h1, h2) -> Hashtbl.find h1 id))
  | Bi (h1, h2) -> (try Hashtbl.find h1 id with
      Not_found -> (match (Hashtbl.find global_registry regtype) with
    	Uni h -> Hashtbl.find h id
      | Bi (h1, h2) -> Hashtbl.find h1 id))

let registry_lookup_identifier regtype value =
  match (Hashtbl.find local_registry regtype) with
    Uni h -> failwith "Not a bidirectional registry property"
  | Bi (h1, h2) -> (try Hashtbl.find h2 value with
      Not_found -> (match (Hashtbl.find global_registry regtype) with
	Uni h -> failwith "Not a bidirectional registry property"
      | Bi (h1, h2) -> Hashtbl.find h2 value))
     
let registry_store_local id regtype value =
  match (Hashtbl.find local_registry regtype) with
    Uni h -> Hashtbl.add h id value
  | Bi (h1, h2) -> (Hashtbl.add h1 id value;
		    Hashtbl.add h2 value id)
     
 
(*;; Load a registry file*)
let read_string stream =
  let s = String.create 100 in
  let rec readstring i first=
    if (in_channel_length stream) = (pos_in stream) then String.sub s 0 i
    else match (input_char stream) with
      ' ' ->  if first  then readstring i first else String.sub s 0 i 
    |'\n' -> if first  then readstring i first else String.sub s 0 i
    | c ->  (String.set s i c; readstring (i + 1) false) in
  readstring 0 true 
			     
let read_int32 stream =
  let s = read_string stream in
  let value = (int_of_string s) in
 if value < -9 then (*LAL hack-subtypes can be neg...number too big to convert from string*)
  (print_int value; failwith "fg" )(*"not yet ready to read in int32s"*)
 else int32_of_int value

let read_registry =
  let stream = open_in registry_file in
  let p = (read_string stream) in
  let rec loop ident =
    if ident = "" then close_in stream
    else  let a = (read_string stream) in let b = (read_int32 stream) in
    registry_store_local ident a b;
    loop (read_string stream)
  in loop p

(*	read_registry registry_file
  
;; Generated the LISP declaration file for label names.
let generate_registry_declarations ofile file =
  let stream = open_in file and ostream = open_out ofile in
  when or None ofile None file
    multiple_value_bind in out default_registry_files
      when None ofile
	setq ofile out
      when None file
	setq file in	
  with_open_file stream file :direction :input
    with_open_file ostream ofile :direction :output
    format ostream registry_header_text

    loop for ident = read_item stream
	  with regtype and regval
	  do when None ident
	       return t
	  setq regtype read_item stream
	  setq regval read_item stream
	  when string_equal regtype "StringId"
	    ;; Don't bother with MBS_ variables for single characters or Temps'
	    unless or = length ident 1
			and = length ident 5
			     char= aref ident 0 #\T
			     char= aref ident 1 #\e
			     char= aref ident 2 #\m
			     char= aref ident 3 #\p
	      format ostream "~%defvar mBS_~A #x~16R~%export 'mathbus::MBS_~A 'mathbus~%" 
		      ident regval string_upcase ident
	  format ostream "registry_store_local \"~A\" \"~A\" ~D~%"
		  ident regtype regval
    format ostream "~%~%;;; End of automatically generated file.~%~%"
           
*)
