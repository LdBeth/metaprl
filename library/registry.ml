(* In this simple implementation, a registry is a collection of hash
 * tables, one or two for each registry type.  There are two
 * registries, one for the global information, and one for the local
 * information.
 *)
open BigInt

type tb = Uni of (string, bigint) Hashtbl.t
  | Bi of (string, bigint) Hashtbl.t*(bigint, string) Hashtbl.t
type regtbl = (string, tb) Hashtbl.t
      
type  regval =  bigint option
type regid = string option
      
let global_registry = (Hashtbl.create 7:regtbl)
let local_registry = (Hashtbl.create 7:regtbl)

let registry_types = []

(*
 * Allow the registry file to be specified in an environment variable.
 *)
let registry_file_ref = Env_arg.string (**)
                           "registry"
                           "/usr/u/nuprl/nuprl5/bin/reg-file.txt"
                           "The registry file defines the MathBus syntax"
                           (fun _ s v -> s := v)

let registry_file = !registry_file_ref

let define_registry_type label bidirectional = 
  if (List.mem label registry_types) then ()
  else 
    begin
      Hashtbl.add local_registry label
        (if bidirectional then
	   Bi ((Hashtbl.create 10), (Hashtbl.create 10))
	else Uni (Hashtbl.create 10));
      
      Hashtbl.add global_registry label (if bidirectional then
	Bi ((Hashtbl.create 10), (Hashtbl.create 10))
      else Uni (Hashtbl.create 10));
      
      let registry_types  = label::registry_types in  ()
    end  ;;
      

define_registry_type "StringId" true  ;;
define_registry_type "SubTypes" false  ;;

let clear_registry globalp localp =
  begin
    if globalp then
      Hashtbl.clear global_registry
    else ();
    if localp then
      Hashtbl.clear local_registry
    else ()
  end

 (*params are string*)	 
let registry_lookup_value id regtype =
  match (Hashtbl.find local_registry regtype)  with
    Uni h -> let l = Hashtbl.find_all h id in
    if List.length l > 0 then Some (List.hd l)
    else (match (Hashtbl.find global_registry regtype)  with
      Uni h -> let l = Hashtbl.find_all h id in
      if List.length l > 0 then Some (List.hd l)
      else  None
    | Bi (h1, h2) -> let l = Hashtbl.find_all h1 id in
      if List.length l > 0 then Some (List.hd l)
      else  None)
  | Bi (h1, h2) -> let l = Hashtbl.find_all h1 id in
    if List.length l > 0 then Some (List.hd l)
    else (match (Hashtbl.find global_registry regtype)  with
      Uni h -> let l = Hashtbl.find_all h id in
      if List.length l > 0 then Some (List.hd l)
      else None
    |Bi (h1, h2) -> let l = Hashtbl.find_all h1 id in
      if List.length l > 0 then Some (List.hd l)
      else  None)
     
(*param are string*)	 
let registry_lookup_identifier regtype value =
  match (Hashtbl.find local_registry regtype) with
    Uni h ->
      failwith "Not a bidirectional registry property"
  | Bi (h1, h2) ->
      let v = Hashtbl.find_all  h2 value in
      if  (List.length v) > 0 then Some (List.hd v)
      else match (Hashtbl.find global_registry regtype) with
	Uni h -> failwith "Not a bidirectional registry property"
      | Bi (h1, h2) -> let p = Hashtbl.find_all h2 value in
	if List.length  p > 0 then Some (List.hd p)
	else  None
     

(*param are string*)	 
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
			     
let read_number stream =
  let s = read_string stream in
  create (int_of_string s)


let read_registry =
  let stream = open_in registry_file in
  let p = (read_string stream) in
  let rec loop ident =
    if ident = "" then close_in stream
    else  let a = (read_string stream) in let b = (read_number stream) in
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
