(*
 * In this simple implementation, a registry is a collection of hash
 * tables, one or two for each registry type.  There are two
 * registries, one for the global information, and one for the local
 * information.
 *)
open Printf 
open Debug
 
open Int32

(*
 * jyh: we need documentation for these types.
 *)
type tb =
   Uni of (string, int32) Hashtbl.t
 | Bi of (string, int32) Hashtbl.t * (int32, string) Hashtbl.t

type regtb = (string, tb) Hashtbl.t
      
let global_registry = (Hashtbl.create 3 : regtb)
let local_registry = (Hashtbl.create 3 : regtb)

let registry_types = ref []

(*
 * The registry should be stored in the lib directory.
 *)
let registry_file =
   try Filename.concat (Sys.getenv "NLLIB") "registry.txt" with
      Not_found ->
         raise (Failure "environment variable NLLIB is not defined")

(*
 * Define a particular type of registry.
 *)
let define_registry_type label bidirectional = 
   if not (List.mem label !registry_types) then
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
      
      end

(*
 * These are the two types that we use.
 *)
let _ = define_registry_type "StringId" true
let _ = define_registry_type "SubTypes" false

(*
 * Remove all entries from the global and local registries.
 *)
let clear_registry globalp localp =
   begin
      if globalp then
         Hashtbl.clear global_registry;
      if localp then 
         Hashtbl.clear local_registry
   end

(*
 * Get a value from the registry.
 *)
let registry_lookup_value id regtype =
   match Hashtbl.find local_registry regtype with
      Uni h ->
         (try Hashtbl.find h id with
             Not_found ->
                (match (Hashtbl.find global_registry regtype) with
                    Uni h -> Hashtbl.find h id
                  | Bi (h1, h2) -> Hashtbl.find h1 id))
    | Bi (h1, h2) ->
         (try Hashtbl.find h1 id with
             Not_found ->
                (match (Hashtbl.find global_registry regtype) with
                    Uni h -> Hashtbl.find h id
                  | Bi (h1, h2) -> Hashtbl.find h1 id))

(*
 * Lookup indetifiers.  jyh: what are identifiers?
 *)
let registry_lookup_identifier regtype v =
   match (Hashtbl.find local_registry regtype) with
      Uni h ->
         failwith "Not a bidirectional registry property"
    | Bi (h1, h2) ->
         (try Hashtbl.find h2 v with
             Not_found ->
                (match (Hashtbl.find global_registry regtype) with
                    Uni h -> failwith "Not a bidirectional registry property"
                  | Bi (h1, h2) -> Hashtbl.find h2 v))

(*
 * Save a value in the local registry.
 *)
let registry_store_local id regtype v =
   match Hashtbl.find local_registry regtype with
      Uni h ->
         Hashtbl.add h id v
 | Bi (h1, h2) ->
      (Hashtbl.add h1 id v;
       Hashtbl.add h2 v id)
 
(*
 * Read the next string from the registry.
 *)
let read_string stream =
   let rec read (buf : string) len skip =
      try
         match input_char stream with
            ' ' | '\r' | '\n' | '\t' ->
               if skip then
                  read buf len skip
               else
                  String.sub buf 0 len
          | c ->
               let buf =
                  if String.length buf = len then
                     let buf' = String.create (len * 2) in
                        String.blit buf 0 buf' 0 len;
                        buf'
                  else
                     buf
               in
                  String.set buf len c;
                  read buf (len + 1) false
      with
         End_of_file ->
            String.sub buf 0 len
   in
      read (String.create 100) 0 true

(*
 * Read a 32bit integer from the stream.
 *)
let read_int32 stream =
   let s = read_string stream in
   let v = int_of_string s in
      if v < -9 then (*LAL hack-subtypes can be neg...number too big to convert from string*)
         (print_int v; failwith "fg" )  (*"not yet ready to read in int32s"*)
      else
         int32_of_int v

(*
 * Load the entire registry file.
 *)
let read_registry =
   let stream = open_in registry_file in
   let p = read_string stream in
   let rec loop ident =
      if ident = "" then
         close_in stream
      else
         let a = read_string stream in
         let b = read_int32 stream in
            registry_store_local ident a b;
            loop (read_string stream)
   in
      loop p

(*
 read_registry registry_file
  
 Generated the LISP declaration file for label names.
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
	     Don't bother with MBS_ variables for single characters or Temps'
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
    format ostream "~%~%; End of automatically generated file.~%~%"
           
*)

(*
 * $Log$
 * Revision 1.6  1998/04/08 15:58:26  jyh
 * Updated registry.ml to fix read_string,
 * and adjust the style.
 *
 *)