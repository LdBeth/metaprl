(*
 * This implements a filesystem interface to the library.
 *)

open Printf
open Debug

open File_base_type

open Refiner.Refiner.Term
open Basic

open Utils
open Library

let _ =
   if !debug_load then
      eprintf "Loading Library_type_base%t" eflush



let library = null_oref()
let connection = null_oref()

exception LibraryException of string

let library_close () =
 if oref_p library
    then (leave (oref_val library);
	  disconnect (oref_val connection))
    else raise (LibraryException "Close: No library open.")

open Printf
open Debug


let library_open host localport remoteport =

 eprintf "%s %d %d %t" host localport remoteport eflush;

 if oref_p library
    then raise (LibraryException "Open: Library already open.")
    else ( oref_set library (join (oref_set connection (connect host localport remoteport))
				["NuprlLight"])
	 ; at_exit (function () -> library_close()))
 ; ()

let maybe_lib_open () =

  (if not (oref_p library)
     then let host = Sys.getenv "NUPRLLIB_HOST"
	  and port = int_of_string (Sys.getenv "NUPRLLIB_PORT")
	  in
	  library_open host port (port+2))
 ; ()


				
let lib_get () =
   maybe_lib_open ();
   oref_val library


(*
 * Save a term to the library.
 * "Magic" is a magic number that is sued to identify the
 * version of the file.
 *)

let library_set magic filename term =
  print_string "hello";
  (with_transaction (lib_get())

    (function t ->
      let root = (root t "Files") in
	
	(* make filename dir if none *)
	let dir = try (descendent t root [filename])
		    with _ -> (let ndir = make_directory t root filename in
				put_property t ndir "NAME" (itoken_term filename);
				ndir)
	  in

	  (* store term in filename at magic *)
	  ninsert_leaf t dir (string_of_int magic) "TERM" term))

  ; ()


	

(*
 * Get a term from the library.
 *)

let library_get magic filename =
  print_string "hello g";
  with_transaction (lib_get())

   (function t ->
     let root = (root t "Files") in

       let obid = (descendent t root [filename; (string_of_int magic)])
	 in

	   get_term t obid)

(*
 * This "combo" is the module that defines how to fetch
 * an object from the library.  We are passed an argument
 * that describes how to marshal and unmarshal objects to terms.
 *)
module IO =
struct
   type t = term
   let write = library_set
   let read  = library_get
end

(*
 * $Log$
 * Revision 1.8  1998/05/28 13:46:45  jyh
 * Updated the editor to use new Refiner structure.
 * ITT needs dform names.
 *
 * Revision 1.7  1998/05/27 15:13:26  jyh
 * Functorized the refiner over the Term module.
 *
 * Revision 1.6  1998/05/05 20:05:53  eaton
 * .
 *
 * Revision 1.5  1998/04/08 18:10:34  eaton
 * .
 *
 * Revision 1.4  1998/04/03 18:07:33  eaton
 * .
 *
; Revision 1.3  1998/02/19  17:24:13  jyh
; Splitting filter_parse.
;
 * Revision 1.2  1998/02/18 18:46:37  jyh
 * Initial ocaml semantics.
 *
 * Revision 1.1  1998/02/12 23:35:44  jyh
 * Added base Nuprl-Light interface to the library.
 *
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
