(*
 *	
 *	Compile test.ml : should only be called when library.cma is up-to-date
 * 	ocamlc -c test.ml
 * 
 *	Create test executable
 *	ocamlc -o test -custom -I ../../nuprl5/sys/io/mathbus unix.cma io.cma library.cma test.cmo -cclib -lunix
 *	
 *)

exception NoTest
exception Test of string

open Basic
open Library

let test remote_port local_port =
 print_string "Test called ";
 print_newline();

 special_error_handler
 (function () -> (
 (let connection = connect "ALFHEIM" remote_port local_port in

 (let lib = lib_open connection in
     (* raise (Test "Ungraceful client failure"); *)
  lib_close lib);

  disconnect connection);
  ))
 (fun s t ->  raise (Test s));
 raise (Test "DONE")
;;


test 3289 2289;;
