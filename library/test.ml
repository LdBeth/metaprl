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

let create_test lib = 
   with_transaction lib
	(function t ->
	  create t "TERM" None []) 


let put_get_test lib oid i =
 (with_transaction lib
	     (function t ->
                 put_term t oid (inatural_term i)));
 i = number_of_inatural_term
       (with_transaction lib
         (function t -> get_term t oid))
    

let test remote_port local_port =
 print_string "Test called ";
 print_newline();

 (let connection = connect "ALFHEIM" remote_port local_port in

 (unwind_error
 (function () -> (

 (let lib = lib_open connection in
     (* raise (Test "Ungraceful client failure"); *)
	
  unwind_error
  (function () -> (

    put_get_test lib (create_test lib) 1;
    ))

  (function () -> lib_close lib);
  lib_close lib)
  ))

  (function nil -> disconnect connection));
  disconnect connection);

 raise (Test "DONE") 
;;

test 4289 5289 
