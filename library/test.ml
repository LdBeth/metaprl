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
                 put_term t oid (inatural_term i);
		 put_property t oid "foo" (inatural_term (i+1))));
 (with_transaction lib
   (function t ->
     if not ((i = number_of_inatural_term (get_term t oid))
	     & (i+1) = number_of_inatural_term (get_property t oid "foo"))
        then raise (Test "Failed")))

let test remote_port local_port =
 print_string "Test called ";
 print_newline();

  (let connection = connect "ALFHEIM" remote_port local_port in

    unwind_error
      (function () ->
        (let lib = temp_lib_open connection "" in

 	  unwind_error
 	    (function () -> (put_get_test lib (create_test lib) 1))
	    (function () -> temp_lib_close lib);
          temp_lib_close lib))
       (function nil -> disconnect connection);

    disconnect connection);

 raise (Test "DONE") 
;;

test 6289 7289 
