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
        then raise (Test "Failed")));
 oid

let activate_test lib oid =
 (with_transaction lib
	     (function t ->
                 activate t oid));
 (with_transaction lib
   (function t ->
	try (eval t (object_id_ap (null_ap (itext_term "\oid. if not (lib_active_p oid) then fail"))
					  oid))
	with e -> raise (Test "Failed")));
 oid

let test remote_port local_port =
 print_string "Test called ";
 print_newline();

  (let connection = connect "ALFHEIM" remote_port local_port in

    unwind_error
      (function () ->
        (let lib = temp_lib_open connection "" in

 	  unwind_error
 	    (function () -> (activate_test lib (put_get_test lib (create_test lib) 1)))
	    (function () -> temp_lib_close lib);
          temp_lib_close lib))
       (function nil -> disconnect connection);

    disconnect connection);

 raise (Test "DONE") 
;;



let demo_put_test lib i =
 (with_transaction lib
	     (function t ->
		let oid = root t "demo" in
		 insert_leaf t oid "nuprl_light_data" "TERM" (inatural_term i)));
 ()



let jointest remote_port local_port =
 print_string "Test called ";
 print_newline();

  (let connection = connect "ALFHEIM" remote_port local_port in

    unwind_error
      (function () ->
        (let lib = lib_join connection ["rledemo"] in
 	  unwind_error
	    (function () -> (demo_put_test lib 289))
	    (function () -> lib_leave lib);
          lib_leave lib))
       (function nil -> disconnect connection);

    disconnect connection);

 raise (Test "Join Test Successful") 
;;


special_error_handler (function () -> jointest 5289 2895)
 (fun s t -> print_string s; print_newline(); Mbterm.print_term t)

(* test 5289 2895 *)


class fu x = 
  val i = x
end

class bar x =
  val s = x
end

type fubar = Fu of fu | Bar of bar
