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

open Utils
open Basic
open Library


exception Testfailed

let oiljgs connection = 

  let cookie = ref "" in
  let lib = lib_new connection "testall" in
    (unwind_error
      (function () -> 
	(* test put *)
	(with_transaction lib
	   (function t ->
	     let oid = make_root t "demo" in 
		insert_leaf t oid "test1" "TERM" (inatural_term 1))
	; (leave lib)))

     (function () -> (lib_close lib); (raise Testfailed)));

  let lib = join connection ["testall"] in
    (unwind_error
      (function () -> 
	(* test get *)
	(with_transaction lib
	   (function t ->
		 let oid = root t "demo" in 
		   if not (1 = number_of_inatural_term (get_term t (child t oid "test1")))
		        then (print_string "check"; raise (Test "check"))))
	; cookie := (lib_close lib))

     (function () -> (lib_close lib); (raise Testfailed)));

    print_endline "open insert leave join get successful.";

    !cookie
	  
let seri connection cookie =
  let lib = restore connection cookie (function t -> ()) in

  let ex = ref "" 
  and ncookie = ref "" in
  
    (unwind_error
      (function () -> 
	(* test put *)
	let oid = with_transaction lib
		   (function t ->
		     child t (root t "demo") "test1") in
	   ncookie := save lib (function t -> ex := oid_export t oid);
	   lib_close lib)
	
      (function () -> lib_close lib));

    let oid = null_oref () in
    let nlib = restore connection !ncookie (function t -> oref_set oid (oid_import t !ex); ()) in
      (unwind_error
	(function () -> 
	   (with_transaction nlib
	     (function t ->
		   if not (1 = number_of_inatural_term (get_term t (oref_val oid)))
		        then (print_string "restore check"; raise (Test "check"))));
	   ncookie := lib_close nlib)
        (function () -> lib_close nlib));

      print_endline "save export restore import successful.";

      !ncookie

exception Pleasefail

(* join after close, then ... *)
let ptest connection =
  let lib = join connection ["testall"] in
    (unwind_error
      (function () -> 
	(* test get *)
	let doid = (with_transaction lib
		   (function t ->
			 let oid = root t "demo" in 
			   if not (1 = number_of_inatural_term (get_term t (child t oid "test1")))
			        then (print_string "check"; raise (Test "check"))
			   ; oid)) in

	
	let noid = (with_transaction lib
		     (function t ->
 		       create t "TERM" (Some (inatural_term 2)) [("foo", inatural_term 3)] ))
	in
  	  try
	  (with_transaction lib
	    (function t ->
	      (* monkey with properties and then fail*)		
	      (let ps = get_properties t noid in

		put_properties t noid (("goo", inatural_term 4) :: ps);
		if not (3 = number_of_inatural_term (get_property t noid "foo"))
		   then raise Testfailed;
		if not (4 = number_of_inatural_term (get_property t noid "goo"))
		   then raise Testfailed;

		put_property t noid "foo" (inatural_term 5);
		if not (5 = number_of_inatural_term (get_property t noid "foo"))
		   then raise Testfailed;

		remove_property t noid "goo";
                (let failp = ref false in 
		  (try (get_property t noid "goo"); () with e -> failp := true);
		  if (not !failp) then raise Testfailed);

 		raise Pleasefail)))
		
	  with
	    Pleasefail -> 
	     (with_transaction lib
	      (function t ->
		(if not (3 = number_of_inatural_term (get_property t noid "foo"))
		   then raise Testfailed
		   else ())))
	   | e -> raise e;

     (lib_close lib); ())

     (function () -> (lib_close lib); (raise Testfailed)))

  ; print_newline()
  ; print_endline "property test successful, (the failures were part of the test)."



  
let testall remote_port local_port =
 print_newline(); 
 print_newline(); 
 print_endline "TestAll Called ";

  let cookie = ref "" in
  (let connection = connect "ALFHEIM" remote_port local_port in

    (unwind_error
      (function () -> 
	  cookie := oiljgs connection
	; cookie := seri connection !cookie
	; ptest connection
	)
      (function () -> disconnect connection; (raise Testfailed)))

    ; disconnect connection)

 ; print_string "TestAll DONE" 
 ; print_newline()
 ; print_newline()
;;




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

let demo lib =
 (with_transaction lib
   (function t -> let oid = make_root t "demo" in make_directory t oid "test"))
 

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
        (let lib = lib_new connection "nuprl-light" in

 	  unwind_error
	    (function () -> (demo lib))
(*
	    (function () -> (demo lib))
 	    (function () -> (activate_test lib (put_get_test lib (create_test lib) 1)))
*)
	    (function () -> lib_close lib);
          lib_close lib))
       (function () -> disconnect connection);

    disconnect connection);

 raise (Test "DONE") 
;;


let demo_put_test lib i =
 (with_transaction lib
	     (function t ->
		let oid = root t "demo" in
		 insert_leaf t oid "nuprl_light_data" "TERM" (inatural_term i)));
 ()


let demo_get_put_test lib =
 (with_transaction lib
	     (function t ->
		let rootoid = root t "demo" in
		let childoid = (child t rootoid "nuprl_light_data") in

		let i = number_of_inatural_term (get_term t childoid) in

		  put_term t childoid (inatural_term (i + 1));

		  if not (i = 290)
		     then raise (Test "Failed")
		     else ())) 
  ; ()


let jointest remote_port local_port =
 print_string "Test called ";
 print_newline();

  (let connection = connect "ALFHEIM" remote_port local_port in

    unwind_error
      (function () ->
        (let lib = join connection ["nuprl-light"] in
 	  unwind_error
	    (function () -> (demo_get_put_test lib))
(*
	    (function () -> (demo_get_put_test lib))
	    (function () -> (demo_put_test lib 289))
*)
	    (function () -> leave lib);
          leave lib))
       (function () -> disconnect connection);

    disconnect connection);

 raise (Test "Join Test Successful") 
;;


special_error_handler (function () -> testall 3289 2892)
 (fun s t -> print_string s; print_newline(); Mbterm.print_term t)

(*
special_error_handler (function () -> testall 5289 2895)
 (fun s t -> print_string s; print_newline(); Mbterm.print_term t)

special_error_handler (function () -> jointest 5289 2895)
 (fun s t -> print_string s; print_newline(); Mbterm.print_term t)

special_error_handler (function () -> test 5289 2895)
 (fun s t -> print_string s; print_newline(); Mbterm.print_term t)
*)


