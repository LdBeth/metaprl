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
 *
 *	Compile test.ml : should only be called when library.cma is up-to-date
 * 	ocamlc -c test.ml
 *
 *	Create test executable
 *	ocamlc -o test -custom -I ../../nuprl5/sys/io/mathbus unix.cma io.cma library.cma test.cmo -cclib -lunix
 *
 *)

open Lm_debug
open Lm_printf

open Utils
open Basic
open Library

open Ascii_scan

open Library_type_base

let _ =
   show_loading "Loading Test%t"

exception NoTest
exception Test of string

exception Testfailed of int

(*  insert_leaf

*)
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

     (function () -> (lib_close lib); (raise (Testfailed 1))));

  let lib = join connection ["testall"] in
    (unwind_error
      (function () ->
	(* test get *)
	(with_transaction lib
	   (function t ->
		 (print_string "oiljgs 1"); (print_string (string_of_int (List.length (roots t))));
		 let oid = root t "demo" in
		   (print_string "oiljgs 2");
		   (child t oid "test1"); (print_string "oiljgs 3");

		   if not (1 = number_of_inatural_term (get_term t (child t oid "test1")))
		        then (print_string "check"; raise (Test "check"))))
	; cookie := (lib_close lib))

     (function () -> (lib_close lib); (raise (Testfailed 2))));

    print_endline "open insert leave join get successful.";

    !cookie

(* restore
   save
   oid_export
   oid_import
*)


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
(* join
   create
   put_property get_property remove_property
   put_properties get_properties
   put_term get_term
*)

let ptest connection =
  let lib = join connection ["testall"] in
    (unwind_error
      (function () ->
	(* test get *)
	(with_transaction lib
	 (function t ->
	  let oid = root t "demo" in
	   if not (1 = number_of_inatural_term (get_term t (child t oid "test1")))
	        then (print_string "check"; raise (Test "check"))
	   ; ()));

	let noid = (with_transaction lib
		     (function t ->
 		       create t "TERM" (Some (inatural_term 2)) [("foo", inatural_term 3)] ))
	in
  	  try
	  (with_transaction lib
	    (function t ->
	      (* monkey with properties and then fail*)
	      (let ps = get_properties t noid in

		if not (2 = number_of_inatural_term (get_term t noid))
		   then raise (Testfailed 3);
		put_term t noid (inatural_term 3);
		if not (3 = number_of_inatural_term (get_term t noid))
		   then raise (Testfailed 4);

		put_properties t noid (("goo", inatural_term 4) :: ps);
		if not (3 = number_of_inatural_term (get_property t noid "foo"))
		   then raise (Testfailed 5);
		if not (4 = number_of_inatural_term (get_property t noid "goo"))
		   then raise (Testfailed 6);

		put_property t noid "foo" (inatural_term 5);
		if not (5 = number_of_inatural_term (get_property t noid "foo"))
		   then raise (Testfailed 7);

		remove_property t noid "goo";
                (let failp = ref false in
		  (try (get_property t noid "goo"); () with e -> failp := true);
		  if (not !failp) then raise (Testfailed 8));

 		raise Pleasefail)))

	  with
	    Pleasefail ->
	     (with_transaction lib
	      (function t ->
		(if not (3 = number_of_inatural_term (get_property t noid "foo"))
		   then raise (Testfailed 9)
		   else ())))
	   | e -> raise e;

	(with_transaction lib
	    (function t ->
		if not (2 = number_of_inatural_term (get_term t noid))
		   then raise (Testfailed 10);
		put_term t noid (inatural_term 3);
		if not (3 = number_of_inatural_term (get_term t noid))
		   then raise (Testfailed 11)
	    ));

	(try
	  let f = (with_transaction lib
		    (function t ->
		      (* cause failure by using dead transaction *)
		      (function () -> roots t))) in
       	    (f (); raise (Testfailed 115))
	  with e -> ());

	(with_transaction lib
	    (function t ->
		if not (3 = number_of_inatural_term (get_term t noid))
		   then raise (Testfailed 12)));

     (lib_close lib); ())

     (function () -> (lib_close lib)))

  ; print_newline()
  ; print_endline "property test successful, (the failures were part of the test)."


(* make_directory
   insert
   make_leaf
   directory_p
   deactivate activate
   roots root children child descendent
*)
let dtest connection =
  let lib = join connection ["testall"] in
    (unwind_error
      (function () ->

	(* test get *)
	let doid = (with_transaction lib
		   (function t ->
			 let oid = root t "demo" in
			   (let toid = make_directory t oid "test"
	 		    and coid =  create t "TERM" (Some (inatural_term 1)) [] in
			     insert t toid "test1" coid
			     ; (let coid2 = make_leaf t toid "TEST2" "TERM" in
				 put_term t coid2 (inatural_term 2)))
			   ; oid)) in

	(with_local_transaction lib
	   (function t ->
             let toid = child t doid "test" in
		if not (directory_p t toid) then raise (Testfailed 14)));

	(with_transaction lib
	   (function t ->
             let toid = child t doid "test"in
		deactivate t toid;
		if (directory_p t toid) then raise (Testfailed 15)));

	(with_local_transaction lib
	   (function t ->
             let toid = child t doid "test" in
		if (directory_p t toid) then raise (Testfailed 16)));

	(with_transaction lib
	   (function t ->
             let toid = child t doid "test" in
		activate t toid;
		if not (directory_p t toid) then raise (Testfailed 17)));

	(with_local_transaction lib
	   (function t ->
             let toid = child t doid "test" in
		if not (directory_p t toid) then raise (Testfailed 18)));

	(with_local_transaction lib
	   (function t ->
		if not (1 = List.length (roots t)) then raise (Testfailed 19)));

	(with_transaction lib
	   (function t ->
		let t2 = descendent t doid ["test"; "TEST2"] in
		 if not (2 = number_of_inatural_term (get_term t t2)) then raise (Testfailed 20)));

	(with_transaction lib
	   (function t ->
	      let toid = descendent t doid ["test"] in
		if not (2 = List.length (children t toid)) then raise (Testfailed 21);
		remove_leaf t toid "TEST2";
		if not (1 = List.length (children t toid)) then raise (Testfailed 22);
		remove_directory t doid "test";
		get_term t toid;
		disallow_collection t toid;
		delete t toid;
		get_term t toid;
		allow_collection t toid;
		delete_strong t toid;
		try (get_term t toid; raise (Testfailed 23)) with e -> ()
	    ));

     (lib_close lib); ())

     (function () -> (lib_close lib)))

  ; print_newline()
  ; print_endline "activate test successful."



let looptest connection =
  let lib = join connection ["metaprl"] in
  (* let lib = join connection ["nltestl"] in  *)
    (unwind_error
      (function () ->

	(with_transaction lib
	   (function t ->
		(eval t
		 (null_ap (itext_term "\l. inform_message nil ``Metaprl Loop Start`` nil")))))

	; server_loop lib
	; leave lib
	)

     (function () -> leave lib))
;;


let toptestloop libhost remote_port local_port =
 print_newline();
 print_newline();
 print_endline "TestLoop Called ";

  (let connection = connect libhost remote_port local_port in

    (unwind_error
      (function () ->
	looptest connection
	)
      (function () -> disconnect connection))

    ; disconnect connection)

 ; print_string "TestLoop DONE"
 ; print_newline()
 ; print_newline()
;;

let testascii libhost remote_port local_port =
 print_newline();
 print_newline();
 print_endline "TestAscii Called ";

  (let connection = connect libhost remote_port local_port in

    (unwind_error
      (function () ->
	looptest connection
	)
      (function () -> disconnect connection))

    ; disconnect connection)

 ; print_string "TestAscii DONE"
 ; print_newline()
 ; print_newline()
;;


let testall libhost remote_port local_port =
 print_newline();
 print_newline();
 print_endline "TestAll Called ";

  let cookie = ref "" in
  (let connection = connect libhost remote_port local_port in

    (unwind_error
      (function () ->
	  cookie := oiljgs connection
        ; dtest connection
	; cookie := seri connection !cookie
	; ptest connection
	; looptest connection
	)
      (function () -> disconnect connection))

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
	     && (i+1) = number_of_inatural_term (get_property t oid "foo"))
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

  (let connection = connect "LOCKE" remote_port local_port in

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

  (let connection = connect "LOCKE" remote_port local_port in

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

open List
open Refiner.Refiner.Term
open Library_eval

let faux_refine g t =

    print_newline();
    Mbterm.print_term g;
    print_newline();
    [g; g]
;;


special_error_handler (function () -> (library_open_and_loop_eval "lNuprlLight" faux_refine))
 (fun s t -> print_string s; print_newline(); Mbterm.print_term t)


(*
special_error_handler (function () -> testascii "LOCKE" 5289 5289)
  (fun s t -> print_string s; print_newline(); Mbterm.print_term t)


special_error_handler (function () -> testall "ALFHEIM" 3448 4688)
 (fun s t -> print_string s; print_newline(); Mbterm.print_term t)


special_error_handler (function () ->
			( Db.db_init "/home/nuprl/nuprl5/NuPrlDB" true
			; Mbterm.print_term (Db.session_string_to_term "l!l{1:n}ltvoid()t")
			;()
			))
 (fun s t -> print_string s; print_newline(); Mbterm.print_term t)


special_error_handler (function () ->
			( maybe_lib_open()
			; Mbterm.print_term (Db.session_string_to_term "l!l{1:n}ltvoid()t")
			;()))
 (fun s t -> print_string s; print_newline(); Mbterm.print_term t)

special_error_handler
  (function () ->
 	   Mbterm.print_term (Db.string_to_term "!level_assoc(!l{0:n};!data_persist{STATIC:t}(!stamp{6740:n,3091287743:time,44:n,1900031899_3D6C_B8413DA9:t}))")
	   ;())
  (fun s t -> print_string s; print_newline(); Mbterm.print_term t)

special_error_handler
  (function () ->
 	let s = (make_scanner "[|]"  "\n\t\r " (Stream.of_string "[0]")) in
          print_string (List.hd (scan_char_delimited_list s (function () -> scan_string s) '[' ']' '|'))
	   ;())
  (fun s t -> print_string s; print_newline(); Mbterm.print_term t)



special_error_handler (function () -> testall "DENEB" 3444 4444)
 (fun s t -> print_string s; print_newline(); Mbterm.print_term t)

special_error_handler (function () -> maybe_lib_open())
 (fun s t -> print_string s; print_newline(); Mbterm.print_term t)


special_error_handler (function () -> disconnect (connect "BOLVERK" 5289 5291); ())
 (fun s t -> print_string s; print_newline(); Mbterm.print_term t)

special_error_handler (function () -> testall "ALFHEIM" 7289 2897)
 (fun s t -> print_string s; print_newline(); Mbterm.print_term t)

special_error_handler (function () -> jointest 5289 2895)
 (fun s t -> print_string s; print_newline(); Mbterm.print_term t)

special_error_handler (function () -> test 5289 2895)
 (fun s t -> print_string s; print_newline(); Mbterm.print_term t)
*)


