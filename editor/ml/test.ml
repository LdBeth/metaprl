(*
 * Display all the elements in a particular theory.
 *)

include Itt_theory

interactive test_and_intro 'H :
   sequent ['ext] { 'H >- 'A } -->
   sequent ['ext] { 'H >- 'B } -->
   sequent ['ext] { 'H >- 'A & 'B }

interactive test_imp_elim 'H 'J 'y :
   sequent ['ext] { 'H; x: 'A => 'B; 'J['x] >- 'A } -->
   sequent ['ext] { 'H; x: 'A => 'B; 'J['x]; y: 'B >- 'C['x] } -->
   sequent ['ext] { 'H; x: 'A => 'B; 'J['x] >- 'C['x] }

interactive test_and_elim 'H 'J 'u 'v :
   sequent ['ext] { 'H; u: 'A; v: 'B; 'J['u, 'v] >- 'C['u, 'v] } -->
   sequent ['ext] { 'H; x: 'A & 'B; 'J['x] >- 'C['x] }

(*
 * -*-
 * Local Variables:
 * Caml-master: "editor.top"
 * End:
 * -*-
 *)
