(*
 * Create an ediable rewrite object.
 *)

open Filter_summary

include Shell_type
include Package_info
include Package_df

(*
 * This is the actual rewrite object.
 *)
type rewrite =
   { mutable rw_params : param list;
     mutable rw_assums : term list;
     mutable rw_redex : term;
     mutable rw_contractum : term;
     mutable rw_ped : Proof_edit.t
   }

(*
 * The object has a package in scope.
 *)
let create pack prog name =
   let obj =
      { rw_assums = [];
        rw_params = [];
        rw_redex = unit_term;
        rw_contractum = unit_term;
        rw_proof = null_proof
      }
   in
   let mk_goal () =
      let { rw_assums = assums;
            rw_params = params;
            rw_redex = redex;
            rw_contractum = contractum
          } = obj
      in
         { crw_name = name;
           crw_params = params;
           crw_args = assums;
           crw_redex = redex;
           crw_contractum = contractum;
           crw_proof = null_proof
         }
   in
   let edit_format db buf =
      (* Convert to a term *)
      let t = term_of_rewrite convert_impl (mk_rewrite_for_display ()) in
         format_term db buf t
   in
   let edit_set_goal t =
      raise (Failure "Shell_rewrite.edit_set_goal: bogus edit")
   in
   let edit_set_redex t =
      obj.rw_redex <- t
   in
   let edit_set_contractum t =
      obj.rw_contractum <- t
   in
   let edit_set_assumptions tl =
      obj.rw_assums <- tl
   in
   let edit_set_params pl =
      obj.rw_params <- pl
   in
   let edit_check () =
      (*
       * Install the proof.
       * Get the extract from the proof, and add the rule to the refiner.
       *)
      let { rw_assums = assums;
            rw_params = params;
            rw_redex = redex;
            rw_contractum = contractum;
            rw_ped = ped
          } = obj
      in
      
      


(*
 * $Log$
 * Revision 1.3  1998/04/22 14:06:28  jyh
 * Implementing proof editor.
 *
 * Revision 1.2  1998/04/21 20:57:58  jyh
 * Fixed typing problems introduced by refiner msequents.
 *
 * Revision 1.1  1998/04/17 20:48:16  jyh
 * Updating refiner for extraction.
 *
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
