(*
 * Compute relationships between theories.
 *
 * $Log$
 * Revision 1.1  1997/08/06 16:17:15  jyh
 * This is an ocaml version with subtyping, type inference,
 * d and eqcd tactics.  It is a basic system, but not debugged.
 *
 * Revision 1.2  1996/10/23 15:17:49  jyh
 * First working version of dT tactic.
 *
 * Revision 1.1  1996/09/02 19:33:16  jyh
 * Semi-working package management.
 *
 * Revision 1.3  1996/06/11 18:22:31  jyh
 * Demo version 0.0
 *
 * Revision 1.2  1996/05/21 02:25:23  jyh
 * This is a semi-working version before Wisconsin vacation.
 *
 * Revision 1.1  1996/05/20 16:59:46  jyh
 * This is an intermediate form of the editor with packages
 * before debugging.  Will be removing theoryGraph files next.
 *
 * Revision 1.1  1996/05/01 15:04:33  jyh
 * This is the initial checkin of the NuprlLight editor.  This editor provides
 * an emacs interface, a library navigator, and a proof editor.
 *
 *)

open Printf;;
open Util;;
open Term;;
open Refine;;
open ImpDAG;;
open Theory;;
open DformPrint;;
open FilterSummary;;
open PackageInfo;;

(************************************************************************
 * IMPLEMENTATION                                                       *
 ************************************************************************)

(*
 * Get the parents that a theory inherits from.
 *)
let compute_parents =
   let rec aux parents refiner =
      if is_null_refiner refiner then
         parents
      else
         let item, next = dest_refiner refiner in
            match item with
               RIParent p -> aux (p::parents) next
             | _ -> aux parents next
   in
      aux [];;

(*
 * Expand the list of theories to contain all parents also.
 * This is in case package have been created that have not
 * been recorded as theories.
 *)
let complete_with_parents =
   (* Add the parent refiner for thy to the packages list *)
   let add_parent mod_info packages refiner =
      let rec aux = function
	 h::t ->
            let r = package_refiner h in
               if r == refiner then
                  (* Found the package containing the refiner *)
                  packages
               else
                  aux t
       | [] ->
            (* No package containing the refiner, so make a new one *)
            let name = package_name mod_info in
            let dformer = package_dforms mod_info in
               fprintf stderr "Package %s.parent\n" name;
               packages @ [new_package { thy_name = name ^ ".parent";
                                         thy_refiner = refiner;
                                         thy_dformer = dformer;
                                         thy_id = ignore_id
                           }]
      in
         aux packages
   in

   (*
    * Keep expanding the packages.
    * There are only a finite number, so this must eventually terminate.
    *)
   let rec expand i packages =
      let len = List.length packages in
         if i < len then
            let mod_info = nth packages i in
            let parents = compute_parents (package_refiner mod_info) in
            let packages' = it_list (add_parent mod_info) packages parents in
               expand (i + 1) packages'
         else
            packages
   in
      expand 0;;

(*
 * Compute the inheritance diagram.
 * The sweep function determines whether to sweep up or down.
 * If sweep up, it computes the class roots.
 * If sweep down, it computes the class leaves.
 *)
let compute_inheritance_aux sweep packages =
   (* Add hidden theories, then create the DAG with just the nodes *)
   let packages' = complete_with_parents packages in
   let dag = ImpDAG.new () in
   let entries = map (insert dag) packages' in

   (* Make association list to map refiner -> node *)
   let make_rnode pack node = package_refiner pack, node in
   let rnodes = map2 make_rnode packages' entries in
      
   (* Add all the parent edges for a particular refiner *)
   let add_edges (refiner, node) =
      let add_edge parent =
         ImpDAG.add_edge dag node (assq parent rnodes)
      in
         do_list add_edge (compute_parents refiner)
   in
   let _ = do_list add_edges rnodes in

   (* Create the tree by sweeping_up *)
   let convert mod_info children =
      { pnode_package = mod_info; pnode_children = children }
   in
      sweep dag convert;;

let compute_roots =
   compute_inheritance_aux sweep_up_all;;

let compute_leaves =
   compute_inheritance_aux sweep_down_all;;

(*
 * Rather than computing the inheritance graph, compute the definition
 * graph, which shows where packages are defined.
 *
 * Right now, this is a pointwise graph since we don't have any
 * subpackages.
 *)
let compute_definition packages =
   let convert mod_info =
      { pnode_package = mod_info; pnode_children = [] }
   in
      map convert packages;;

(*
 * -*-
 * Local Variables:
 * Caml-master: "editor.top"
 * End:
 * -*-
 *)
