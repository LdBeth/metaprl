(*
 * Display a theory graph on standard out.
 *
 * $Log$
 * Revision 1.1  1997/08/06 16:17:04  jyh
 * This is an ocaml version with subtyping, type inference,
 * d and eqcd tactics.  It is a basic system, but not debugged.
 *
 * Revision 1.1  1996/09/02 19:32:49  jyh
 * Semi-working package management.
 *
 * Revision 1.2  1996/05/21 02:24:55  jyh
 * This is a semi-working version before Wisconsin vacation.
 *
 * Revision 1.1  1996/05/20 16:59:27  jyh
 * This is an intermediate form of the editor with packages
 * before debugging.  Will be removing theoryGraph files next.
 *
 * Revision 1.1  1996/05/01 15:03:58  jyh
 * This is the initial checkin of the NuprlLight editor.  This editor provides
 * an emacs interface, a library navigator, and a proof editor.
 *
 *)

open Rformat;;
open Refine;;
open Theory;;
open DformPrint;;
open PackageInfo;;
open PackageGraph;;

(************************************************************************
 * IMPLEMENTATION                                                       *
 ************************************************************************)

(*
 * Display the graph on the standard output.
 * We display it as a tree, with indentation to specify the
 * branching.
 *)
let display_package_graph buf nodes =
   let rec display_node mod_info =
      let name = package_name mod_info.pnode_package in
      let children = mod_info.pnode_children in
	 format_string buf name;
	 if children <> [] then
         begin
            format_string buf "\n  ";
            format_pushm buf 0;
            display_nodes' children;
            format_popm buf;
         end
   and display_node' mod_info =
      let name = package_name mod_info.pnode_package in
      let children = mod_info.pnode_children in
         format_string buf name;
         if not (memq mod_info nodes) & children <> [] then
	 begin
	    format_string buf "\n  ";
	    format_pushm buf 0;
	    display_nodes children;
	    format_popm buf;
	 end
   and display_nodes = function
      [h] ->
         display_node h
    | h::t ->
         display_node h;
         format_newline buf;
         display_nodes t
    | [] -> ()
   and display_nodes' = function
      [h] ->
         display_node' h
    | h::t ->
         display_node' h;
         format_newline buf;
         display_nodes' t
    | [] -> ()
   in
      display_nodes nodes;;

(*
 * -*-
 * Local Variables:
 * Caml-master: "editor.top"
 * End:
 * -*-
 *)
