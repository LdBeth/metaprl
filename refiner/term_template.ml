(*
 * This module computes term templates for use by hashtables.
 *
 * $Log$
 * Revision 1.1  1997/04/28 15:51:48  jyh
 * This is the initial checkin of Nuprl-Light.
 * I am porting the editor, so it is not included
 * in this checkin.
 *
 * Directories:
 *     refiner: logic engine
 *     filter: front end to the Ocaml compiler
 *     editor: Emacs proof editor
 *     util: utilities
 *     mk: Makefile templates
 *
 * Revision 1.1  1996/11/13 22:59:16  jyh
 * Initial version of forward/backward chaining cache.
 *
 *)

open Opname
open Term

(************************************************************************
 * TYPES                                                                *
 ************************************************************************)

(*
 * Terms are reduced to these templates for indexing
 * purposes.  Each templet just contains information
 * about the opname, the order and types of params,
 * and the arties of the subterms.
 *)
type local_term_template =
   { template_opname : opname;
     template_params : localParamTemplate array;
     template_arities : (int * opname) array
   }

and localParamTemplate =
   Number
 | String
 | Token
 | Level
 | Var

type term_template = int

(************************************************************************
 * IMPLEMENTATION                                                       *
 ************************************************************************)

(*
 * Compute the template for a particular term.
 *)
let local_compute_template t =
   let { term_op = op; term_terms = bterms } = dest_term t in
   let { op_name = opname; op_params = params } = dest_op op in
   let compute_arity bterm =
      let { bvars = bvars; bterm = t } = dest_bterm bterm in
         List.length bvars, opname_of_term t
   in
   let compute_param param =
      match dest_param param with
         Term.Number _ -> Number
       | Term.String _ -> String
       | Term.Token _ -> Token
       | Term.Level _ -> Level
       | Term.Var _ -> Var
       | Term.MNumber _ -> Number
       | Term.MString _ -> String
       | Term.MToken _ -> Token
       | Term.MLevel _ -> Level
       | Term.MVar _ -> Var
       | Term.MSum _
       | Term.MDiff _
       | Term.MProduct _
       | Term.MQuotient _
       | Term.MRem _ -> Number
       | Term.MLessThan _
       | Term.MEqual _
       | Term.MNotEqual _ -> Token
   in
      { template_opname = opname;
        template_params =
           if params = [] then
              [||]
           else
              Array.of_list (List.map compute_param params);
        template_arities = Array.of_list (List.map compute_arity bterms)
      }
(*
 * Integer index from template.
 *)
let compute_template t =
   Hashtbl.hash (local_compute_template t)

(*
 * -*-
 * Local Variables:
 * Caml-master: "editor.run"
 * End:
 * -*-
 *)
