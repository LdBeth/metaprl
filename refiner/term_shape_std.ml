(*
 * Compute the "shape" of the term that can be used for reductions.
 * Terms are reduced to these templates for indexing
 * purposes.  Each template just contains information
 * about the opname, the order and types of params,
 * and the arties of the subterms.
 *)

open Printf

open Opname
open Term_std

module TermShape =
struct
   open Term
   
   type term = Term.term

   type shape =
      { shape_opname : opname;
        shape_params : shape_param list;
        shape_arities : int list
      }
   
   and shape_param =
      ShapeNumber
    | ShapeString
    | ShapeToken
    | ShapeLevel
    | ShapeVar
   
   (*
    * When computing the shape, we don't allow meta-parameters.
    * Raises Invalid_argument if this happens.
    *)
   let shape_of_term { term_op = { op_name = name; op_params = params }; term_terms = bterms } =
      let param_type = function
         Number _ -> ShapeNumber
       | String _ -> ShapeString
       | Token _ -> ShapeToken
       | Level _ -> ShapeLevel
       | Var _ -> ShapeVar
       | MNumber _ -> ShapeNumber
       | MString _ -> ShapeString
       | MToken _ -> ShapeToken
       | MLevel _ -> ShapeLevel
       | MVar _ -> ShapeVar
       | _ ->
            raise (Invalid_argument "Term.shape_of_term")
      in
      let bterm_type { bvars = vars } =
         List.length vars
      in
         { shape_opname = name;
           shape_params = List.map param_type params;
           shape_arities = List.map bterm_type bterms
         }
   
   let print_shape out { shape_opname = name; shape_params = params; shape_arities = arities } =
      let print_param param =
         let s =
            match param with
               ShapeNumber ->
                  "N"
             | ShapeString ->
                  "S"
             | ShapeToken  ->
                  "T"
             | ShapeLevel  ->
                  "L"
             | ShapeVar    ->
                  "V"
         in
            output_string out s
      in
      let rec print_arity out = function
         [i] ->
            fprintf out "%d" i
       | i::t ->
            fprintf out "%d;%a" i print_arity t
       | [] ->
            ()
      in
         output_string out (flat_opname name);
         output_string out "[";
         List.iter print_param params;
         output_string out "]{";
         print_arity out arities;
         output_string out "}"
end

(*
 * $Log$
 * Revision 1.1  1998/05/27 15:14:47  jyh
 * Functorized the refiner over the Term module.
 *
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
