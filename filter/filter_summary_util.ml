(*
 * Utilities for the filter_summary module.
 *)

open Printf

open Term
open Opname
open Simple_print
open Filter_ast
open Filter_type
open Filter_util
open Filter_summary
open Filter_summary_io
open Filter_cache

open File_base_type
open File_type_base

(************************************************************************
 * FILTER CACHE                                                         *
 ************************************************************************)

(*
 * This type may contain something useful after a while.
 *)
type imp_proof = MLast.expr

(*
 * For this compiler, we only use two summaries.
 *)
type select_type =
   InterfaceType
 | ImplementationType

(*
 * The interface type and implementation proofs.
 *)
type proof_type =
   InterfaceProof
 | ImplementationProof of imp_proof

(*
 * Build the cache.
 *)
module FileTypes =
struct
   type select = select_type
   type cooked = proof_type module_info
end
       
module InterfaceInfo =
struct
   type select = select_type
   type raw = unit module_info
   type cooked = proof_type module_info
   
   let select = InterfaceType
   let suffix = "cmiz"
   let magic = 0x73ac6be1
   let marshal info =
      let marshal_proof kind = function
         InterfaceProof ->
            ()
       | ImplementationProof _ ->
            raise (Invalid_argument (sprintf "Filter_summary_util.InterfaceInfo.marshal: %s" kind))
      in
         proof_map marshal_proof info
   
   let unmarshal info =
      let unmarshal_proof kind () = InterfaceProof in
         proof_map unmarshal_proof info
end

module ImplementationInfo =
struct
   type select = select_type
   type raw = imp_proof module_info
   type cooked = proof_type module_info
   
   let select = ImplementationType
   let suffix = "cmoz"
   let magic = 0x73ac6be2
   let marshal info =
      let marshal_proof kind = function
         InterfaceProof ->
            raise (Invalid_argument (sprintf "Filter_summary_util.ImplementationInfo.marshal: %s" kind))
       | ImplementationProof pf ->
            pf
      in
         proof_map marshal_proof info
   
   let unmarshal info =
      let unmarshal_proof kind pf = ImplementationProof pf in
         proof_map unmarshal_proof info
end

module SummaryTypes =
struct
   type proof = proof_type
   type select = select_type
end

module InterfaceCombo = MakeSingletonCombo (InterfaceInfo)
module Combo = ExtendCombo (ImplementationInfo) (InterfaceCombo)
module FileBase = MakeFileBase (FileTypes) (Combo)
module SummaryBase = MakeSummaryBase (SummaryTypes) (FileBase)
module FilterCache = MakeFilterCache (SummaryBase)

(************************************************************************
 * CONTEXT OPERATORS
 ************************************************************************)

(*
 * Distinguish between context var parameters, var names,
 * and other parameters.
 *)
let extract_params cvars bvars =
   let aux h =
      if is_var_term h then
         let v = dest_var h in
            if List.mem v cvars then
               ContextParam v
            else if List.mem v bvars then
               VarParam v
            else
               TermParam h
      else
         TermParam h
   in
      List.map aux

(*
 * Param expression.
 *)
let param_expr loc = function
   ContextParam s ->
      <:expr< $uid:"Filter_summary"$ . $uid:"ContextParam"$ $str:s$ >>
 | VarParam v ->
      <:expr< $uid:"Filter_summary"$ . $uid:"VarParam"$ $str:v$ >>
 | TermParam t ->
      let t' = build_ml_term loc t in
         <:expr< $uid:"Filter_summary"$ . $uid:"TermParam"$ $t'$ >>
                                           
(*
 * Create function type.
 *)
let params_ctyp loc ctyp params =
   let rec convert = function
      [] -> ctyp
    | h::t ->
         let ctyp' = convert t in
         let arg_type =
            match h with
               ContextParam _ ->
                  <:ctyp< $lid:"int"$ >>
             | VarParam _ ->
                  <:ctyp< $lid:"string"$ >>
             | TermParam _ ->
                  <:ctyp< $uid:"Term"$ . $lid:"term"$ >>
         in
            <:ctyp< $arg_type$ -> $ctyp'$ >>
   in
      convert params

(************************************************************************
 * OPNAMES                                                              *
 ************************************************************************)

(*
 * $Log$
 * Revision 1.2  1997/08/06 16:17:35  jyh
 * This is an ocaml version with subtyping, type inference,
 * d and eqcd tactics.  It is a basic system, but not debugged.
 *
 * Revision 1.1  1997/04/28 15:51:00  jyh
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
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
