(*
 * Debugging functions for rewrite stat types.
 *
 * ----------------------------------------------------------------
 *
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/index.html for information on Nuprl,
 * OCaml, and more information about this system.
 *
 * Copyright (C) 1998 Jason Hickey, Cornell University
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
 * Author: Jason Hickey
 * jyh@cs.cornell.edu
 *)
INCLUDE "refine_error.mlh"

open Lm_printf
open Lm_debug
open Lm_symbol

open Opname
open Term_sig
open Term_base_sig
open Term_addr_sig
open Refine_error_sig
open Rewrite_sig

(*
 * Show the file loading.
 *)
let _ =
   show_loading "Loading Rewrite_debug%t"

module MakeRewriteDebug
   (TermType : TermSig)
   (Term : TermBaseSig with module TermTypes = TermType)
   (TermAddr : TermAddrSig with module AddrTypes = TermType)
   (RefineError : RefineErrorSig with module ErrTypes.Types = TermType)
   =
struct
   module RewriteTypes = Rewrite_types.MakeRewriteTypes(TermType)(TermAddr)
   open TermType
   open Term
   open TermAddr
   open RewriteTypes

   type rwterm  = RewriteTypes.rwterm
   type rstack  = RewriteTypes.rstack
   type stack   = RewriteTypes.stack
   type varname = RewriteTypes.varname
   type rwcontractum = RewriteTypes.rwcontractum

   (*
    * Name in the stack.
    *)
   let print_varname out = function
      StackName i ->
         fprintf out "stack:%d" i
    | SaveName i ->
         fprintf out "save:%d" i

   let print_varname_list =
      print_any_list print_varname

   (*
    * Print a stack item.
    * We can't print terms.
    *)
   let print_stack_item out = function
      StackVoid ->
         fprintf out "Void"
    | StackNumber n ->
         fprintf out "Number %s" (Lm_num.string_of_num n)
    | StackString s ->
         fprintf out "String %s" s
    | StackVar v ->
         fprintf out "Var %a" output_symbol v
    | StackLevel _ ->
         fprintf out "Level"
    | StackBTerm (t, vars) ->
         fprintf out "BTerm %a[%a]" print_term t output_symbol_list vars
    | StackITerm ts ->
         fprintf out "ITerm %a" (print_any_list (fun out (t, subterms) -> fprintf out "%a[%d] " print_term t (List.length subterms))) ts
    | StackContext (vars, t, addr) ->
         fprintf out "Context (%a/%a/%s)" (**)
            output_symbol_list vars
            print_term t
            (string_of_address addr)
    | StackSeqContext (vars, (i, len, _)) ->
         fprintf out "SeqContext (%a/(%d,%d))" (**)
            output_symbol_list vars i len
    | StackContextRestrict (vars) ->
         fprintf out "ContextRestrict (%a)" output_symbol_list (SymbolSet.to_list vars)

   (*
    * Stack is printed on lines.
    *)
   let print_stack out stack =
      let print_item item =
         output_char out '\t';
         print_stack_item out item;
         eflush out
      in
         fprintf out "Stack: %d%t" (Array.length stack) eflush;
         Array.iter print_item stack

   let string_of_conts conts =
      String.concat "," (List.map string_of_symbol conts)

   (*
    * Redex stack names.
    *)
   let rstack_item_str = function
      FreeFOVarPattern v ->
         "FreeFOVarPattern " ^ string_of_symbol v
    | FreeFOVarInstance v ->
         "FreeFOVarInstance " ^ string_of_symbol v
    | SOVarPattern (v, conts, i) ->
         sprintf "SOVarPattern %s<%s>[%d]" (string_of_symbol v) (string_of_conts conts) i
    | SOVarInstance (v, conts, i) ->
         sprintf "SOVarInstance %s<%s>[%d]" (string_of_symbol v) (string_of_conts conts) i
    | FOVar v ->
         "FOVar " ^ (string_of_symbol v)
    | CVar (v, conts, i) ->
         sprintf "CVar %s<%s>[%d]" (string_of_symbol v) (string_of_conts conts) i
    | PVar (v, _) ->
         "PVar " ^ string_of_symbol v ^ ":*"

   let print_rstack out stack =
      let print_item item =
         output_char out '\t';
         output_string out (rstack_item_str item);
         eflush out
      in
         fprintf out "RStack: %d%t" (Array.length stack) eflush;
         Array.iter print_item stack

   (*
    * Parameters.
    *)
   let rec print_param out = function
      RWNumber n ->
         fprintf out "%s:n" (Lm_num.string_of_num n)
    | RWString s ->
         fprintf out "%s:s" s
    | RWToken s ->
         fprintf out "%s:t" s
    | RWQuote ->
         fprintf out "q"
    | RWMNumber i ->
         fprintf out "@@%d:n" i
    | RWMString i ->
         fprintf out "@@%d:s" i
    | RWMToken i ->
         fprintf out "@@%d:t" i
    | RWMLevel1 i ->
         fprintf out "@@%d:l" i
    | RWMLevel2 { rw_le_const = c; rw_le_vars = vars } ->
         fprintf out "(%d" c;
         List.iter (fun { rw_le_var = v; rw_le_offset = o } ->
               fprintf out " [%d %d]" v o) vars;
         fprintf out ")"
    | RWMVar i ->
         fprintf out "@@%d:v" i
    | RWObId _ ->
         fprintf out "ObId"
    | RWParamList pl ->
         fprintf out "[%a]" print_param_list pl

   and print_param_list out pl =
      let rec collect = function
         [h] ->
            print_param out h
       | h::t ->
            fprintf out "%a; %a" print_param h print_param_list t
       | [] ->
            ()
      in
         collect pl

   let rec print_sparam out h = match dest_param h with
      Number n ->
         fprintf out "%s:n" (Lm_num.string_of_num n)
    | String s ->
         fprintf out "%s:s" s
    | Token s ->
         fprintf out "%s:t" s
    | Var s ->
         fprintf out "%s:v" (Lm_symbol.string_of_symbol s)
    | Quote ->
         fprintf out "q"
    | ParamList pl ->
         fprintf out "[%a]" print_sparam_list pl
    | _ ->
         raise (Invalid_argument "Rewrite_debug.print_sparam")

   and print_sparam_list out pl =
      let rec collect = function
         [h] ->
            print_sparam out h
       | h::t ->
            fprintf out "%a; %a" print_sparam h print_sparam_list t
       | [] ->
            ()
      in
         collect pl

   (*
    * Tab to the tabstop.
    *)
   let tab out stop =
      for _i = 0 to stop do
         output_char out ' '
      done

   (*
    * Print the rewrite program.
    *)
   let rec print_prog tabstop out prog =
      tab out tabstop;
      match prog with
         RWComposite { rw_op = op; rw_bterms = bterms } ->
            fprintf out "RWComposite %a\n%a" (**)
               print_op op
               (print_bterms (tabstop + 3)) bterms
       | RWCompositeSimple { rws_op = op; rws_bterms = bterms } ->
            fprintf out "RWCompositeSimple %a\n%a" (**)
               print_sop op
               (print_bterms (tabstop + 3)) bterms
       | RWSequent (arg, hyps, concl) ->
            fprintf out "RWSequent:\n%aArg:\n%a%aHyps:\n%a%aGoals:\n%a" (**)
               tab tabstop
               (print_prog (tabstop + 3)) arg
               tab tabstop
               (print_seq_prog (tabstop + 3)) hyps
               tab tabstop
               (print_prog (tabstop + 3)) concl
       | RWSOVar (i, il) ->
            fprintf out "RWSOVar (%d, %a)\n" i print_int_list il
       | RWSOInstance (i, tl) ->
            fprintf out "RWSOInstance (%d)\n%a" i (print_prog_list (tabstop + 3)) tl
       | RWSOContext (i, j, t, il) ->
            fprintf out "RWSOContext (%d, %d, [%a])\n%a" (**)
               i j print_int_list il
               (print_prog (tabstop + 3)) t
       | RWSOContextSubst (i, t, tl) ->
            fprintf out "RWSOContextSubst %d\n%a\n%a" (**)
               i
               (print_prog (tabstop + 3)) t
               (print_prog_list (tabstop + 3)) tl
       | RWFreeVars (t,il1,il2) ->
            fprintf out "RWFreeVars <%a> [%a]\n%a" print_int_list il1 print_int_list il2 (print_prog (tabstop+3)) t
       | RWMatchFreeFOVar (i, cs, is) ->
            fprintf out "RWMatchFreeFOVar <%d> [%a] [%a]\n" i print_int_list cs print_int_list is
       | RWCheckVar i ->
            fprintf out "RWCheckVar %d\n" i
       | RWStackVar i ->
            fprintf out "RWStackVar %d\n" i

   and print_prog_list tabstop out tl =
      List.iter (print_prog tabstop out) tl

   and print_bterm tabstop out { rw_bvars = bvars; rw_bnames = bnames; rw_bterm = t } =
      tab out tabstop;
      fprintf out "Bterm %d(%a)\n%a" (**)
         bvars
         print_varname_list bnames
         (print_prog (tabstop + 3)) t

   and print_bterms tabstop out bterms =
      List.iter (print_bterm tabstop out) bterms

   and print_seq_prog_item tabstop out item =
      tab out tabstop;
      match item with
         RWSeqContext (i, j, il) ->
            fprintf out "RWSeqContext (%d, %d, [%a])\n" i j print_int_list il
       | RWSeqContextInstance (i, tl) ->
            fprintf out "RWSeqContextInstance (%d)\n%a" (**)
               i (print_prog_list (tabstop + 3)) tl
       | RWSeqHyp (v, t) ->
            fprintf out "RWSeqHyp: %a\n%a" (**)
               print_varname v
               (print_prog (tabstop + 3)) t
       | RWSeqFreeVarsContext (ilc, ilv, i, j, il) ->
            fprintf out "RWSeqFreeVarsContext (<%a>%a): %d, %d, [%a]\n"
               print_int_list ilc print_int_list ilv i j print_int_list il

   and print_seq_prog tabstop out items =
      List.iter (print_seq_prog_item tabstop out) items

   and print_op out { rw_name = opname; rw_params = params } =
      fprintf out "%s%a" (string_of_opname opname) (**)
         print_param_list params

   and print_sop out op =
      let op' = dest_op op in
         output_string out (string_of_opname op'.op_name);
         if op'.op_params <> [] then
            fprintf out "[%a]" print_sparam_list op'.op_params

   let print_prog = print_prog 0

   let print_strict out strict =
      let s =
         match strict with
            Strict -> "Strict"
          | Relaxed -> "Relaxed"
      in
         fprintf out "%s" s

   let print_any_array print out l =
      match l with
         [||] ->
            ()
       | [|x|] ->
            print out x
       | _ ->
            let len = Array.length l in
               fprintf out "@[<b 3>%a" print l.(0);
               for i = 1 to pred len do
                  fprintf out ",@ %a" print l.(i)
               done;
               fprintf out "@]"

   let print_contractum out con =
      match con with
         RWCFunction _ ->
            fprintf out "<function>"
       | RWCTerm (progs, vars) ->
            fprintf out "@[<v 3>[%a]" (print_any_array output_symbol) vars;
            List.iter (fun prog -> fprintf out "@ %a" print_prog prog) progs;
            fprintf out "@]"
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
