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
 *)

(*
 * This implements a filesystem interface to the Nuprl library.
 *)
open Lm_debug
open Term_sig
open Refiner.Refiner.Term
open Refiner.Refiner.Refine
open Basic
open Utils
open Library
open Nuprl5
open Lm_printf
open Shell_sig
open Opname

module Nuprl (Edit : ShellEditSig) = struct
   exception LibraryException of string

   let itt_bug = ref true
   let _ = show_loading "Loading Nuprl_eval(Edit)%t"
   let library = null_oref ()
   let connection = null_oref ()

   let library_close () =
      if oref_p library then
         (leave (oref_val library);
          Library.disconnect (oref_val connection);
          oref_nullify library)
      else raise (LibraryException "Close: No library open.")

   let library_open_eval servername hook host remoteport =
      if oref_p library then
         raise (LibraryException "Open: Library already open.")

      else
         let _ = oref_set connection (connect servername host remoteport) in
         let _ = oref_set library (join_eval (oref_val connection) ["JPROVER"] hook) in
            at_exit library_close (* jyh: something is strange here *)

   (*
    *
    * MetaPRL cmd syntax
    *
    * !mp_list_root{}() returns !tok_cons
    * !mp_list_module{}(tok_cons) returns !mp_edit{}(!cons(!token{thm:t}(); sequent); !cons ...)
    * !mp_list_display{}(list) returns !ipair_term (name) (idform_term)
    * !mp_lookup_proof{}(symaddr) returns !mp_prf(goal; tactic; children; extras) g and ex are msequent, ch are mp_prf
    * !mp_thm_create{}(!msequent) returns !void()
    * !mp_create_set{}(!tok_cons; sequent) returns !void()
    * !mp_set_goal{}(!tok_cons; sequent) returns !void ()
    * !mp_refine{}(symaddr; address; tactic) returns !mp_ref(goal; subgoals; extras) g, subs and ex are sequent terms
    * !mp_undo{}(symb) returns !mp_prf(goal; tactic; children; extras)
    * !mp_save{}(symb) returns !void()
    *
    *)
   let token s = Token (mk_opname s nil_opname)

   let mp_list_root_op = mk_nuprl5_op [ make_param (token "!mp_list_root")]
   let mp_list_display_op = mk_nuprl5_op [ make_param (token "!mp_list_display")]
   let mp_list_module_op = mk_nuprl5_op [ make_param (token "!mp_list_module")]
   let mp_create_op = mk_nuprl5_op [ make_param (token "!mp_create")]
   let mp_create_rw_op = mk_nuprl5_op [ make_param (token "!mp_create_rw")]
(* unused
   let mp_create_set_op = mk_nuprl5_op [ make_param (token "!mp_create_set")]
   let mp_set_goal_op = mk_nuprl5_op [ make_param (token "!mp_set_goal")]
 *)
   let mp_set_thm_op = mk_nuprl5_op [ make_param (token "!mp_set_thm")]
   let mp_set_rw_op = mk_nuprl5_op [ make_param (token "!mp_set_rw")]
   let mp_lookup_op = mk_nuprl5_op [ make_param (token "!mp_lookup_proof")]
   let mp_refine_op = mk_nuprl5_op [ make_param (token "!mp_refine")]
   let mp_undo_op = mk_nuprl5_op [ make_param (token "!mp_undo")]
   let mp_save_op = mk_nuprl5_op [ make_param (token "!mp_save")]
   let mp_save_thy_op = mk_nuprl5_op [ make_param (token "!mp_save_thy")]
(* unused
   let mp_node_op = mk_nuprl5_op [ make_param (token "!mp_node")]
   let refiner_op = mk_nuprl5_op [ make_param (token "!refine")]
 *)
   let mp_compile_op = mk_nuprl5_op [ make_param (token "!mp_compile")]

(* unused
   let refine_req_p t = opeq refiner_op (operator_of_term t)
 *)

   let current_symaddr = ref []
(* unused
   let current_pair = ref ("foo", "foo")
 *)

(* unused
   let refine_args t =
      match dest_term t with
         {term_op = op; term_terms = goal :: tac :: r } when (opeq op refiner_op)
      -> (term_of_unbound_term goal, term_of_unbound_term tac)
       | _ -> error ["refine_args"; "op"; "unrecognized"] [] [t]
 *)

(* unused
   let iinf_sequent_op = mk_nuprl5_op [make_param (token "!inf_sequent"); Basic.inil_parameter]
   let iinf_sequent_term hyp term = mk_term iinf_sequent_op [(mk_bterm [] hyp); (mk_bterm [Lm_symbol.add ""] term)]
 *)

   let mp_prf_term g tac s e =
      mk_term (mk_nuprl5_op [make_param (token "!mp_prf")])
      [(mk_bterm [] g); (mk_bterm [] tac); (mk_bterm [] s); (mk_bterm [] e)]

   let mp_edit_term w c a r =
      mk_term (mk_nuprl5_op [make_param (token "!mp_edit")])
      [(mk_bterm [] w); (mk_bterm [] c); (mk_bterm [] a); (mk_bterm [] r)]

   let mp_ref_term g s e =
      mk_term (mk_nuprl5_op [make_param (token "!mp_ref")])
      [(mk_bterm [] g); (mk_bterm [] s); (mk_bterm [] e)]

(* unused
   let inatural_cons_op = (mk_nuprl5_op [make_param (token "!natural_cons")])
   let int_list_of_term_old t =
      map_isexpr_to_list_by_op inatural_cons_op number_of_inatural_term t
 *)
   let ipui_addr_cons_op = mk_nuprl5_op [make_param (token "!pui_addr_cons")]

   let int_list_of_term t =
      map_isexpr_to_list_by_op ipui_addr_cons_op number_of_ipui_addr_term t

   let string_list_of_term t =
      map_isexpr_to_list string_of_itoken_term t

   let imp_msequent_op = (mk_nuprl5_op [make_param (token "!mp_msequent")])
   let imp_msequent_term a g = mk_term imp_msequent_op [(mk_bterm [] a); (mk_bterm [] g)]
   let imcons_op = (mk_nuprl5_op [make_param (token "!mcons")])
   let ianno_arg_cons_op = (mk_nuprl5_op [make_param (token "!anno_arg_cons")])

   let msequent_to_term mseq =
      let (goal, hyps) = dest_msequent mseq
      in imp_msequent_term (list_to_ilist_by_op imcons_op hyps) goal

   let mp_objc_to_term oc =
      let (name, status, metagoal, params) = oc in
         Filter_cache.term_of_meta_term metagoal

   let term_to_msequent t =
      match dest_term t with
         { term_op = imp_msequent_op; term_terms = [assums; goal]} ->
            (map_isexpr_to_list_by_op imcons_op (let f x = x in f) (term_of_unbound_term assums),
             (term_of_unbound_term goal))
       | _ ->
            raise (Refiner.Refiner.RefineError.RefineError
                   ("term_to_msequent", (Refiner.Refiner.RefineError.TermMatchError
					 (t, "malformed metasequent"))))

(* unused
   let term_to_symaddr t =
      match string_list_of_term t with
         a::b::c::tl -> (b, c)
       | _ ->
            raise (Refiner.Refiner.RefineError.RefineError
                   ("term_to_symaddr", (Refiner.Refiner.RefineError.TermMatchError
					(t, "malformed msequent"))))
 *)

   let ilist_op = (mk_nuprl5_op [make_param (token "!list")])

   let ipair_op = (mk_nuprl5_op [make_param (token "!pair")])
   let ipair_term h t = mk_term ipair_op [(mk_bterm [] h); (mk_bterm [] t)]

   let list_of_ints length =
      let rec ll len l =
         if len = 0 then l
         else ll (len - 1) (len :: l)
      in (ll length [])

   open List
   open Lm_list_util

   let get_parents_all name =
      let rec f l1 l2 =
         if l1 = [] then l2
         else let l3 = List.map String.uncapitalize_ascii (Edit.list_parents (hd l1)) in
                 f (union l3 (tl l1)) (union l3 l2) in
         f [name] [name]

   let idform_op = mk_nuprl5_op [make_param (token "!dform")]
   let idform_term attr lhs rhs =
      mk_term idform_op [mk_bterm [] attr; mk_bterm [] lhs; mk_bterm [] rhs]

   let imp_prec_pair_op = (mk_nuprl5_op [make_param (token "!mp_prec_pair")])
   let imp_prec_pair_term lhs rhs =
      mk_term imp_prec_pair_op [mk_bterm [] lhs; mk_bterm [] rhs]
   let imp_prec_rel_op r = (mk_nuprl5_op [make_param (token "!mp_prec_rel"); make_param (token r)])
   let imp_prec_rel_term r lhs rhs =
      mk_term (imp_prec_rel_op r) [mk_bterm [] lhs; mk_bterm [] rhs]

   let idform_attr_op = (mk_nuprl5_op [make_param (token "!dform_attr_cons")])
   let imode_cons_op = (mk_nuprl5_op [make_param (token "!mode_cons")])
   let imp_prec_cons_op = (mk_nuprl5_op [make_param (token "!mp_prec_cons")])
   let imp_prec_rel_cons_op = (mk_nuprl5_op [make_param (token "!mp_prec_rel_cons")])

   let ifail_parameter = make_param (token "!fail")
   let ifail_op = mk_nuprl5_op [ifail_parameter]
   let ifail_term t = mk_term ifail_op [mk_bterm [] t]

   let dest_rw_term t =
      match dest_term t with
         { term_op = imp_msequent_op; term_terms = [assums; goal]} ->
            (match dest_term (term_of_unbound_term goal) with
                { term_op = op ; term_terms = [redex; contract]} ->
                   ((term_of_unbound_term redex), (term_of_unbound_term contract))
              | _ ->
                   raise (Refiner.Refiner.RefineError.RefineError
                          ("dest_rw_term", (Refiner.Refiner.RefineError.TermMatchError
                                            ((term_of_unbound_term goal), "malformed rw")))))
       | _ ->
            raise (Refiner.Refiner.RefineError.RefineError
                   ("term_to_msequent", (Refiner.Refiner.RefineError.TermMatchError
					 (t, "malformed metasequent"))))

   let name_of_symbolic_address sa =
      hd (tl (string_list_of_term (term_of_unbound_term sa)))

   let refine_ehook =

      unconditional_error_handler
      (function () ->
            (function t ->
                  let rec visit_proof address =
                     let (tac, goal, subgoals, extras) = Edit.node address in
                        (match tac with
                            Some s -> let length = List.length subgoals in
                                      let vps i = visit_proof (i :: address) in
                                         mp_prf_term (msequent_to_term goal) (itext_term s)
                                         (list_to_ilist (List.map vps (list_of_ints length)))
                                         (list_to_ilist (List.map msequent_to_term extras))

                          | None -> mp_prf_term (msequent_to_term goal) ivoid_term inil_term
                                    (list_to_ilist (List.map msequent_to_term extras))) in

          (*command loop*)
                     (match dest_term t with

                         {term_op = op; term_terms = [] } when (opeq op mp_list_root_op) ->
                            list_to_ilist (map itoken_term (Edit.list_modules ()))

                       | {term_op = op; term_terms = sa :: r } when (opeq op mp_list_module_op) ->
                            let name = name_of_symbolic_address sa in
                            let visit_obj x =
                               (try (Edit.cd_thm name x;
                                     let (tac, goal, subgoals, extras) = Edit.node [] in
                                        ipair_term (itoken_term x) (msequent_to_term goal))
                                with
                                   _ ->
                                      (eprintf "Failed cd'ing into thm %s %s%t" name x eflush;
                                       ipair_term (itoken_term x) (itoken_term "Error cd thm : mp_list_module")))

                            and parents = (try (get_parents_all name)
                                           with
                                              _ ->
                                                 (eprintf "Error! Failed getting parents : %s%t" name eflush;
                                                  [])) in

	      (*return cons of parent list (dependencies of the module) and the objects*)
                               icons_term icons_op
                               (list_to_ilist_map itoken_term parents)

                               (let (w, c, a, rl) = (try (Edit.list_module name)
                                                     with _ -> ([], [], [], [])) in
                                   mp_edit_term (list_to_ilist_map visit_obj w )
                                   (list_to_ilist_map visit_obj c)
                                   (list_to_ilist_map visit_obj a)
                                   (list_to_ilist_map visit_obj rl))

                       | {term_op = op; term_terms = symaddr :: r } when (opeq op mp_list_display_op) ->

                            let get_dform name =
                               (let precs = (try (Edit.list_precs name) with
                                                Not_found -> (eprintf "Error: Precs for %s Not found%t" name eflush; []))
                                and prec_rels = (try (Edit.list_prec_rels name) with
                                                    Not_found -> (eprintf "Error: Precs for %s Not found%t" name eflush; []))
                                and dforms = (try (Edit.list_dforms name) with
                                                 Not_found -> (eprintf "Error: Precs for %s Not found%t" name eflush; [])) in

                                let visit_dform =
                                function (n, modes, attr, model, formats) ->
                                      ipair_term (itoken_term n)
                                      (idform_term (list_to_ilist_by_op idform_attr_op
                                                    ((list_to_ilist_by_op imode_cons_op []) :: attr))
                                       formats
                                       model) in

                                   ipair_term
                                   (imp_prec_pair_term (list_to_ilist_by_op imp_prec_cons_op precs)

                                    (list_to_ilist_by_op_map imp_prec_rel_cons_op
                                     (function (r, t1, t2) -> imp_prec_rel_term r t1 t2)
                                     prec_rels))
                                   (list_to_ilist (List.map visit_dform dforms))) in

                               list_to_ilist_map
                               get_dform
                               (map (function x -> hd (string_list_of_term x))
                                (map_isexpr_to_list_by_op ilist_op
                                 (function x -> x) (term_of_unbound_term symaddr)))

                       | {term_op = op; term_terms = symaddr :: r} when (opeq op mp_lookup_op) ->
                            let l = string_list_of_term (term_of_unbound_term symaddr) in
                               (try
                                   if l <> !current_symaddr then
                                      Edit.cd_thm (hd l) (hd (tl l));
                                   visit_proof []
                                with
                                   _ ->
                                      (eprintf "Error: Lookup cd thm for %s %s%t" (hd l) (hd (tl l)) eflush;
                                       mp_prf_term ivoid_term (**)
                                          (itext_term "Error: Cd Thm None")
                                          (list_to_ilist [])
                                          (list_to_ilist [])))

                       | {term_op = op; term_terms = symaddr :: addr :: tac :: r } when (opeq op mp_refine_op) ->
                            (let l = string_list_of_term (term_of_unbound_term symaddr) in
                                if l <> !current_symaddr then
                                   (current_symaddr := l; Edit.cd_thm (hd (tl l)) (hd (tl (tl l))));
                                let tactic = string_of_itext_term (term_of_unbound_term tac) in
                                   eprintf "%s %t" tactic eflush;
                                   let (msequent, msubgoals, mextras) =
                                      Edit.refine (int_list_of_term (term_of_unbound_term addr)) tactic in
                                      mp_ref_term (msequent_to_term msequent)
                                      (list_to_ilist (List.map msequent_to_term msubgoals))
                                      (list_to_ilist_by_op ianno_arg_cons_op (List.map msequent_to_term mextras)))

                       | {term_op = op; term_terms = symaddr :: r } when (opeq op mp_undo_op) ->
                            (let l = string_list_of_term (term_of_unbound_term symaddr) in
                                if l <> !current_symaddr then
                                   (current_symaddr := l; Edit.cd_thm (hd (tl l)) (hd (tl (tl l))));
                                Edit.undo ();
                                visit_proof [])

                       | {term_op = op; term_terms = text :: mli_text :: r } when (opeq op mp_compile_op) ->
		   (* (Hashtbl.add
		       Toploop.directive_table
		       "natp1"
		       (Toploop.Directive_int (let natp1 d = returnval := inatural_term (1 + d) in natp1)) []) *)
		   (* could have library write to file since coding for pretty printing already exists *)
		   (* (output_to_file mp_compile_ml_file text;
		       output_to_file mp_compile_ml_file mli_text;
		       Unix.system "sh -c omake";
		   *)
                            ivoid_term

                       | {term_op = op; term_terms = symaddr :: r } when (opeq op mp_save_op) ->
                            (let l = string_list_of_term (term_of_unbound_term symaddr) in
                             let modname = hd (tl l) in
                             let thmname = hd (tl (tl l)) in
                                if l <> !current_symaddr then
                                   (current_symaddr := l;
                                    Edit.cd_thm modname thmname);
                                eprintf "saving thm %s %t" thmname eflush;
                                Edit.save modname;
                                ivoid_term)

                       | {term_op = op; term_terms = symaddr :: r } when (opeq op mp_save_thy_op) ->
                            ( (* let l = string_list_of_term (term_of_unbound_term symaddr) in
		  Edit.save_thy (hd (tl l)); *)
                               ivoid_term)

                       | {term_op = op; term_terms = symaddr :: r } when false (*opeq op mp_wip_op*) ->
                            let name = hd (tl (map_isexpr_to_list string_of_itoken_term (term_of_unbound_term symaddr))) in

                            let thm_to_term x =
                               (Edit.cd_thm name x;
                                let (tac, goal, subgoals, extras) = Edit.node [] in
                                   ipair_term (itoken_term x) (msequent_to_term goal))

                            and rules_to_term l =
                               (let objs = ref [] in
                                   List.iter
                                   (function oc ->
                                         let (name, status, x, y) = oc in
                                            if (List.mem name l) then
                                               objs := oc::(!objs))
                                   (Edit.cd_list_contents name);

                                   list_to_ilist_map
                                   (function oc ->
                                         let (name, status, x, y) = oc in
                                            ipair_term (itoken_term name) (mp_objc_to_term oc))
                                   !objs) in

                               icons_term icons_op
                               (list_to_ilist_map itoken_term (get_parents_all name))
                               (let (w, c, a, rl) = Edit.list_module name in
                                   mp_edit_term (rules_to_term w)
                                   (list_to_ilist_map thm_to_term c)
                                   (list_to_ilist_map thm_to_term a)
                                   (rules_to_term rl))

                       | {term_op = op; term_terms = symaddr :: mseq :: r} when (opeq op mp_set_thm_op) ->
                            let l = string_list_of_term (term_of_unbound_term symaddr) in
                            let modname = hd (tl l) in
                            let thmname = hd (tl (tl l)) in
                            let () =
                               if l <> !current_symaddr then
                                  Edit.cd_thm modname thmname;
                            in
                            let assums, goal = term_to_msequent (term_of_unbound_term mseq) in
                               eprintf "setting goal %s %s %t" modname thmname eflush;
                               eprintf "TO: %a/%t" print_term goal eflush;
                               Edit.set_goal modname thmname goal;
                               Edit.set_assumptions modname thmname assums;
                               Edit.save modname;
                               ivoid_term

                       | {term_op = op; term_terms = symaddr :: rw :: r} when (opeq op mp_set_rw_op) ->
                            let l = string_list_of_term (term_of_unbound_term symaddr) in
                            let modname = hd (tl l) in
                            let thmname = hd (tl (tl l)) in
                            let () =
                               if l <> !current_symaddr then
                                  Edit.cd_thm modname thmname
                            in
                            let redex, contract = dest_rw_term (term_of_unbound_term rw) in
                               eprintf "setting redex %s %s %t" modname thmname eflush;
                               eprintf "TO: %a/%a%t" print_term redex print_term contract eflush;
                               Edit.set_redex modname thmname redex;
                               Edit.set_contractum modname thmname contract;
                               ivoid_term (*use Edit.set?*)

                       | {term_op = op; term_terms = mname :: name :: r} when (opeq op mp_create_op) ->
                            (Edit.create_thm (string_of_itoken_term (term_of_unbound_term mname))
                             (string_of_itoken_term (term_of_unbound_term name));
                             ivoid_term)

                       | {term_op = op; term_terms = mname :: name :: r} when (opeq op mp_create_rw_op) ->
                            (Edit.create_rw (string_of_itoken_term (term_of_unbound_term mname))
                             (string_of_itoken_term (term_of_unbound_term name));
                             ivoid_term)

       (* step refine ie for rewrite tactic *)
       (*
          | {term_op = op; term_terms = goal :: tac :: r} when (opeq op mp_step_refine_op) ->
          let subgoals, _ = refine (term_of_unbound_term tac) (term_of_unbound_term goal)
          in list_to_ilist_map id subgoals
       *)

                       | _ -> error ["refine_ehook"; "op"; "unrecognized"] [] [t])))

      (function term -> (function t -> (ifail_term term)))

   let library_mini_loop_eval () =
      let lib = oref_val library in
         server_loop lib

   let library_loop_eval () =
      let lib = oref_val library in
         (with_transaction lib
          (function t ->
                (Library.eval t
                 (null_ap
                  (itext_term "\\l. inform_message nil
		  ``MetaPRL Loop Start`` nil")))));
         server_loop lib

   let library_open_and_loop_eval name rhook =
      let host = Sys.getenv "NUPRL_HOST"
      and port = int_of_string (Sys.getenv "NUPRL_PORT") in
         library_open_eval name rhook host port;
         (unwind_error
          (function () -> library_loop_eval ();
                          library_close ())
          (function () -> library_close ()))

   let library_open_and_loop_eval' port host name rhook =
      library_open_eval name rhook host port;
      (unwind_error
       (function () -> library_mini_loop_eval ();
                       library_close ())
       (function () -> library_close ()))

end
