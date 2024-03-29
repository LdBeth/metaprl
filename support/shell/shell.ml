(*
 * The shell provides a command interface to the editor.
 * The commands defined here included:
 *    1. Creation. navigation of modules
 *    2. Creation, inspection of items in modules.
 *
 * ----------------------------------------------------------------
 *
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/htmlman/default.html or visit http://metaprl.org/
 * for more information.
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
 * Author: Jason Hickey <jyh@cs.cornell.edu>
 * Modified by: Aleksey Nogin <nogin@cs.cornell.edu>
 *)
extends Mptop
extends Shell_package
extends Shell_root
extends Shell_fs

open Lm_debug
open Lm_rprintf
open Lm_thread

open Opname
open Precedence
open Refiner.Refiner.Term
open Refiner.Refiner.TermOp

open Filter_type
open Filter_summary

open Tactic_type

open Shell_sig
open Shell_p4_sig
open Shell_internal_sig
open Shell_core
open Shell_current

(*
 * Show that the file is loading.
 *)
let () =
   show_loading "Loading Shell"

(* unused
let debug_shell  = load_debug "shell"
*)

(*
 * Shell takes input parser as an argument.
 *)
module Shell (ShellP4 : ShellP4Sig) =
struct
   let () =
      show_loading "Loading Shell module"

(* unused
   type t = shell
   type pid = string
 *)

   (*
    * Parsing arguments come from the shell.
    *)
   let parse_arg =
      ShellP4.parse_string, ShellP4.eval_tactic

   (*
    * Flush the output.
    * This also saves the session.
    *)
   let flush shell =
      let options = Session.get_view_options () in
         view options shell

   (************************************************************************
    * NUPRL5 INTERFACE                                                     *
    ************************************************************************)

   module Edit : ShellEditSig =
   struct
      (*
       * Assign the editor its own shell.
       *)
      let edit_pid = Lm_thread_shell.create "edit" Lm_thread_shell.HiddenJob

      let synchronize f =
         Lm_thread_shell.with_pid edit_pid (fun () ->
               State.write shell_entry (fun shell ->
                     Filter_exn.print_exn (get_db shell) None f shell)) ()

      (*
       * Return the list of all module names.
       *)
      let list_modules = all_theories

      let get_pack shell name =
         try Package_info.get packages name with
            NotLoaded _ ->
               eprintf "Loading package %s%t" name eflush;
               Package_info.load packages parse_arg name

      let get_info shell name =
         Package_info.info (get_pack shell name) parse_arg

      let save name =
         synchronize (fun shell ->
               Package_info.save parse_arg (get_pack shell name))

      let list_module_all name =
         synchronize (fun shell ->
               let shell = get_info shell name in
               let rec collect = function
                  [] ->
                     []
                | (h, _) :: t ->
                     let names = collect t in
                        match h with
                           Rewrite { rw_name = name; _ }
                         | CondRewrite { crw_name = name; _ }
                         | Rule { rule_name = name; _ } ->
                              name :: names
                         | _ ->
                              names
               in
                  collect (info_items shell))

      let list_module name =
         synchronize (fun shell ->
               let shell = get_info shell name in
               let rec collect wnames cnames anames rnames = function
                  [] -> (wnames, cnames, anames, rnames)
                | (h, _) :: t ->
                     match h with
                        Rewrite { rw_name = name; _ } ->
                           collect (name :: wnames) cnames anames rnames t
                      | CondRewrite { crw_name = name; _ } ->
                           collect wnames (name :: cnames) anames rnames t
                      | Rule { rule_name = name; _ } ->
                           collect wnames cnames anames (name :: rnames) t
                      | _ -> collect wnames cnames anames rnames t
               in
                  collect [] [] [] [] (info_items shell))

      let list_module_rw name =
         synchronize (fun shell ->
               let shell = get_info shell name in
               let rec collect = function
                  [] ->
                     []
                | (h, _) :: t ->
                     let names = collect t in
                        match h with
                           Rewrite { rw_name = name; _ } -> name :: names
                         | _ -> names
               in
                  collect (info_items shell))

      (*
       * Navigating the hierarchy.
       *)
      let list_parents name =
         synchronize (fun shell ->
               let rec collect = function
                  [] ->
                     []
                | (h, _) :: t ->
                     let parents = collect t in
                        match h with
                           Parent { parent_name = [name]; _ } ->
                              name :: parents
                         | _ ->
                              parents
               in
                  collect (info_items (get_info shell name)))

      (*
       * Display forms and precedences.
       *)
      let summary_opname = make_opname ["Summary"]
      let prec_op =
         mk_opname "prec" summary_opname
      let inherit_term =
         mk_simple_term (mk_opname "inherit" summary_opname) []
      let parens_term =
         mk_simple_term (mk_opname "parens" summary_opname) []

      let list_dforms name =
         synchronize (fun shell ->
               let mk_dform_option = function
                  DFormInheritPrec ->
                     inherit_term
                | DFormPrec s ->
                     mk_string_term prec_op s
                | DFormParens ->
                     parens_term
               in
               let rec collect = function
                  [] ->
                     []
                | (h, _) :: t ->
                     let dforms = collect t in
                        match h with
                           DForm { dform_name = name;
                                   dform_modes = modes;
                                   dform_options = options;
                                   dform_redex = redex;
                                   dform_def = def
                           } ->
                              begin
                                 match def with
                                  | TermDForm contractum ->
                                       (name, modes, List.map mk_dform_option options, redex, contractum) :: dforms
                                  | NoDForm
                                  | MLDForm _ ->
                                       dforms
                              end
                         | _ ->
                              dforms
               in
                  collect (info_items (get_info shell name)))

      let list_precs name =
         synchronize (fun shell ->
               let rec collect = function
                  [] ->
                     []
                | (h, _) :: t ->
                     let precs = collect t in
                        match h with
                           Prec s ->
                              mk_string_term prec_op s :: precs
                         | _ ->
                              precs
               in
                  collect (info_items (get_info shell name)))

      let list_prec_rels name =
         synchronize (fun shell ->
               let rec collect = function
                  [] ->
                     []
                | (h, _) :: t ->
                     let rels = collect t in
                        match h with
                           PrecRel { prec_rel = rel;
                                     prec_left = left;
                                     prec_right = right
                           } ->
                              begin
                                 let left = mk_string_term prec_op left in
                                 let right = mk_string_term prec_op right in
                                    match rel with
                                       LTRelation ->
                                          ("lt", left, right) :: rels
                                     | EQRelation ->
                                          ("eq", left, right) :: rels
                                     | GTRelation ->
                                          ("lt", right, left) :: rels
                                     | NoRelation ->
                                          rels
                              end
                         | _ ->
                              rels
               in
                  collect (info_items (get_info shell name)))

      let cd_list_contents mname =
         synchronize (fun shell ->
               let objs = ref [] in
               let f obj _ =
                  objs := obj#edit_get_contents shell.shell_subdir:: !objs;
                  false
               in
                  chdir parse_arg shell false false (module_dir mname);
                  apply_all parse_arg f false Shell_core.dont_clean_item Shell_core.dont_clean_module shell;
                  List.rev !objs)

      (*
       * Create a new thm.
       *)
      let cd_thm mname name =
         synchronize (fun shell ->
               let rec collect = function
                  [] ->
                     []
                | (h, _) :: t ->
                     match h with
                        SummaryItem { item_item = <:str_item< open $sl$ >>; _ } ->
                           sl :: collect t
                      | _ ->
                           collect t
               in
               let opens = collect (info_items (get_info shell mname)) in
                  chdir parse_arg shell false false (proof_dir mname name);
                  ignore (ShellP4.eval_opens opens))

      let create_thm mname name =
         synchronize (fun shell ->
               ignore (get_info shell mname);
               chdir parse_arg shell false false (module_dir mname);
               let package = get_current_package shell in
               let item = Shell_rule.create package parse_arg (get_display_mode shell) name in
                  item#edit_save;
                  touch shell;
                  cd_thm mname name)

      let create_rw mname name =
         synchronize (fun shell ->
               ignore (get_info shell mname);
               chdir parse_arg shell false false (module_dir mname);
               let package = get_current_package shell in
               let item = Shell_rule.create package parse_arg (get_display_mode shell) name in
                  item#edit_save;
                  touch shell;
                  cd_thm mname name)

      (*
       * Wrappers because Nuprl5 doesn't know about loading packages.
       *)
      let set_goal modname thmname t =
         synchronize (fun shell ->
               cd_thm modname thmname;
               set_goal shell t)

      let set_redex modname thmname t =
         synchronize (fun shell ->
               cd_thm modname thmname;
               set_redex shell t)

      let set_contractum modname thmname t =
         synchronize (fun shell ->
               cd_thm modname thmname;
               set_contractum shell t)

(* unused
      let set_assumptions_current tl =
         synchronize (fun shell ->
               set_assumptions shell tl)
 *)

      let set_assumptions modname thmname tl =
         synchronize (fun shell ->
               cd_thm modname thmname;
               set_assumptions shell tl)

      let set_params modname thmname pl =
         synchronize (fun shell ->
               cd_thm modname thmname;
               set_params shell pl)

      (*
       * Return the current node.
       *)
(* unused
      let addr addr =
         synchronize (fun shell ->
               cd parse_arg shell (string_of_int addr))
*)

      let node addr =
         synchronize (fun shell ->
               let proof = shell.shell_proof in
                  let { edit_goal = goal;
                        edit_expr = tac;
                        edit_subgoals = subgoals;
                        edit_extras = extras
                      } = proof#edit_info (List.map string_of_int addr)
                  in
                  let goal = Sequent.msequent goal in
                  let children = List.map Sequent.msequent subgoals in
                  let extras = List.map Sequent.msequent extras in
                     Some tac, goal, children, extras)

      let refine addr str =
         synchronize (fun shell ->
               let proof = shell.shell_proof in
               let expr = ShellP4.parse_string str in
               let tac = ShellP4.eval_tactic expr in
               let addr = List.map string_of_int addr in
                  if proof#edit_interpret addr (ProofRefine (str, expr, tac)) then touch shell;
                  let { edit_goal = goal;
                        edit_subgoals = subgoals;
                        edit_extras = extras;
                        _
                      } = proof#edit_info addr
                  in
                  let goal = Sequent.msequent goal in
                  let children = List.map Sequent.msequent subgoals in
                  let extras = List.map Sequent.msequent extras in
                     goal, children, extras)

      let undo () =
         synchronize undo
   end

   (************************************************************************
    * Toplevel functions.
    *)

   (*
    * Refresh the package list.
    *)
   let refresh_packages () =
      Package_info.refresh packages (Shell_state.get_includes ())

   module Top : ShellTopSig =
   struct
      let version = ShellP4.version

      (*
       * Synchronize to the current shell.
       *)
      let synchronize f =
         State.write shell_entry (fun shell ->
               if shell.shell_needs_refresh then
                  begin
                     refresh parse_arg shell;
                     shell.shell_needs_refresh <- false
                  end;
               Filter_exn.print_exn (get_db shell) None f shell)

      (*
       * Evaluate an expression in some shell.
       *)
      let eval text =
         try
            synchronize (fun shell ->
                  ShellP4.eval_top text)
         with
            End_of_file as exn ->
               raise exn
          | _ when not Refine_exn.backtrace ->
               ()

      let chdir text =
         try
            synchronize (fun shell ->
                  ignore (cd parse_arg text shell);
                  true)
         with
            exn when not Refine_exn.backtrace ->
               false

      let get_resource () =
         State.write shell_entry get_resource

      (*
       * Wrap the remaining commands.
       *)
      let wrap cmd arg =
         synchronize (fun shell ->
               cmd shell arg)

      let wrap_unit cmd () =
         synchronize cmd

      let wrap_unit_arg cmd () =
         synchronize (cmd parse_arg)

      let debug              = wrap_unit     (fun shell -> shell.shell_debug)
      let refresh            = wrap_unit_arg refresh
      let pwd                = wrap_unit     pwd
      let filename           = wrap_unit_arg filename
      let get_ls_options     = wrap_unit     get_ls_options
      let get_view_options   = wrap_unit     get_view_options
      let set_view_options   = wrap          set_view_options
      let clear_view_options = wrap          clear_view_options
      let get_dforms         = wrap          get_display_mode
      let set_dfmode         = wrap          set_dfmode
      let set_dftype         = wrap          set_dftype
      let set_window_width   = wrap          set_window_width
      let flush              = wrap_unit     flush

      let backup             = wrap_unit_arg backup
      let backup_all         = wrap_unit_arg backup_all

      let extract shell path =
         extract parse_arg shell (dir_of_path path)

      (*
       * Refresh packages at startup.
       *)
      let init () =
         refresh_packages ();
         if not !Shell_state.batch_flag then begin
            Shell_current.restore_sessions ();
            Shell_state.set_module "shell_theory";
            synchronize (fun shell ->
               eprintf "Current directory: %s@." (string_of_dir (shell.shell_fs, shell.shell_subdir)))
         end else
            (* XXX HACK: This initializes things somehow *)
            synchronize (fun _ -> ())

      (*
       * External toploop.
       *)
      let () =
         Shell_command.synchronize (fun commands ->
               commands.initialized         <- true;
               commands.init                <- init;
               commands.sync                <- synchronize;
               commands.parse_arg           <- parse_arg;
               commands.refresh             <- refresh;
               commands.pwd                 <- pwd;
               commands.set_dfmode          <- set_dfmode;
               commands.backup              <- backup;
               commands.backup_all          <- backup_all;
               commands.extract             <- wrap extract;
               commands.get_view_options    <- get_view_options;
               commands.set_view_options    <- set_view_options;
               commands.clear_view_options  <- clear_view_options;
               commands.edit                <- Shell_syscall.deref_edit ())
   end

   (************************************************************************
    * External Toploop.
    *)
   module Main : ShellMainSig =
   struct
      let main () =
         let pid =  Lm_thread_shell.create_or_find "shell" 1 Lm_thread_shell.VisibleJob in
            Lm_thread_shell.set_pid pid;

            (*
             * Note! the main function will call Top.init.
             *)
            ShellP4.main ();
            if not (!Shell_state.batch_flag) then begin
               Shell_current.flush ();
               Top.backup_all ()
            end
   end
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "editor.top"
 * End:
 * -*-
 *)
