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
 * Author: Jason Hickey <jyh@cs.cornell.edu>
 * Modified by: Aleksey Nogin <nogin@cs.cornell.edu>
 *)
extends Mptop
extends Package_info
extends Shell_util
extends Shell_rule
extends Shell_package
extends Shell_root
extends Shell_p4_sig
extends Shell_syscall

open Exn_boot

open Lm_debug
open Lm_rprintf
open Lm_thread
open Lm_thread_sig

open Opname
open Precedence
open Refiner.Refiner
open Refiner.Refiner.Term
open Refiner.Refiner.TermType
open Refiner.Refiner.TermShape
open Refiner.Refiner.RefineError
open Dform

open Filter_type
open Filter_summary

open Tactic_type
open Tactic_type.Tacticals

open Shell_sig
open Shell_p4_sig
open Shell_util

(*
 * Show that the file is loading.
 *)
let _ =
   show_loading "Loading Shell%t"

let debug_refine = load_debug "refine"

let debug_shell =
   create_debug (**)
      { debug_name = "shell";
        debug_description = "Display shell operations";
        debug_value = false
      }

type commands =
   { mutable cd : string -> string;
     mutable pwd : unit -> string list;
     mutable set_dfmode : string -> unit;
     mutable create_pkg : string -> unit;
     mutable save : unit -> unit;
     mutable export : unit -> unit;
     mutable view : LsOptionSet.t -> string -> unit;
     mutable expand : unit -> unit;
     mutable expand_all : unit -> unit;
     mutable apply_all : (edit_object -> dform_base -> unit) -> bool -> bool -> unit;
     mutable interpret : proof_command -> unit;
     mutable undo : unit -> unit;
     mutable redo : unit -> unit;
     mutable create_ax_statement : term -> string -> unit;
     mutable refine : tactic -> unit;
     mutable check : unit -> unit;
     mutable extract : string list -> unit -> Refine.extract;
     mutable term_of_extract : term list -> term; (* XXX HACK: temporary interface *)
     mutable print_theory : string -> unit;
     mutable get_view_options : unit -> string;
     mutable set_view_options : string -> unit;
     mutable clear_view_options : string -> unit;
     mutable find_subgoal : int -> string;
   }

let uninitialized _ = raise (Invalid_argument "The Shell module has not been instantiated")

let commands =
   { cd = uninitialized;
     pwd = uninitialized;
     set_dfmode = uninitialized;
     create_pkg = uninitialized;
     save = uninitialized;
     export = uninitialized;
     view = uninitialized;
     check = uninitialized;
     apply_all = uninitialized;
     expand = uninitialized;
     expand_all = uninitialized;
     interpret = uninitialized;
     undo = uninitialized;
     redo = uninitialized;
     create_ax_statement = uninitialized;
     refine = uninitialized;
     print_theory = uninitialized;
     extract = (fun _ -> uninitialized);
     term_of_extract = uninitialized;
     get_view_options = uninitialized;
     set_view_options = uninitialized;
     clear_view_options = uninitialized;
     find_subgoal = uninitialized;
   }

(************************************************************************
 * TYPES                                                                *
 ************************************************************************)

(*
 * This is the info we need for each subshell.
 * The subshells can be threaded; we must make this shell
 * thread-safe.
 *)
type info =
   { (* Display *)
      mutable shell_width   : int;
      mutable shell_df_mode : string;

      (* Current module and path and proof *)
      mutable shell_dir     : string list;
      mutable shell_package : Package_info.package option;
      mutable shell_proof   : edit_object;

      (* Display options *)
      mutable shell_view_options : LsOptionSet.t;

      (* Handle to current shell *)
      shell_shell           : Shell_state.t
   }

(************************************************************************
 * GLOBAL VALUES                                                        *
 ************************************************************************)

(*
 * We should skip the packages that do not have basic shell commands in them.
 *)
let shell_package pkg =
   let name = Package_info.name pkg in
      try Mptop.mem (Mptop.get_toploop_resource (Mp_resource.find (Mp_resource.theory_bookmark name)) []) "cd" with
         Not_found ->
            false

(*
 * All loaded modules.
 *)
let packages = Package_info.create (Shell_state.get_includes ())

let all_packages () =
   List.filter shell_package (Package_info.packages packages)

let default_mode_base = Mp_resource.theory_bookmark "summary"

(*
 * Turn a path to an absolute string.
 *)
let rec string_of_path = function
   [n] ->
      "/" ^ n
 | h::t ->
      "/" ^ h ^ string_of_path t
 | [] ->
      "/"

let mk_dep_name opname =
   "/" ^ (String.concat "/" (List.rev (dest_opname opname)))

(*
 * Shell takes input parser as an argument.
 *)
module Shell (ShellP4 : ShellP4Sig) =
struct
   let _ =
      show_loading "Loading Shell module%t"

   type t = info
   type pid = string

   (************************************************************************
    * IMPLEMENTATION                                                       *
    ************************************************************************)

   (*
    * Get the current "prl" printing base.
    *)
   let get_dfbase info =
      match info.shell_package with
         Some mod_info ->
            if !debug_shell then
               eprintf "Selecting display forms from %s%t" (Package_info.name mod_info) eflush;
            Mp_resource.theory_bookmark (Package_info.name mod_info)
       | None ->
            if !debug_shell then
               eprintf "Restoring default display forms%t" eflush;
            default_mode_base

   let get_display_mode info =
      let dfbase = get_dfbase info in
         match info.shell_df_mode with
            "tex" ->
               DisplayTex dfbase
          | "html" ->
               DisplayBrowser dfbase
          | mode ->
               DisplayText (dfbase, mode)

   let get_db info =
      let dfbase = get_dfbase info in
         get_mode_base dfbase info.shell_df_mode

   let set_dfmode info mode =
      info.shell_df_mode <- mode

   (*
    * Get the resource collection.
    *)
   let get_resource info =
      match info.shell_package with
         Some mod_info ->
            Mp_resource.find (Mp_resource.theory_bookmark (Package_info.name mod_info))
       | None ->
            raise Not_found

   (*
    * Create a new sub-shell.
    *)
   let default_shell =
      let port = None in
      let shell = ShellP4.get_current_state () in
      let dfmode = "prl" in
      let display_mode = DisplayText (default_mode_base, dfmode) in
      let proof = Shell_root.create packages display_mode in
         { shell_width   = 80;
           shell_df_mode = dfmode;
           shell_dir     = [];
           shell_package = None;
           shell_proof   = proof;
           shell_shell   = shell;
           shell_view_options = ls_options_default
         }

   let fork_shell shell =
      let { shell_proof = old_proof;
            shell_shell = old_shell
          } = shell
      in
      let new_proof = old_proof.edit_copy () in
      let new_shell = Shell_state.fork old_shell in
         { shell with shell_proof = new_proof;
                      shell_shell = new_shell
         }

   let current = State.private_val "Shell.current" default_shell fork_shell

   (*
    * Get the current package.
    *)
   let get_current_package shell =
      match shell.shell_package with
         Some pack ->
            pack
       | None ->
            raise (RefineError ("Shell.get_current_package", StringError "no current package"))

   (*
    * Parsing arguments come from the shell.
    *)
   let get_parse_arg shell =
      let shell = shell.shell_shell in
      let parse = ShellP4.parse_string shell in
      let eval = ShellP4.eval_tactic shell in
         parse, eval

   (*
    * Update the timestamp.
    *)
   let touch shell =
      let pack = get_current_package shell in
         try Package_info.touch pack with
            Failure _ ->
               begin
                  (* Change the status so that we can write to the file. *)
                  Package_info.set_status pack PackModified;
                  Package_info.touch pack
               end

   (************************************************************************
    * VIEWING                                                              *
    ************************************************************************)

   (*
    * Turn a string into a path, relative to info.dir.
    * The string is "/"-separated; "." means current directory, ".." its
    * parent, "..." its grandparent etc.
    * Also, "~" refers to the second level from top, e.g. cd "~" goes to
    * the current module.
    *)
   let parse_path info name =
      let home =
         match info.shell_dir with
            [] ->
               []
          | modname :: _ ->
               [modname]
      in
      let rec aux dir names =
         match names with
            [] -> dir
          | ""::ns -> aux dir ns
          | "~"::ns -> aux home ns
          | n::ns when (Lm_string_util.for_all (fun c -> c = '.') n) ->
               (* Remove |n| elements from dir's tail *)
               let head = try (fst (Lm_list_util.split_list ((List.length dir) - (String.length n) + 1) dir))
                          with Failure "split_list" -> []
               in aux head ns
          | n::ns -> aux (dir @ [n]) ns
      in
         aux (if String.length name <> 0 & name.[0] = '/' then [] else info.shell_dir)
         (Lm_string_util.split "/" name)

   (*
    * Update the current item being edited.
    *)
   let set_packages info =
      info.shell_proof.edit_addr [];
      info.shell_proof <- Shell_root.view packages (get_display_mode info)

   let set_package info modname =
      let pack = Package_info.get packages modname in
         info.shell_proof.edit_addr [];
         info.shell_proof <- Shell_package.view pack (get_parse_arg info) (get_display_mode info)

   let get_item info modname name =
      let parse_arg = get_parse_arg info in
      let display_mode = get_display_mode info in
      let pack =
         match info.shell_package with
            Some pack -> pack
          | None -> invalid_arg "Shell.get_item: no package"
      in
      let item =
         try Package_info.find pack parse_arg name with
            Not_found ->
               eprintf "Item '/%s/%s' not found%t" modname name eflush;
               raise Not_found
      in
         match item with
            Rewrite rw ->
               Shell_rule.view_rw pack parse_arg display_mode rw
          | CondRewrite crw ->
               Shell_rule.view_crw pack parse_arg display_mode crw
          | Rule rl ->
               Shell_rule.view_rule pack parse_arg display_mode rl
          | Opname _ ->
               eprintf "Editing opname '/%s/%s' not supported%t" modname name eflush;
               raise (Failure "view")
          | MLRewrite _ ->
               eprintf "Editing ML rewrite '/%s/%s' not implemented%t" modname name eflush;
               raise (Failure "view")
          | MLAxiom _ ->
               eprintf "Editing ML rule '/%s/%s' not implemented%t" modname name eflush;
               raise (Failure "view")
          | Parent _ ->
               eprintf "Editing parent '/%s/%s' not supported%t" modname name eflush;
               raise (Failure "view")
          | Module _ ->
               eprintf "Editing module '/%s/%s' not implemented%t" modname name eflush;
               raise (Failure "view")
          | DForm _ ->
               eprintf "Editing display form '/%s/%s' not implemented%t" modname name eflush;
               raise (Failure "view")
          | Prec _ ->
               eprintf "Editing precedence '/%s/%s' not supported%t" modname name eflush;
               raise (Failure "view")
          | PrecRel _ ->
               eprintf "Editing precedence relation '/%s/%s' not supported%t" modname name eflush;
               raise (Failure "view")
          | Id _ ->
               eprintf "Editing magic number '/%s/%s' not supported%t" modname name eflush;
               raise (Failure "view")
          | Resource _ ->
               eprintf "Editing resource '/%s/%s' not supported%t" modname name eflush;
               raise (Failure "view")
          | Improve _ ->
               eprintf "Editing resource improvement '/%s/%s' not supported%t" modname name eflush;
               raise (Failure "view")
          | GramUpd _ ->
               eprintf "Editing infix/suffix '/%s/%s' not supported%t" modname name eflush;
               raise (Failure "view")
          | SummaryItem _
          | ToploopItem _ ->
               eprintf "Editing summary item '/%s/%s' not implemented%t" modname name eflush;
               raise (Failure "view")
          | MagicBlock _ ->
               eprintf "Editing magic block '/%s/%s' not implemented%t" modname name eflush;
               raise (Failure "view")
          | Comment _ ->
               eprintf "Editing comment '/%s/%s' not implemented%t" modname name eflush;
               raise (Failure "view")
          | InputForm _ ->
               eprintf "Editing input form '/%s/%s' not implemented%t" modname name eflush;
               raise (Failure "view")
          | Definition _ ->
               eprintf "Editing definition '/%s/%s' not implemented%t" modname name eflush;
               raise (Failure "view")

   (*
    * Display the current proof.
    *)
   let display_proof info proof options =
      proof.edit_display options

   (*
    * Display the "root" directory.
    * This is just a list of the "important" packages.
    *)
   let view_packages info options =
      let proof = Shell_root.view packages (get_display_mode info) in
         display_proof info proof options

   (*
    * Display a particular package.
    *)
   let view_package info name options =
      let pack = Package_info.get packages name in
      let parse_arg = get_parse_arg info in
      let display_mode = get_display_mode info in
      let proof = Shell_package.view pack parse_arg display_mode in
         display_proof info proof options

   (*
    * View an item in a package.
    *)
   let view_item info modname name options =
      display_proof info info.shell_proof options

   (*
    * General purpose displayer.
    *)
   let view shell options name =
      let dir = parse_path shell name in
         match dir with
            [] ->
               view_packages shell options
          | [modname] ->
               view_package shell modname options
          | modname :: item :: _ ->
               view_item shell modname item options

   (*
    * Window width.
    *)
   let set_window_width shell i =
      shell.shell_width <- max !Mp_term.min_screen_width i

   (************************************************************************
    * PROCESS CONTROL                                                      *
    ************************************************************************)

   (*
    * Show current process id.
    *)
   let pid shell =
      Lm_thread_shell.get_pid ()

   (*
    * Show all process names.
    *)
   let jobs shell =
      let pids = Lm_thread_shell.get_pids () in
      let buf = Buffer.create 32 in
         ignore (List.fold_left (fun first i ->
                       if not first then
                          Buffer.add_string buf " ";
                       Buffer.add_string buf (string_of_int i);
                       false) true pids);
         Buffer.contents buf

   (*
    * Switch jobs.
    *)
   let fg shell pid =
      Lm_thread_shell.set_pid pid

   (*
    * Interface to the HTTP shell.
    *)
   let flush shell =
      view shell shell.shell_view_options "."

   let get_ls_options shell =
      shell.shell_view_options

   let get_view_options shell =
      string_of_ls_options shell.shell_view_options

   let set_view_options shell s =
      shell.shell_view_options <- ls_options_add shell.shell_view_options s

   let clear_view_options shell s =
      shell.shell_view_options <- ls_options_clear shell.shell_view_options s

   let get_shortener shell =
      match shell.shell_package with
         Some pkg ->
            let mk_opname = Package_info.mk_opname pkg in
            let shortener opname params bterms =
               match Opname.dest_opname opname with
                  h :: _ ->
                     let params = List.map param_type params in
                     let arities = List.map (fun bterm -> List.length (dest_bterm bterm).bvars) bterms in
                     let opname' = mk_opname [h] params arities in
                        if Opname.eq opname' opname then
                           h
                        else
                           Opname.string_of_opname opname
                | [] ->
                     "$"
            in
               shortener
       | None ->
            let shortener opname _ _ =
               Opname.string_of_opname opname
            in
               shortener

   let pwd shell =
      string_of_path shell.shell_dir

   let filename shell =
      match shell.shell_package with
         Some pack ->
            let parse_arg = get_parse_arg shell in
               Some (Package_info.filename pack parse_arg)
       | None ->
            None

   (************************************************************************
    * MODULES                                                              *
    ************************************************************************)

   (*
    * Make a new package.
    * Right now we only allow packages at the top level.
    *)
   let create_pkg shell name =
      match parse_path shell name with
         [modname] ->
            (* Top level *)
            let _ = Package_info.create_package packages (get_parse_arg shell) modname in
               view shell LsOptionSet.empty name
       | [] ->
            raise (Failure "Shell.create_package: can't create root package")
       | _ ->
            raise (Failure "Shell.create_package: packages can't be nested right now")

   (*
    * Save the current package.
    *)
   let save shell =
      match shell.shell_package with
         Some pack ->
            Package_info.save pack
       | None ->
            ()

   let export shell =
      touch shell;
      match shell.shell_package with
         Some pack ->
            Package_info.export (get_parse_arg shell) pack
       | None ->
            ()

   (************************************************************************
    * OBJECTS                                                              *
    ************************************************************************)

   (*
    * Creation functions.
    *)
   let create_rw shell name =
      touch shell;
      raise (RefineError ("Shell.create_rw", StringError "not implemented"))

   let create_axiom shell name =
      touch shell;
      raise (RefineError ("Shell.create_axiom", StringError "not implemented"))

   let create_thm shell name =
      touch shell;
      raise (RefineError ("Shell.create_thm", StringError "not implemented"))

   let create_ax_statement shell seq name =
      let parse_arg = get_parse_arg shell in
      let display_mode = get_display_mode shell in
      let package = get_current_package shell in
      let item = Shell_rule.create package parse_arg display_mode name in
         item.edit_set_goal seq;
         item.edit_save ();
         touch shell

   let create_opname shell name =
      touch shell;
      raise (RefineError ("Shell.create_opname", StringError "not implemented"))

   let create_condition shell name =
      touch shell;
      raise (RefineError ("Shell.create_condition", StringError "not implemented"))

   let create_parent shell name =
      touch shell;
      raise (RefineError ("Shell.create_parent", StringError "not implemented"))

   let create_dform shell name =
      touch shell;
      raise (RefineError ("Shell.create_dform", StringError "not implemented"))

   let create_prec shell name =
      touch shell;
      raise (RefineError ("Shell.create_prec", StringError "not implemented"))

   let create_prec_rel shell name =
      touch shell;
      raise (RefineError ("Shell.create_prec_rel", StringError "not implemented"))

   let create_resource shell name =
      touch shell;
      raise (RefineError ("Shell.create_resources", StringError "not implemented"))

   let create_infix shell name =
      touch shell;
      raise (RefineError ("Shell.create_infix", StringError "not implemented"))

   let create_ml shell name =
      touch shell;
      raise (RefineError ("Shell.create_ml", StringError "not implemented"))

   (*
    * Proof operations.
    *)
   let set_goal shell t =
      touch shell;
      shell.shell_proof.edit_set_goal t;
      display_proof shell shell.shell_proof LsOptionSet.empty

   let set_redex shell t =
      touch shell;
      shell.shell_proof.edit_set_redex t;
      display_proof shell shell.shell_proof LsOptionSet.empty

   let set_contractum shell t =
      touch shell;
      shell.shell_proof.edit_set_contractum t;
      display_proof shell shell.shell_proof LsOptionSet.empty

   let set_assumptions shell tl =
      touch shell;
      shell.shell_proof.edit_set_assumptions tl;
      display_proof shell shell.shell_proof LsOptionSet.empty

   let set_params shell pl =
      touch shell;
      shell.shell_proof.edit_set_params pl;
      display_proof shell shell.shell_proof LsOptionSet.empty

   let check shell =
      match shell with
         { shell_package = Some pack; shell_dir = mod_name :: name :: _ } ->
            begin
               try
                  let deps = Refine.compute_dependencies (Package_info.get_refiner pack) (make_opname [name; mod_name]) in
                     printf "The proof of /%s/%s depends on the following definitions and axioms:\n" mod_name name;
                     let print_dep (dep, opname) =
                        let kind =
                           match dep with
                              Refine_sig.DepDefinition -> "Definition"
                            | Refine_sig.DepRule -> "Rule"
                            | Refine_sig.DepRewrite -> "Rewrite"
                            | Refine_sig.DepCondRewrite -> "Conditional Rewrite"
                        in
                           printf "\t%s %s\n" kind (mk_dep_name opname)
                     in
                        List.iter print_dep deps
               with
                  Refine_sig.Incomplete opname ->
                     eprintf "Error: Proof of /%s/%s depends on incomplete proof of %s!%t" mod_name name (mk_dep_name opname) eflush
            end
       | _ ->
            raise (Failure "check - must be inside a proof")

   let expand shell =
      let start = Unix.times () in
      let start_time = Unix.gettimeofday () in
      let () = shell.shell_proof.edit_interpret ProofExpand in
      let finish = Unix.times () in
      let finish_time = Unix.gettimeofday () in
         display_proof shell shell.shell_proof LsOptionSet.empty;
         eprintf "User time %f; System time %f; Real time %f%t" (**)
            ((finish.Unix.tms_utime +. finish.Unix.tms_cutime)
             -. (start.Unix.tms_utime +. start.Unix.tms_cstime))
            ((finish.Unix.tms_stime +. finish.Unix.tms_cstime)
             -. (start.Unix.tms_stime +. finish.Unix.tms_cstime))
            (finish_time -. start_time)
            eflush

   let refine shell tac =
      let str, ast = Shell_state.get_tactic shell.shell_shell in
         touch shell;
         if !debug_refine then
            eprintf "Starting refinement%t" eflush;
         shell.shell_proof.edit_refine str ast tac;
         if !debug_refine then
            eprintf "Displaying proof%t" eflush;
         if Shell_state.is_interactive shell.shell_shell then
            display_proof shell shell.shell_proof LsOptionSet.empty;
         if !debug_refine then
            eprintf "Proof displayed%t" eflush

   (*
    * Get the current goal.
    *)
   let goal shell =
      touch shell;
      (shell.shell_proof.edit_info ()).edit_goal

   let undo shell =
      touch shell;
      shell.shell_proof.edit_undo ();
      display_proof shell shell.shell_proof LsOptionSet.empty

   let redo shell =
      touch shell;
      shell.shell_proof.edit_redo ();
      display_proof shell shell.shell_proof LsOptionSet.empty

   let interpret shell command =
      touch shell;
      shell.shell_proof.edit_interpret command;
      display_proof shell shell.shell_proof LsOptionSet.empty

   let rec apply_all shell f time clean_res =
      let dir = shell.shell_dir in
      let apply_it item mod_name name =
         (*
          * Here we indeed want to ignore absolutely any kind of error.
          * Whatever went wrong, we just want to skip the bad item and continue to the next one.
          *)
         (try Shell_state.set_so_var_context shell.shell_shell (Some (item.edit_get_terms ())) with
             _ ->
                ());
         (try f item (get_db shell) with
             _ ->
                ());
         if clean_res then
            Mp_resource.clear_results (mod_name, name)
      in
      let rec apply_all_exn time =
         let parse_arg = get_parse_arg shell in
         let display_mode = get_display_mode shell in
            match shell.shell_package with
               Some pack ->
                  touch shell;
                  let mod_name = Package_info.name pack in
                  let apply_item = function
                     Rewrite rw, _ ->
                        if !debug_shell then eprintf "Rewrite %s%t" rw.rw_name eflush;
                        apply_it (Shell_rule.view_rw pack parse_arg display_mode rw) mod_name rw.rw_name
                   | CondRewrite crw, _ ->
                        if !debug_shell then eprintf "CondRewrite %s%t" crw.crw_name eflush;
                        apply_it (Shell_rule.view_crw pack parse_arg display_mode crw) mod_name crw.crw_name
                   | Rule rl, _ ->
                        if !debug_shell then eprintf "Rule %s%t" rl.rule_name eflush;
                        apply_it (Shell_rule.view_rule pack parse_arg display_mode rl) mod_name rl.rule_name
                   | _ ->
                        ()
                  in
                  let items = info_items (Package_info.info pack parse_arg) in
                     if time then
                        let start = Unix.times () in
                        let start_time = Unix.gettimeofday () in
                        let _ = List.iter apply_item items in
                        let finish = Unix.times () in
                        let finish_time = Unix.gettimeofday () in
                           eprintf "User time %f; System time %f; Real time %f%t" (**)
                              ((finish.Unix.tms_utime +. finish.Unix.tms_cutime)
                               -. (start.Unix.tms_utime +. start.Unix.tms_cstime))
                              ((finish.Unix.tms_stime +. finish.Unix.tms_cstime)
                               -. (start.Unix.tms_stime +. finish.Unix.tms_cstime))
                              (finish_time -. start_time)
                              eflush
                     else
                        List.iter apply_item items

             | None ->
                  let expand pack =
                     let name = Package_info.name pack in
                        eprintf "Entering %s%t" name eflush;
                        chdir shell false true [name];
                        apply_all_exn false
                  in
                     List.iter expand (all_packages())
      in
         apply_all_exn time;
         chdir shell false false [];
         chdir shell false false dir

   and expand_all shell =
      let f item db =
         item.edit_interpret ProofExpand
      in
         apply_all shell f true true

   (*
    * Change directory.
    *)
   and chdir shell need_shell verbose path =
      let shell' = shell.shell_shell in
         match path with
            [] ->
               (* go to toplevel *)
               shell.shell_dir <- [];
               shell.shell_package <- None;
               set_packages shell;
               Shell_state.set_dfbase shell' None;
               Shell_state.set_mk_opname shell' None;
               Shell_state.set_so_var_context shell' None;
               Shell_state.set_infixes shell' None;
               Shell_state.set_module shell' "shell"
          | (modname :: item) as dir ->
               (* change module only if in another (or at top) *)
               if shell.shell_dir = [] || List.hd shell.shell_dir <> modname then
                  begin
                     if modname <> String.uncapitalize modname then
                        raise (Failure "Shell.chdir: module name should not be capitalized");

                     (* See if the theory exists *)
                     ignore (Theory.get_theory modname);
                     let pkg = Package_info.load packages (get_parse_arg shell) modname in
                        if need_shell && not (shell_package pkg) then
                           failwith ("Module " ^ modname ^ " does not contain shell commands");
                        shell.shell_package <- Some pkg;
                        Shell_state.set_dfbase shell' (Some (get_db shell));
                        Shell_state.set_mk_opname shell' (Some (Package_info.mk_opname pkg));
                        Shell_state.set_infixes shell' (Some (Package_info.get_infixes pkg));
                        Shell_state.set_module shell' modname;
                        if verbose then
                           eprintf "Module: /%s%t" modname eflush
                  end;

               if item = [] then
                  begin
                     (* top of module *)
                     shell.shell_dir <- dir;
                     Shell_state.set_so_var_context shell' None;
                     set_package shell modname
                  end
               else
                  begin
                     (* select an item (if not there already), then go down the proof. *)
                     begin
                        match shell.shell_dir with
                           old_modname::old_item::_ when old_modname = modname && old_item = List.hd item ->
                              shell.shell_proof.edit_addr (List.map int_of_string (List.tl item))
                         | _ ->
                              try
                                 (* Leave the old proof at the root *)
                                 shell.shell_proof.edit_addr [];
                                 let proof = get_item shell modname (List.hd item) in
                                    Shell_state.set_so_var_context shell' (Some (proof.edit_get_terms ()));
                                    proof.edit_addr (List.map int_of_string (List.tl item));
                                    shell.shell_proof <- proof
                              with
                                 exn ->
                                    begin
                                       (* Bring things back to where they were *)
                                       let dir = shell.shell_dir in
                                          if (dir <> []) && (List.hd dir = modname) then
                                             shell.shell_dir <- [modname]
                                          else
                                             shell.shell_dir <- [];
                                          chdir shell false verbose dir;
                                          raise exn
                                    end;
                     end;
                     shell.shell_dir <- dir
                  end

   and cd shell name =
      chdir shell true true (parse_path shell name);
      string_of_path shell.shell_dir

   (*
    * TeX functions.
    *)
   and print_theory shell name =
      let mode = shell.shell_df_mode in
      let dir = shell.shell_dir in
         shell.shell_df_mode <- "tex";
         chdir shell false true [name];
         expand_all shell;
         view shell LsOptionSet.empty ".";
         shell.shell_df_mode <- mode;
         chdir shell false false dir

   let extract shell path () =
      let dir = shell.shell_dir in
         try
            chdir shell false false path;
            let res = shell.shell_proof.edit_get_extract () in
               chdir shell false false dir;
               res
         with
            exn ->
               chdir shell false false dir;
               raise exn

   let term_of_extract shell terms =
      match shell with
         { shell_package = Some pack; shell_dir = mod_name :: name :: _ } ->
            Refine.extract_term (Package_info.get_refiner pack) (make_opname [name;mod_name]) terms
       | _ ->
            raise (Failure "Shell.term_of_extract only works inside a proof")

   let edit_find info i =
      match info.shell_dir with
         modname :: name :: _ ->
            let dir = modname :: name :: (List.map string_of_int (info.shell_proof.edit_find i)) in
               info.shell_dir <- dir;
               string_of_path dir
      | _ ->
            raise (Invalid_argument "Shell.find_subgoal: not in a proof")

   (************************************************************************
    * NUPRL5 INTERFACE                                                     *
    ************************************************************************)

   module Edit : ShellEditSig =
   struct
      (*
       * Assign the editor its own shell.
       *)
      let edit_pid = Lm_thread_shell.create true

      let synchronize f =
         Lm_thread_shell.with_pid edit_pid (fun () ->
               State.write current (fun shell ->
                     Filter_exn.print_exn (get_db shell) None f shell)) ()

      (*
       * Return the list of all module names.
       *)
      let list_modules () =
         List.map Package_info.name (all_packages ())

      let get_info shell name =
         let parse_arg = get_parse_arg shell in
            try Package_info.info (Package_info.get packages name) parse_arg with
               NotLoaded _ ->
                  eprintf "Loading package %s%t" name eflush;
                  ignore (Package_info.load packages (get_parse_arg shell) name);
                  Package_info.info (Package_info.get packages name) parse_arg

      let save name =
         Package_info.save (Package_info.get packages name)

      let list_module_all name =
         synchronize (fun shell ->
               let shell = get_info shell name in
               let rec collect = function
                  [] ->
                     []
                | (h, _) :: t ->
                     let names = collect t in
                        match h with
                           Rewrite { rw_name = name }
                         | CondRewrite { crw_name = name }
                         | Rule { rule_name = name } ->
                              name :: names
                         | _ ->
                              names
               in
                  collect (info_items shell))

      let list_module name =
         synchronize (fun shell ->
               let shell = get_info shell name in
               let (wnames : string list ref) = ref []
               and (cnames : string list ref) = ref []
               and (anames : string list ref) = ref []
               and (rnames : string list ref) = ref []
               in
               let rec collect = function
                  [] -> (!wnames, !cnames, !anames, !rnames)
                | (h, _) :: t ->
                     match h with
                        Rewrite { rw_name = name } -> wnames := name :: !wnames; collect t
                      | CondRewrite { crw_name = name } -> cnames := name :: !cnames; collect t
                      | Rule { rule_name = name } -> rnames := name :: !rnames; collect t
                      | _ -> collect t
               in
                  collect (info_items shell))

      let list_module_rw name =
         synchronize (fun shell ->
               let shell = get_info shell name in
               let rec collect = function
                  [] ->
                     []
                | (h, _) :: t ->
                     let names = collect t in
                        match h with
                           Rewrite { rw_name = name } -> name :: names
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
                           Parent { parent_name = [name] } ->
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
               let opname = make_opname [name] in
               let mk_dform_option = function
                  DFormInheritPrec ->
                     inherit_term
                | DFormPrec name' ->
                     mk_simple_term prec_op [mk_simple_term (make_opname [name'; name]) []]
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
                           Prec name' ->
                              mk_simple_term (make_opname [name'; name]) [] :: precs
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
                                 let left = mk_simple_term (make_opname [left; name]) [] in
                                 let right = mk_simple_term (make_opname [right; name]) [] in
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
                  objs := obj.edit_get_contents () :: !objs
               in
                  chdir shell false false [mname];
                  apply_all shell f false false;
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
                        SummaryItem { item_item = <:str_item< open $sl$ >> } ->
                           sl :: collect t
                      | _ ->
                           collect t
               in
               let opens = collect (info_items (get_info shell mname)) in
                  chdir shell false false [mname; name];
                  ignore (ShellP4.eval_opens shell.shell_shell opens))

      let create_thm mname name =
         synchronize (fun shell ->
               ignore (get_info shell mname);
               chdir shell false false [mname];
               let package = get_current_package shell in
               let item = Shell_rule.create package (get_parse_arg shell) (get_display_mode shell) name in
                  item.edit_save ();
                  touch shell;
                  cd_thm mname name)

      let create_rw mname name =
         synchronize (fun shell ->
               ignore (get_info shell mname);
               chdir shell false false [mname];
               let package = get_current_package shell in
               let item = Shell_rule.create package (get_parse_arg shell) (get_display_mode shell) name in
                  item.edit_save ();
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

      let set_assumptions_current tl =
         synchronize (fun shell ->
               set_assumptions shell tl)

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
      let addr addr =
         synchronize (fun shell ->
               shell.shell_proof.edit_down addr)

      let node addr =
         synchronize (fun shell ->
               let proof = shell.shell_proof in
                  proof.edit_addr addr;
                  let { edit_goal = goal;
                        edit_expr = tac;
                        edit_subgoals = subgoals;
                        edit_extras = extras
                      } = proof.edit_info ()
                  in
                  let goal = Sequent.msequent goal in
                  let children = List.map Sequent.msequent subgoals in
                  let extras = List.map Sequent.msequent extras in
                     Some tac, goal, children, extras)

      let refine addr str =
         synchronize (fun shell ->
               let proof = shell.shell_proof in
               let expr = ShellP4.parse_string shell.shell_shell str in
               let tac = ShellP4.eval_tactic shell.shell_shell expr in
                  proof.edit_addr addr;
                  proof.edit_refine str expr tac;
                  let { edit_goal = goal;
                        edit_subgoals = subgoals;
                        edit_extras = extras
                      } = proof.edit_info ()
                  in
                  let goal = Sequent.msequent goal in
                  let children = List.map Sequent.msequent subgoals in
                  let extras = List.map Sequent.msequent extras in
                     goal, children, extras)

      let undo () =
         synchronize (fun shell ->
               shell.shell_proof.edit_undo ())
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
      (*
       * Synchronize to the current shell.
       *)
      let synchronize f =
         State.write current (fun shell ->
               Filter_exn.print_exn (get_db shell) None f shell)

      (*
       * Refresh packages at startup.
       *)
      let refresh_packages = refresh_packages

      (*
       * Evaluate an expression in some shell.
       *)
      let eval text =
         try
            synchronize (fun shell ->
                  ShellP4.eval_top shell.shell_shell text)
         with
            End_of_file as exn ->
               raise exn
          | _ when not backtrace ->
               ()

      let chdir text =
         try
            synchronize (fun shell ->
                  ignore (cd shell text);
                  true)
         with
            exn when not backtrace ->
               false

      let get_resource () =
         State.write current get_resource

      (*
       * Wrap the remaining commands.
       *)
      let wrap cmd arg =
         synchronize (fun shell ->
               cmd shell arg)

      let wrap_unit cmd () =
         synchronize cmd

      let pwd                = wrap_unit pwd
      let filename           = wrap_unit filename
      let get_ls_options     = wrap_unit get_ls_options
      let get_view_options   = wrap_unit get_view_options
      let set_view_options   = wrap      set_view_options
      let clear_view_options = wrap      clear_view_options
      let get_shortener      = wrap_unit get_shortener
      let set_dfmode         = wrap      set_dfmode
      let set_window_width   = wrap      set_window_width
      let flush              = wrap_unit flush

      let _ =
         if commands.cd != uninitialized then
            raise (Invalid_argument "The Shell module was initialized twice");
         commands.cd <- wrap cd;
         commands.pwd <- (fun () -> synchronize (fun shell -> shell.shell_dir));
         commands.set_dfmode <- set_dfmode;
         commands.create_pkg <- wrap create_pkg;
         commands.save <- wrap_unit save;
         commands.export <- wrap_unit export;
         commands.view <- wrap view;
         commands.check <- wrap_unit check;
         commands.expand <- wrap_unit expand;
         commands.expand_all <- wrap_unit expand_all;
         commands.apply_all <- wrap apply_all;
         commands.interpret <- wrap interpret;
         commands.undo <- wrap_unit undo;
         commands.redo <- wrap_unit redo;
         commands.create_ax_statement <- wrap create_ax_statement;
         commands.refine <- wrap refine;
         commands.print_theory <- wrap print_theory;
         commands.extract <- (fun path () -> synchronize (fun shell -> extract shell path ()));
         commands.term_of_extract <- wrap term_of_extract;
         commands.get_view_options <- get_view_options;
         commands.set_view_options <- set_view_options;
         commands.clear_view_options <- clear_view_options;
         commands.find_subgoal <- wrap edit_find;
         ()
   end

   (************************************************************************
    * External Toploop.
    *)

   module Main : ShellMainSig =
   struct
      let main () =
         refresh_packages ();
         let state =
            State.read current (fun shell ->
                  Shell_state.set_module shell.shell_shell "shell";
                  shell.shell_shell)
         in
            ShellP4.main state
   end
end

let extract path () = commands.extract path ()
let term_of_extract ts = commands.term_of_extract ts

(*
 * Control profiling.
 *)
external restart_gmon : unit -> unit = "restart_gmon"
external stop_gmon : unit -> unit = "stop_gmon"

(*
 * Toploop functions
 *)
let exit () = raise End_of_file
let set_debug = set_debug
let print_gc_stats () =
   flush stdout;
   Gc.print_stat Pervasives.stdout;
   Pervasives.flush Pervasives.stdout

let cd s = commands.cd s
let pwd _ = string_of_path (commands.pwd ())
let set_dfmode s = commands.set_dfmode s
let create_pkg s = commands.create_pkg s
let save _ = commands.save ()
let export _ = commands.export ()
let check _ = commands.check ()
let expand _ = commands.expand ()
let expand_all _ = commands.expand_all ()
let apply_all f t c = commands.apply_all f t c
let undo _ = commands.undo ()
let redo _ = commands.redo ()
let create_ax_statement t s = commands.create_ax_statement t s
let refine t = commands.refine t
let print_theory s = commands.print_theory s
let get_view_options _ = commands.get_view_options ()
let set_view_options s = commands.set_view_options s
let clear_view_options s = commands.clear_view_options s
let find_subgoal i = commands.find_subgoal i

let nop _ = commands.interpret ProofNop
let kreitz _ = commands.interpret ProofKreitz
let clean _ = commands.interpret ProofClean
let squash _ = commands.interpret ProofSquash
let copy s = commands.interpret (ProofCopy s)
let paste s = commands.interpret (ProofPaste s)
let make_assum _ = commands.interpret ProofMakeAssum

let interpret_all command =
   let f item db =
      item.edit_interpret command
   in
      apply_all f true false

let clean_all _ = interpret_all ProofClean
let squash_all _ = interpret_all ProofSquash

let set_tex_file = Shell_tex.set_file

let save_all _ =
   let save pack =
      if Package_info.status pack = PackModified then
         Package_info.save pack
   in
      List.iter save (all_packages ())

let ls s =
   commands.view (ls_options_of_string s) "."

let view name =
   commands.view LsOptionSet.empty name

let up i =
   ignore (cd (String.make (i + 1) '.'));
   ls ""

let down i =
   ignore (cd (string_of_int i));
   ls ""

let root () =
   match commands.pwd () with
      modname::item::_ ->
         ignore (cd ("/" ^ modname ^ "/" ^ item));
         ls ""
    | _ ->
         eprintf "Can not go to the proof root: not inside a proof%t" eflush

let status item =
   let name, status, _, _ = item.edit_get_contents () in
   let str_status = match status with
      ObjPrimitive ->
         "is a primitive axiom"
    | ObjDerived ->
         "is an internally derived object"
    | ObjComplete(c1,c2) ->
         sprintf "is a derived object with a complete proof (%i rule boxes, %i primitive steps)" c1 c2
    | ObjIncomplete(c1,c2) ->
         sprintf "is a derived object with an incomplete proof (%i rule boxes, %i primitive steps)" c1 c2
    | ObjBad ->
         "is a derived object with a broken proof"
    | ObjUnknown ->
         "is an object with unknown status"
   in
      eprintf "Status: `%s' %s%t" name str_status eflush

let status_all () =
   let f item db =
      eprintf "Expanding `%s':%t" (let name, _, _, _ = item.edit_get_contents () in name) eflush;
      begin try item.edit_interpret ProofExpand with Invalid_argument _ | _ -> () end;
      status item;
   in
      apply_all f true true

let check_all () =
   (* Make a few things bols, or highlight with `...' and *...* *)
   let bfs, bfe, bfs2, bfe2 =
      match Lm_terminfo.tgetstr Lm_terminfo.enter_bold_mode, Lm_terminfo.tgetstr Lm_terminfo.exit_attribute_mode with
         Some b, Some e -> b, e, b, e
       | _ -> "*", "*", "`", "'"
   in
   let check item =
      let name, _, _, _ = item.edit_get_contents () in
      let status =
         match item.edit_check () with
            RefPrimitive ->
               "is a primitive axiom"
          | RefIncomplete(c1,c2) ->
               sprintf "is a derived object with an %sincomplete%s proof (%i rule boxes, %i primitive steps)" bfs bfe c1 c2
          | RefComplete(c1,c2,l) ->
               sprintf "is a derived object with a complete grounded proof (%i rule boxes, %i primitive steps, %i dependencies)" c1 c2 (List.length l)
          | RefUngrounded(c1,c2,op) ->
               sprintf "is a derived object with a complete %sungrounded%s proof (%i rule boxes, %i primitive steps) that depends on an incomplete %s%s%s" bfs bfe c1 c2 bfs2 (mk_dep_name op) bfe2
      in
         eprintf "Refiner status: %s%s%s %s%t" bfs2 name bfe2 status eflush
   in
   let f item db =
      check item
   in
      apply_all f true true

(*
 * -*-
 * Local Variables:
 * Caml-master: "editor.top"
 * End:
 * -*-
 *)
