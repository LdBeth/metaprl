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
 * Author: Jason Hickey
 * jyh@cs.cornell.edu
 *)

include Proof_edit
include Package_info
include Shell_rewrite
include Shell_rule
include Shell_package
include Shell_root
include Shell_p4_sig

open Printf
open Mp_debug

open Opname
open Precedence
open Refiner.Refiner.Term
open Refiner.Refiner.Refine
open Refiner.Refiner.RefineError
open Dform
open Dform_print
open Rformat

open Filter_type
open Filter_summary

open Tactic_type
open Tactic_type.Tacticals
open Mptop

open Package_sig
open Package_info
open Proof_edit
open Shell_sig
open Shell_p4_sig

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

(*
 * Control profiling.
 *)
external restart_gmon : unit -> unit = "restart_gmon"
external stop_gmon : unit -> unit = "stop_gmon"

(*
 * Shell takes input parser as an argument.
 *)
module Shell (ShellP4 : ShellP4Sig) =
struct
   let _ =
      show_loading "Loading Shell module%t"

   (************************************************************************
    * TYPES                                                                *
    ************************************************************************)

   (*
    * This is the info we need for each subshell.
    * The subshells can be threaded; we must make this shell
    * thread-safe.
    *)
   type t =
      { (* Display *)
         mutable width : int;
         mutable df_mode : string;
         mutable port : Mux_channel.session option;

         (* Current module and path and proof *)
         mutable dir : string list;
         mutable package : Package.package option;
         mutable proof : edit_object;

         (* Handle to current shell *)
         shell : ShellP4.t
      }

   (************************************************************************
    * GLOBAL VALUES                                                        *
    ************************************************************************)

   (*
    * All loaded modules.
    *)
   let packages = Package.create (ShellP4.get_includes ())

   let default_mode_base =
      try Package.get_dforms "summary" with
         Not_found ->
            eprintf "Can't install Summary display forms%t" eflush;
            Dform_print.null_mode_base

   (************************************************************************
    * IMPLEMENTATION                                                       *
    ************************************************************************)

   (*
    * Get the current "prl" printing base.
    *)
   let get_dfbase info =
      match info.package with
         Some mod_info ->
            if !debug_shell then
               eprintf "Selecting display forms from %s%t" (Package.name mod_info) eflush;
            Package.dforms mod_info
       | None ->
            if !debug_shell then
               eprintf "Restoring default display forms%t" eflush;
            default_mode_base

   let get_display_mode info =
      let dfbase = get_dfbase info in
         match info.port with
            Some port ->
               DisplayGraphical (port, dfbase)
          | None ->
               match info.df_mode with
                  "tex" ->
                     DisplayTex dfbase
                | mode ->
                     DisplayText (dfbase, mode)

   let get_db info =
      let dfbase = get_dfbase info in
         get_mode_base dfbase info.df_mode

   let set_db info mode =
      info.df_mode <- mode

   let set_dfmode mode info =
      info.df_mode <- mode

   (*
    * Create a new sub-shell.
    *)
   let create_aux port shell =
      let dfmode = "prl" in
      let display_mode = DisplayText (default_mode_base, dfmode) in
      let proof = Shell_root.create packages display_mode in
         { width = 80;
           df_mode = dfmode;
           dir = [];
           package = None;
           port = port;
           proof = proof;
           shell = shell
         }

   (*
    * Global values.
    *)
   let global_lock = Mutex.create ()

   let global = create_aux None (ShellP4.get_current_state ())

   let global_shells = ref [global]

   let current_shell = ref global

   let get_current_shell () =
      !current_shell

   let synchronize info f x =
      Mutex.lock global_lock;
      try
         let result = f x in
            Mutex.unlock global_lock;
            result
      with
         exn ->
            Mutex.unlock global_lock;
            let buf = Rformat.new_buffer () in
            let db = get_db info in
               TacticExn.format_exn db buf exn;
               Rformat.print_to_channel info.width buf stderr;
               flush stderr;
               raise exn

   (*
    * Get the current package.
    *)
   let get_current_package info =
      match info.package with
         Some pack ->
            pack
       | None ->
            raise (RefineError ("Shell.get_current_package", StringError "no current package"))

   (*
    * Parsing arguments come from the shell.
    *)
   let get_parse_arg info =
      let shell = info.shell in
      let parse = ShellP4.parse_string shell in
      let eval = ShellP4.eval_tactic shell in
         parse, eval

   (*
    * Update the timestamp.
    *)
   let touch info =
      let pack = get_current_package info in
         try Package.touch pack with
            Failure "touch" ->
               eprintf "The module %s is read-only.  Use set_writeable () to change it.%t" (Package.name pack) eflush;
               raise (Failure "touch")

   (*
    * Change the status so that we can write to the file.
    *)
   let set_writeable info =
      let pack = get_current_package info in
         Package.set_status pack Modified

   (*
    * Display possible exceptions.
    *)
   let print_exn info f x =
      try TacticExn.print (get_db info) f x with
         RefineError _ ->
            raise (RefineError ("Shell", ToploopIgnoreError))
       | exn ->
            raise exn

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
         match info.dir with
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
          | n::ns when (String_util.for_all (fun c -> c = '.') n) ->
               (* Remove |n| elements from dir's tail *)
               let head = try (fst (List_util.split_list ((List.length dir) - (String.length n) + 1) dir))
                          with Failure "split_list" -> []
               in aux head ns
          | n::ns -> aux (dir @ [n]) ns
      in
         aux (if String.length name <> 0 & name.[0] = '/' then [] else info.dir)
             (String_util.split '/' name)

   (*
    * Turn a path to an absolute string.
    *)
   let rec string_of_path = function
      [n] -> "/" ^ n
    | h::t ->
         "/" ^ h ^ string_of_path t
    | [] ->
         "/"

   (*
    * Update the current item being edited.
    *)
   let set_packages info =
      info.proof.edit_addr [];
      info.proof <- Shell_root.view packages (get_display_mode info)

   let set_package info modname =
      let pack = Package.get packages modname in
         info.proof.edit_addr [];
         info.proof <- Shell_package.view pack (get_parse_arg info) (get_display_mode info)

   let set_item info modname name =
      let parse_arg = get_parse_arg info in
      let display_mode = get_display_mode info in
      let pack = Package.get packages modname in
      let item =
         try Package.find pack parse_arg name with
            Not_found ->
               eprintf "Item '/%s/%s' not found%t" modname name eflush;
               raise Not_found
      in
      let proof =
         match item with
            Rewrite rw ->
               Shell_rewrite.view_rw pack parse_arg display_mode rw
          | CondRewrite crw ->
               Shell_rewrite.view_crw pack parse_arg display_mode crw
          | Axiom ax ->
               Shell_rule.view_axiom pack parse_arg display_mode ax
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
          | Infix _ ->
               eprintf "Editing infix '/%s/%s' not supported%t" modname name eflush;
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
      in
         (* Leave the current proof at the root *)
         info.proof.edit_addr [];
         proof

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
      let pack = Package.get packages name in
      let parse_arg = get_parse_arg info in
      let display_mode = get_display_mode info in
      let proof = Shell_package.view pack parse_arg display_mode in
         display_proof info proof options

   (*
    * View an item in a package.
    *)
   let view_item info modname name options =
      display_proof info info.proof options

   (*
    * General purpose displayer.
    *)
   let view info options name =
      let dir = parse_path info name in
         match dir with
            [] ->
               view_packages info options
          | [modname] ->
               view_package info modname options
          | modname :: item :: _ ->
               view_item info modname item options

   let ls info s =
      let len = String.length s in
      let rec collect options i =
         if i = len then
            List.rev options
         else
            let options =
               match s.[i] with
                  '-' ->
                     options
                | 'R' ->
                     (LsRules :: options)
                | 'r' ->
                     (LsRewrites :: options)
                | 'u' ->
                     (LsUnjustified :: options)
                | 'a' ->
                     (LsAll :: options)
                | _ ->
                     raise (RefineError ("ls", StringStringError ("unrecognized option", s)))
            in
               collect options (succ i)
      in
      let view () =
         view info (collect [] 0) "."
      in
         print_exn info view ()

   let view info name =
      view info [] name

   (*
    * Window width.
    *)
   let set_window_width info i =
      info.width <- max !Mp_term.min_screen_width i

   (*
    * Show the directory.
    *)
   let pwd info =
      string_of_path info.dir

   (************************************************************************
    * PROCESS CONTROL                                                      *
    ************************************************************************)

   (*
    * Format the process name.
    * We include the pid, and the current directory.
    *)
   let format_process_name shell =
      let port =
         match shell.port with
            Some port ->
               Mux_channel.id_of_session port
          | None ->
               0
      in
         sprintf "[%d] %s" port (string_of_path shell.dir)

   (*
    * Copy the current shell.
    *)
   let fork =
      let fork shell =
         let { width = old_width;
               df_mode = old_mode;
               dir = old_dir;
               package = old_package;
               port = old_port;
               proof = old_proof;
               shell = old_shell
             } = shell
         in
         let new_port =
            match old_port with
               Some session ->
                  Some (Mux_channel.new_session session)
             | None ->
                  None
         in
         let new_proof = old_proof.edit_copy () in
         let new_shell = ShellP4.fork old_shell in
         let new_shell =
            { width = old_width;
              df_mode = old_mode;
              dir = old_dir;
              package = old_package;
              port = new_port;
              proof = new_proof;
              shell = new_shell
            }
         in
            global_shells := new_shell :: !global_shells;
            current_shell := new_shell;
            ShellP4.set_current_state new_shell.shell;
            format_process_name new_shell
      in
         (fun shell -> synchronize shell fork shell)

   (*
    * Show current process id.
    *)
   let pid shell =
      format_process_name shell

   (*
    * Show all process names.
    *)
   let jobs shell =
      let jobs shell =
         let project shell =
            let pid =
               match shell.port with
                  Some session ->
                     Mux_channel.id_of_session session
                | None ->
                     0
            in
               pid, shell
         in
         let compare (pid1, _) (pid2, _) =
            pid1 < pid2
         in
         let shells = List.map project !global_shells in
         let shells = Sort.list compare shells in
            String_util.concat "\n" (List.map (fun (_, shell) -> format_process_name shell) shells)
      in
         synchronize shell jobs shell

   (*
    * Switch jobs.
    *)
   let shell_of_id id =
      let rec search = function
         [] ->
            raise Not_found
       | shell :: tl ->
            match shell.port with
               Some session ->
                  if id = Mux_channel.id_of_session session then
                     shell
                  else
                     search tl
             | None ->
                  search tl
      in
         search !global_shells

   let fg shell =
      let fg id =
         let shell = shell_of_id id in
            current_shell := shell;
            ShellP4.set_current_state shell.shell;
            format_process_name shell
      in
         synchronize shell fg

   (*
    * Interface to the HTTP shell.
    *)
   let set_port port =
      global.port <- port;
      try
         print_exn global (fun info -> ls info "") global
      with
         exn ->
            ()

   (*
    * Evaluate an expression in some shell.
    *)
   let eval id text =
      let shell =
         synchronize !current_shell shell_of_id id
      in
         try
            print_exn shell (fun text ->
                  begin
                     match shell.port with
                        Some port ->
                           eprintf "Shell [%d]: %s%t" (Mux_channel.id_of_session port) text eflush
                      | None ->
                           ()
                  end;
                  ShellP4.eval_expr shell.shell text;
                  ls shell "") text
         with
            exn ->
               ()

   (************************************************************************
    * MODULES                                                              *
    ************************************************************************)

   (*
    * Load a package if it is not already loaded.
    *)
   let load info name =
      let _ =
         try
            let pack = Package.get packages name in
               match Package.status pack with
                  Modified ->
                     raise (Failure (sprintf "Shell.load: package '%s' is modified" name))
                | _ ->
                     ()
         with
            Not_found ->
               ()
      in
         ignore (Package.load packages (get_parse_arg info) name)

   (* Currently load only creates problesm, does not work correctly
    * HACK!!!
    *)
   let fake_load _ _ =
      raise (Failure "The ``load'' function is currently not supported\nPlease ``cd'' into the theory you want to get loaded")

   (*
    * Make a new package.
    * Right now we only allow packages at the top level.
    *)
   let create_pkg info name =
      match parse_path info name with
         [modname] ->
            (* Top level *)
            let _ = Package.create_package packages (get_parse_arg info) modname in
               view info name
       | [] ->
            raise (Failure "Shell.create_package: can't create root package")
       | _ ->
            raise (Failure "Shell.create_package: packages can't be nested right now")

   (*
    * Save the current package.
    *)
   let save info =
      match info.package with
         Some pack ->
            Package.save pack
       | None ->
            ()

   let export info =
      touch info;
      match info.package with
         Some pack ->
            Package.export (get_parse_arg info) pack
       | None ->
            ()

   let save_all info =
      let save pack =
         if Package.status pack = Modified then
            Package.save pack
      in
         List.iter save (Package.packages packages)

   (************************************************************************
    * OBJECTS                                                              *
    ************************************************************************)

   (*
    * Creation functions.
    *)
   let create_rw info name =
      let create name =
         touch info;
         raise (RefineError ("Shell.create_rw", StringError "not implemented"))
      in
         print_exn info create name

   let create_axiom info name =
      let create name =
         touch info;
         raise (RefineError ("Shell.create_axiom", StringError "not implemented"))
      in
         print_exn info create name

   let create_thm info name =
      let create name =
         touch info;
         raise (RefineError ("Shell.create_thm", StringError "not implemented"))
      in
         print_exn info create name

   let create_ax_statement info seq name =
      let create name =
         let parse_arg = get_parse_arg info in
         let display_mode = get_display_mode info in
         let package = get_current_package info in
         let item = Shell_rule.create package parse_arg display_mode name in
            item.edit_set_goal seq;
            item.edit_save ();
            touch info
      in
         print_exn info create name

   let create_opname info name =
      let create name =
         touch info;
         raise (RefineError ("Shell.create_opname", StringError "not implemented"))
      in
         print_exn info create name

   let create_condition info name =
      let create name =
         touch info;
         raise (RefineError ("Shell.create_condition", StringError "not implemented"))
      in
         print_exn info create name

   let create_parent info name =
      let create name =
         touch info;
         raise (RefineError ("Shell.create_parent", StringError "not implemented"))
      in
         print_exn info create name

   let create_dform info name =
      let create name =
         touch info;
         raise (RefineError ("Shell.create_dform", StringError "not implemented"))
      in
         print_exn info create name

   let create_prec info name =
      let create name =
         touch info;
         raise (RefineError ("Shell.create_prec", StringError "not implemented"))
      in
         print_exn info create name

   let create_prec_rel info name =
      let create name =
         touch info;
         raise (RefineError ("Shell.create_prec_rel", StringError "not implemented"))
      in
         print_exn info create name

   let create_resource info name =
      let create name =
         touch info;
         raise (RefineError ("Shell.create_resources", StringError "not implemented"))
      in
         print_exn info create name

   let create_infix info name =
      let create name =
         touch info;
         raise (RefineError ("Shell.create_infix", StringError "not implemented"))
      in
         print_exn info create name

   let create_ml info name =
      let create name =
         touch info;
         raise (RefineError ("Shell.create_ml", StringError "not implemented"))
      in
         print_exn info create name

   (*
    * Proof operations.
    *)
   let set_goal info t =
      let set t =
         touch info;
         info.proof.edit_set_goal t;
         display_proof info info.proof []
      in
         print_exn info set t

   let set_redex info t =
      let set t =
         touch info;
         info.proof.edit_set_redex t;
         display_proof info info.proof []
      in
         print_exn info set t

   let set_contractum info t =
      let set t =
         touch info;
         info.proof.edit_set_contractum t;
         display_proof info info.proof []
      in
         print_exn info set t

   let set_assumptions info tl =
      let set t =
         touch info;
         info.proof.edit_set_assumptions tl;
         display_proof info info.proof []
      in
         print_exn info set tl

   let set_params info pl =
      let set t =
         touch info;
         info.proof.edit_set_params pl;
         display_proof info info.proof []
      in
         print_exn info set pl

   let check info =
      let set info =
         let _ = info.proof.edit_check () in
            display_proof info info.proof []
      in
         print_exn info set info

   let expand info =
      let set info =
         let start = Unix.times () in
         let start_time = Unix.gettimeofday () in
         let _ = info.proof.edit_expand (get_db info) in
         let finish = Unix.times () in
         let finish_time = Unix.gettimeofday () in
            display_proof info info.proof [];
            eprintf "User time %f; System time %f; Real time %f%t" (**)
               ((finish.Unix.tms_utime +. finish.Unix.tms_cutime)
                -. (start.Unix.tms_utime +. start.Unix.tms_cstime))
               ((finish.Unix.tms_stime +. finish.Unix.tms_cstime)
                -. (start.Unix.tms_stime +. finish.Unix.tms_cstime))
               (finish_time -. start_time)
               eflush
      in
         print_exn info set info

   (*
    * Redefined below as a shortcut for cd
    * let root () =
    *    let set () =
    *       info.proof.edit_root ();
    *       display_proof ()
    *    in
    *       print_exn info set ()
    * let up i =
    *    let set () =
    *       info.proof.edit_up i;
    *       display_proof ()
    *    in
    *       print_exn info set ()
    * let down i =
    *    let set i =
    *       info.proof.edit_down i;
    *       display_proof ()
    *    in
    *       print_exn info set i
    *)
   let refine info tac =
      let set info =
         let str, ast = ShellP4.get_tactic info.shell in
            touch info;
            if !debug_refine then
               eprintf "Starting refinement%t" eflush;
            info.proof.edit_refine str ast tac;
            if !debug_refine then
               eprintf "Displaying proof%t" eflush;
            if ShellP4.is_interactive info.shell then
               display_proof info info.proof [];
            if !debug_refine then
               eprintf "Proof displayed%t" eflush
      in
         print_exn info set info

   (*
    * Get the current goal.
    *)
   let goal info =
      let get info =
         touch info;
         (info.proof.edit_info ()).edit_goal
      in
         print_exn info get info

   let undo info =
      let set info =
         touch info;
         info.proof.edit_undo ();
         display_proof info info.proof []
      in
         print_exn info set info

   let redo info =
      let set info =
         touch info;
         info.proof.edit_redo ();
         display_proof info info.proof []
      in
         print_exn info set info

   let interpret info command =
      let set info =
         touch info;
         info.proof.edit_interpret command;
         display_proof info info.proof []
      in
         print_exn info set info

   let nop info =
      interpret info ProofNop

   let unfold info =
      interpret info ProofUnfold

   let kreitz info =
      interpret info ProofKreitz

   let clean info =
      interpret info ProofClean

   let squash info =
      interpret info ProofSquash

   let copy info s =
      interpret info (ProofCopy s)

   let paste info s =
      interpret info (ProofPaste s)

   let make_assum info =
      interpret info ProofMakeAssum

   (*
    * Load all the ped's for the current module.
    * This is mainly for preparing for performance testing.
    *)
   let sync info =
      let sync info =
         let parse_arg = get_parse_arg info in
         match info.package with
            Some pkg ->
               let ped_of_proof = function
                  Filter_cache.Primitive _
                | Filter_cache.Derived _
                | Filter_cache.Incomplete ->
                     ()
                | Filter_cache.Interactive proof ->
                     let _ = Package.ped_of_proof pkg parse_arg proof in
                        ()
               in
               let sync_item (item, _) =
                  match item with
                     Rewrite { rw_proof = proof } ->
                        ped_of_proof proof
                   | CondRewrite { crw_proof = proof } ->
                        ped_of_proof proof
                   | Axiom { axiom_proof = proof } ->
                        ped_of_proof proof
                   | Rule { rule_proof = proof } ->
                        ped_of_proof proof
                   | _ ->
                        ()
               in
                  List.iter sync_item (info_items (Package.info pkg parse_arg))
          | None ->
               eprintf "sync: no current package%t" eflush
      in
         print_exn info sync info

   let rec apply_all f info time =
      let apply_all_exn info =
         let parse_arg = get_parse_arg info in
         let display_mode = get_display_mode info in
         match info.package with
            Some pack ->
               touch info;
               let apply_it item =
                  try f item (get_db info) with
                     _ ->
                        ()
               in
               let apply_item (item, _) =
                  match item with
                     Rewrite rw ->
                        if !debug_shell then eprintf "Rewrite %s%t" rw.rw_name eflush;
                        apply_it (Shell_rewrite.view_rw pack parse_arg display_mode rw)
                   | CondRewrite crw ->
                        if !debug_shell then eprintf "CondRewrite %s%t" crw.crw_name eflush;
                        apply_it (Shell_rewrite.view_crw pack parse_arg display_mode crw)
                   | Axiom ax ->
                        if !debug_shell then eprintf "Axiom %s%t" ax.axiom_name eflush;
                        apply_it (Shell_rule.view_axiom pack parse_arg display_mode ax)
                   | Rule rl ->
                        if !debug_shell then eprintf "Rule %s%t" rl.rule_name eflush;
                        apply_it (Shell_rule.view_rule pack parse_arg display_mode rl)
                   | _ ->
                        ()
               in
               let start = Unix.times () in
               let start_time = Unix.gettimeofday () in
               let _ = List.iter apply_item (info_items (Package.info pack parse_arg)) in
               let finish = Unix.times () in
               let finish_time = Unix.gettimeofday () in
               if time then
                  eprintf "User time %f; System time %f; Real time %f%t" (**)
                     ((finish.Unix.tms_utime +. finish.Unix.tms_cutime)
                      -. (start.Unix.tms_utime +. start.Unix.tms_cstime))
                     ((finish.Unix.tms_stime +. finish.Unix.tms_cstime)
                      -. (start.Unix.tms_stime +. finish.Unix.tms_cstime))
                     (finish_time -. start_time)
                     eflush
               else
                  ()

          | None ->
               let expand pack =
                  let name = Package.name pack in
                     eprintf "Entering %s%t" name eflush;
                     chdir info [name];
                     apply_all f info false
               in
                  List.iter expand (Package.packages packages);
                  chdir info []
      in
         print_exn info apply_all_exn info

   and expand_all info =
      let f item db =
         item.edit_expand db
      in
         apply_all f info true

   and check_all info =
      let f item db =
         ignore (item.edit_check ())
      in
         apply_all f info true

   and status item =
      let status = item.edit_status () in
      let str_status = match fst status with
         ObjRoot ->
            "is a root directory"
       | ObjPackage ->
            "is a theory module"
       | ObjPrimitive ->
            "is a primitive axiom"
       | ObjDerived ->
            "is an internally derived object"
       | ObjComplete ->
            "is a derived object with a complete proof"
       | ObjIncomplete ->
            "is a derived object with an incomplete proof"
       | ObjBad ->
            "is a derived object with a broken proof"
       | ObjUnknown ->
            "is an object with unknown status"
      in
         eprintf "Status: `%s' %s%t" (snd status) str_status eflush

   and status_all info =
      let f item db =
         eprintf "Expanding `%s':%t" (snd (item.edit_status ())) eflush;
         begin try item.edit_expand db with _ | Invalid_argument _ -> () end;
         status item
      in
         apply_all f info true

   and interpret_all command info =
      let f item db =
         item.edit_interpret command
      in
         apply_all f info true

   and clean_all info = interpret_all ProofClean info
   and squash_all info = interpret_all ProofSquash info

   (*
    * Change directory.
    *)
   and chdir info path =
      let shell = info.shell in
      match path with
         [] ->
            (* go to toplevel *)
            info.dir <- [];
            info.package <- None;
            set_packages info;
            ShellP4.set_df shell None;
            ShellP4.set_mk_opname shell None;
            eprintf "Module: /%t" eflush
       | (modname :: item) as dir ->
            (* change module only if in another (or at top) *)
            if info.dir = [] or List.hd info.dir <> modname then
               begin
                  let pkg = Package.get packages modname in
                     info.package <- Some pkg;
                     ShellP4.set_df shell (Some (get_db info));
                     ShellP4.set_mk_opname shell (Some (Package.mk_opname pkg));
                     ShellP4.set_module shell modname (commands info);
                     eprintf "Module: /%s%t" modname eflush;
                     (* HACK!!! I do not know a better way to initialize a package - AN *)
                     ignore (Package.info pkg (get_parse_arg info))
               end;

            if item = [] then
               begin
                  (* top of module *)
                  info.dir <- dir;
                  set_package info modname
               end
            else
               begin
                  (* select an item (if not there already), then go down the proof. *)
                  let proof =
                     if info.dir = []
                        or List.tl info.dir = []
                        or List.hd (List.tl info.dir) <> List.hd item
                     then
                        set_item info modname (List.hd item)
                     else
                        info.proof
                  in

                  (* go down the proof with pf_path *)
                  proof.edit_addr (List.map int_of_string (List.tl item));
                  info.proof <- proof;
                  info.dir <- dir
               end

   and cd info name =
      print_exn info (chdir info) (parse_path info name);
      pwd info

   and root info =
      let set info =
         let _ = cd info (String.make ((List.length info.dir) - 1) '.') in
            display_proof info info.proof []
      in
         if List.length info.dir >= 2 then
            set info

   and up info i =
      let set info =
         let _ = cd info (String.make (i + 1) '.') in
            display_proof info info.proof []
      in
         set info

   and down info i =
      let set i =
         let _ = cd info (string_of_int i) in
            display_proof info info.proof []
      in
         set i

   (*
    * TeX functions.
    *)
   and set_tex_file info name =
      Shell_tex.set_file name

   and print_theory info name =
      let f () =
         let mode = info.df_mode in
            info.df_mode <- "tex";
            ignore (cd info ("/" ^ name));
            expand_all info;
            view info ".";
            info.df_mode <- mode
      in
         print_exn info f ()

   (*
    * Commands.
    *)
   and commands info =
      ["fork",             UnitFunExpr     (fun () -> StringExpr (fork info));
       "pid",              UnitFunExpr     (fun () -> StringExpr (pid info));
       "jobs",             UnitFunExpr     (fun () -> StringExpr (jobs info));
       "fg",               IntFunExpr      (fun i ->  StringExpr (fg info i));
       "cd",               StringFunExpr   (fun s  -> StringExpr (cd info s));
       "pwd",              UnitFunExpr     (fun () -> StringExpr (pwd info));
       "set_dfmode",       StringFunExpr   (fun s ->  UnitExpr (set_dfmode s info));
       "set_window_width", IntFunExpr      (fun i  -> UnitExpr (set_window_width info i));
       "load",             StringFunExpr   (fun s  -> UnitExpr (fake_load info s));
       "create_pkg",       StringFunExpr   (fun s  -> UnitExpr (create_pkg info s));
       "save",             UnitFunExpr     (fun () -> UnitExpr (save info));
       "export",           UnitFunExpr     (fun () -> UnitExpr (export info));
       "save_all",         UnitFunExpr     (fun () -> UnitExpr (save_all info));
       "create_rw",        StringFunExpr   (fun s  -> UnitExpr (create_rw info s));
       "create_axiom",     StringFunExpr   (fun s  -> UnitExpr (create_axiom info s));
       "create_thm",       StringFunExpr   (fun s  -> UnitExpr (create_thm info s));
       "create_ax_statement", TermFunExpr  (fun st -> StringFunExpr (fun s  -> UnitExpr (create_ax_statement info st s)));
       "create_opname",    StringFunExpr   (fun s  -> UnitExpr (create_opname info s));
       "create_condition", StringFunExpr   (fun s  -> UnitExpr (create_condition info s));
       "create_parent",    StringFunExpr   (fun s  -> UnitExpr (create_parent info s));
       "create_dform",     StringFunExpr   (fun s  -> UnitExpr (create_dform info s));
       "create_prec",      StringFunExpr   (fun s  -> UnitExpr (create_prec info s));
       "create_prec_rel", (**)
          StringFunExpr (fun s1 ->
                StringFunExpr (fun s2 ->
                      StringFunExpr (fun s3 ->
                            UnitExpr (create_prec_rel info s1 s2 s3))));
       "create_resource",  StringFunExpr   (fun s  -> UnitExpr (create_resource info s));
       "create_infix",     StringFunExpr   (fun s  -> UnitExpr (create_infix info s));
       "create_ml",        StringFunExpr   (fun s  -> UnitExpr (create_ml info s));
       "view",             StringFunExpr   (fun s  -> UnitExpr (view info s));
       "ls",               StringFunExpr   (fun s  -> UnitExpr (ls info s));
       "set_goal",         TermFunExpr     (fun t  -> UnitExpr (set_goal info t));
       "set_redex",        TermFunExpr     (fun t  -> UnitExpr (set_redex info t));
       "set_contractum",   TermFunExpr     (fun t  -> UnitExpr (set_contractum info t));
       "set_assumptions",  TermListFunExpr (fun tl -> UnitExpr (set_assumptions info tl));
       "check",            UnitFunExpr     (fun () -> UnitExpr (check info));
       "expand",           UnitFunExpr     (fun () -> UnitExpr (expand info));
       "status",           UnitFunExpr     (fun () -> UnitExpr (status info.proof));
       "root",             UnitFunExpr     (fun () -> UnitExpr (root info));
       "up",               IntFunExpr      (fun i  -> UnitExpr (up info i));
       "down",             IntFunExpr      (fun i  -> UnitExpr (down info i));
       "refine",           TacticFunExpr   (fun t  -> UnitExpr (refine info t));
       "undo",             UnitFunExpr     (fun () -> UnitExpr (undo info));
       "redo",             UnitFunExpr     (fun () -> UnitExpr (redo info));
       "nop",              UnitFunExpr     (fun () -> UnitExpr (nop info));
       "unfold",           UnitFunExpr     (fun () -> UnitExpr (unfold info));
       "kreitz",           UnitFunExpr     (fun () -> UnitExpr (kreitz info));
       "clean",            UnitFunExpr     (fun () -> UnitExpr (clean info));
       "squash",           UnitFunExpr     (fun () -> UnitExpr (squash info));
       "copy",             StringFunExpr   (fun s  -> UnitExpr (copy info s));
       "paste",            StringFunExpr   (fun s  -> UnitExpr (paste info s));
       "make_assum",       UnitFunExpr     (fun () -> UnitExpr (make_assum info));
       "sync",             UnitFunExpr     (fun () -> UnitExpr (sync info));
       "expand_all",       UnitFunExpr     (fun () -> UnitExpr (expand_all info));
       "check_all",        UnitFunExpr     (fun () -> UnitExpr (check_all info));
       "status_all",       UnitFunExpr     (fun () -> UnitExpr (status_all info));
       "clean_all",        UnitFunExpr     (fun () -> UnitExpr (clean_all info));
       "squash_all",       UnitFunExpr     (fun () -> UnitExpr (squash_all info));
       "set_debug",        StringFunExpr   (fun s  -> BoolFunExpr (fun b -> UnitExpr (set_debug s b)));
       "stop_gmon",        UnitFunExpr     (fun () -> UnitExpr (stop_gmon ()));
       "restart_gmon",     UnitFunExpr     (fun () -> UnitExpr (restart_gmon ()));
       "print_gc_stats",   UnitFunExpr     (fun () -> UnitExpr (Gc.print_stat stdout));
       "set_tex_file",     StringFunExpr   (fun s  -> UnitExpr (set_tex_file info s));
       "print_theory",     StringFunExpr   (fun s  -> UnitExpr (print_theory info s))]

   (************************************************************************
    * NUPRL5 INTERFACE                                                     *
    ************************************************************************)

   (*
    * Return the list of all module names.
    *)
   let edit_list_modules info =
      let list info =
         List.map Package.name (Package.packages packages)
      in
         print_exn info list info

   let edit_info info name =
      let parse_arg = get_parse_arg info in
      let edit info =
         try Package.info (Package.get packages name) parse_arg with
            NotLoaded _ ->
               eprintf "Loading package %s%t" name eflush;
               ignore (Package.load packages (get_parse_arg info) name);
               Package.info (Package.get packages name) parse_arg
      in
         print_exn info edit info

   let edit_save info name =
      let edit info =
         Package.save (Package.get packages name)
      in
         print_exn info edit info

   let edit_list_module_all info name =
      let info = edit_info info name in
      let rec collect = function
         [] ->
            []
       | (h, _) :: t ->
            let names = collect t in
               match h with
                  Rewrite { rw_name = name } -> name :: names
                | CondRewrite { crw_name = name } -> name :: names
                | Axiom { axiom_name = name } -> name :: names
                | Rule { rule_name = name } -> name :: names
                | _ -> names
      in
         collect (info_items info)

   let edit_list_module info name =
      let info = edit_info info name in
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
                | Axiom { axiom_name = name } -> anames := name :: !anames; collect t
                | Rule { rule_name = name } -> rnames := name :: !rnames; collect t
                | _ -> collect t
      in
         collect (info_items info)

   let edit_list_module_rw info name =
      let info = edit_info info name in
      let rec collect = function
         [] ->
            []
       | (h, _) :: t ->
            let names = collect t in
               match h with
                  Rewrite { rw_name = name } -> name :: names
               (* | CondRewrite { crw_name = name } -> name :: names
                | Axiom { axiom_name = name } -> name :: names
                | Rule { rule_name = name } -> name :: names *)
                | _ -> names
      in
         collect (info_items info)

   (*
    * Navigating the hierarchy.
    *)
   let edit_list_parents info name =
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
         collect (info_items (edit_info info name))

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
   let internal_term =
      mk_simple_term (mk_opname "internal" summary_opname) []

   let edit_list_dforms info name =
      let opname = make_opname [name] in
      let mk_dform_option = function
         DFormInheritPrec ->
            inherit_term
       | DFormPrec name' ->
            mk_simple_term prec_op [mk_simple_term (make_opname [name'; name]) []]
       | DFormParens ->
            parens_term
       | DFormInternal ->
            internal_term
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
         collect (info_items (edit_info info name))

   let edit_list_precs info name =
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
         collect (info_items (edit_info info name))

   let edit_list_prec_rels info name =
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
         collect (info_items (edit_info info name))

   (*
    * Create a new thm.
    *)
   let edit_cd_thm info mname name =
      let rec collect = function
         [] ->
            []
       | (h, _) :: t ->
            match h with
               SummaryItem (<:str_item< open $sl$ >>) ->
                  sl :: collect t
             | _ ->
                  collect t
      in
      let opens = collect (info_items (edit_info info mname)) in
      let _ = cd info ("/" ^ mname ^ "/" ^ name) in
         ShellP4.eval_opens info.shell opens;
         ()

   let edit_create_thm info mname name =
      let _ = edit_info info mname in
      let _ = cd info ("/" ^ mname) in
      let create name =
         let package = get_current_package info in
         let item = Shell_rule.create package (get_parse_arg info) (get_display_mode info) name in
            item.edit_save ();
            touch info
      in
         print_exn info create name;
         edit_cd_thm info mname name

   let edit_create_rw info mname name =
      let _ = edit_info info mname in
      let _ = cd info ("/" ^ mname) in
      let create name =
         let package = get_current_package info in
         let item = Shell_rewrite.create package (get_parse_arg info) (get_display_mode info) name in
            item.edit_save ();
            touch info
      in
         print_exn info create name;
         edit_cd_thm info mname name

   (*
    * Wrappers because Nuprl5 doesn't know about loading packages.
    *)
   let edit_set_goal info modname thmname t =
      edit_cd_thm info modname thmname;
      set_goal info t

   let edit_set_redex info modname thmname t =
      edit_cd_thm info modname thmname;
      set_redex info t

   let edit_set_contractum info modname thmname t =
      edit_cd_thm info modname thmname;
      set_contractum info t

   let edit_set_assumptions info modname thmname tl =
      edit_cd_thm info modname thmname;
      set_assumptions info tl

   let edit_set_params info modname thmname pl =
      edit_cd_thm info modname thmname;
      set_params info pl

   (*
    * Return the current node.
    *)
   let edit_addr info addr =
      let set addr =
         info.proof.edit_down addr
      in
         print_exn info set addr

   let edit_node info addr =
      let edit info =
         let proof = info.proof in
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
               Some tac, goal, children, extras
      in
         print_exn info edit info

   let edit_refine info addr str =
      let edit info =
         let proof = info.proof in
         let expr = ShellP4.parse_string info.shell str in
         let tac = ShellP4.eval_tactic info.shell expr in
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
               goal, children, extras
      in
         print_exn info edit info

   let edit_undo info =
      info.proof.edit_undo ()

   (************************************************************************
    * INITIALIZATION                                                       *
    ************************************************************************)

   (*
    * Print out an initialization file, and parse it.
    *)
   let main () =
      let info = global in
         ShellP4.set_module info.shell "Mptop" (commands info);
         ShellP4.main info.shell
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "editor.top"
 * End:
 * -*-
 *)
