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

open Lm_debug
open Lm_rprintf
open Lm_threads

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
     mutable clear_view_options : string -> unit
   }

let uninitialized _ = raise (Invalid_argument "The Shell module was not instantiated")

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
     clear_view_options = uninitialized
   }

(************************************************************************
 * TYPES                                                                *
 ************************************************************************)

(*
 * This is the info we need for each subshell.
 * The subshells can be threaded; we must make this shell
 * thread-safe.
 *)
type df_info =
   DisplayJavaInfo of Java_mux_channel.session
 | DisplayOtherInfo

type info =
   { (* Display *)
      mutable shell_width   : int;
      mutable shell_df_mode : string;
      mutable shell_df_info : df_info;

      (* Current module and path and proof *)
      mutable shell_dir     : string list;
      mutable shell_package : Package_info.package option;
      mutable shell_proof   : edit_object;

      (* Display options *)
      mutable shell_view_options : LsOptionSet.t;

      (* Process identifier *)
      shell_pid             : string;

      (* Handle to current shell *)
      shell_shell           : Shell_state.t
   }

let create_pid =
   let pid = ref 0 in
      (fun () ->
            let id = succ !pid in
            let name = string_of_int id in
               pid := id;
               name)

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
         match info.shell_df_info with
            DisplayJavaInfo port ->
               DisplayJava (port, dfbase)
          | DisplayOtherInfo ->
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
   let global =
      let port = None in
      let shell = ShellP4.get_current_state () in
      let dfmode = "prl" in
      let display_mode = DisplayText (default_mode_base, dfmode) in
      let proof = Shell_root.create packages display_mode in
      let port =
         match port with
            Some port ->
               DisplayJavaInfo port
          | None ->
               DisplayOtherInfo
      in
         { shell_width = 80;
           shell_df_mode = dfmode;
           shell_dir = [];
           shell_package = None;
           shell_df_info = port;
           shell_proof = proof;
           shell_shell = shell;
           shell_pid = create_pid ();
           shell_view_options = ls_options_default
         }

   (*
    * Global values.
    *)
   let global_lock = Mutex.create ()

   let global_shells = ref [global]

   let current_shell = ref global

   let get_current_shell () =
      !current_shell

   let synchronize_shell shell f x =
      Mutex.lock global_lock;
      let old_shell = !current_shell in
         current_shell := shell;
         try
            let result = f x in
               current_shell := old_shell;
               Mutex.unlock global_lock;
               result
         with
            exn ->
               current_shell := old_shell;
               Mutex.unlock global_lock;
               raise exn

   let synchronize shell f x =
      Mutex.lock global_lock;
      try
         let result = f x in
            Mutex.unlock global_lock;
            result
      with
         exn ->
            Mutex.unlock global_lock;
            let buf = Lm_rformat.new_buffer () in
            let db = get_db shell in
               Filter_exn.format_exn db buf exn;
               output_rbuffer stderr buf;
               flush stderr;
               raise exn

   (*
    * Get the current package.
    *)
   let get_current_package info =
      match info.shell_package with
         Some pack ->
            pack
       | None ->
            raise (RefineError ("Shell.get_current_package", StringError "no current package"))

   (*
    * Parsing arguments come from the shell.
    *)
   let get_parse_arg info =
      let shell = info.shell_shell in
      let parse = ShellP4.parse_string shell in
      let eval = ShellP4.eval_tactic shell in
         parse, eval

   (*
    * Update the timestamp.
    *)
   let touch info =
      let pack = get_current_package info in
         try Package_info.touch pack with
            Failure _ ->
               begin
                  (* Change the status so that we can write to the file. *)
                  Package_info.set_status pack PackModified;
                  Package_info.touch pack
               end

   (*
    * Display possible exceptions.
    *)
   let print_exn info f x =
      try Filter_exn.print_exn (get_db info) None f x with
         RefineError _ ->
            raise (RefineError ("Shell", ToploopIgnoreError))

   let synchronize_shell_print_exn shell f x =
      synchronize_shell shell (print_exn shell f) x

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
   let view info options name =
      let dir = parse_path info name in
      let view =
         match dir with
            [] ->
               view_packages info
          | [modname] ->
               view_package info modname
          | modname :: item :: _ ->
               view_item info modname item
      in
         print_exn info view options

   (*
    * Window width.
    *)
   let set_window_width info i =
      info.shell_width <- max !Mp_term.min_screen_width i

   (************************************************************************
    * PROCESS CONTROL                                                      *
    ************************************************************************)

   (*
    * Copy the current shell.
    *)
   let fork =
      let fork shell =
         let { shell_df_info = old_port;
               shell_proof = old_proof;
               shell_shell = old_shell
             } = shell
         in
         let new_port =
            match old_port with
               DisplayJavaInfo session ->
                  DisplayJavaInfo (Java_mux_channel.new_session session)
             | DisplayOtherInfo ->
                  DisplayOtherInfo
         in
         let new_proof = old_proof.edit_copy () in
         let new_shell = Shell_state.fork old_shell in
         let new_pid = create_pid () in
         let new_shell =
            { shell with shell_df_info = new_port;
                         shell_proof = new_proof;
                         shell_shell = new_shell;
                         shell_pid = new_pid
            }
         in
            global_shells := new_shell :: !global_shells;
            current_shell := new_shell;
            ShellP4.set_current_state new_shell.shell_shell;
            new_shell
      in
         (fun shell -> synchronize shell fork shell)

   (*
    * Show current process id.
    *)
   let pid shell =
      shell.shell_pid

   (*
    * Show all process names.
    *)
   let jobs shell =
      let jobs shell =
         let project shell =
            let pid =
               match shell.shell_df_info with
                  DisplayJavaInfo session ->
                     Java_mux_channel.id_of_session session
                | DisplayOtherInfo ->
                     0
            in
               pid, shell
         in
         let compare (pid1, _) (pid2, _) =
            pid1 < pid2
         in
         let shells = List.map project !global_shells in
         let shells = Sort.list compare shells in
            String.concat "\n" (List.map (fun (_, shell) -> shell.shell_pid) shells)
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
            if shell.shell_pid = id then
               shell
            else
               search tl
      in
         search !global_shells

   let fg shell =
      let fg id =
         let shell = shell_of_id id in
            current_shell := shell;
            ShellP4.set_current_state shell.shell_shell;
            shell.shell_pid
      in
         synchronize shell fg

   (*
    * Interface to the HTTP shell.
    *)
   let set_port port =
      let port =
         match port with
            Some port ->
               DisplayJavaInfo port
          | None ->
               DisplayOtherInfo
      in
         global.shell_df_info <- port;
         try
            print_exn global (fun info -> view info LsOptionSet.empty ".") global
         with
            exn ->
               ()

   let flush info =
      view info info.shell_view_options "."

   let get_ls_options info =
      info.shell_view_options

   let get_view_options info =
      string_of_ls_options info.shell_view_options

   let set_view_options info s =
      info.shell_view_options <- ls_options_add info.shell_view_options s

   let clear_view_options info s =
      info.shell_view_options <- ls_options_clear info.shell_view_options s

   let get_shortener info =
      match info.shell_package with
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
   let create_pkg info name =
      match parse_path info name with
         [modname] ->
            (* Top level *)
            let _ = Package_info.create_package packages (get_parse_arg info) modname in
               view info LsOptionSet.empty name
       | [] ->
            raise (Failure "Shell.create_package: can't create root package")
       | _ ->
            raise (Failure "Shell.create_package: packages can't be nested right now")

   (*
    * Save the current package.
    *)
   let save info =
      match info.shell_package with
         Some pack ->
            Package_info.save pack
       | None ->
            ()

   let export info =
      touch info;
      match info.shell_package with
         Some pack ->
            Package_info.export (get_parse_arg info) pack
       | None ->
            ()

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
         info.shell_proof.edit_set_goal t;
         display_proof info info.shell_proof LsOptionSet.empty
      in
         print_exn info set t

   let set_redex info t =
      let set t =
         touch info;
         info.shell_proof.edit_set_redex t;
         display_proof info info.shell_proof LsOptionSet.empty
      in
         print_exn info set t

   let set_contractum info t =
      let set t =
         touch info;
         info.shell_proof.edit_set_contractum t;
         display_proof info info.shell_proof LsOptionSet.empty
      in
         print_exn info set t

   let set_assumptions info tl =
      let set t =
         touch info;
         info.shell_proof.edit_set_assumptions tl;
         display_proof info info.shell_proof LsOptionSet.empty
      in
         print_exn info set tl

   let set_params info pl =
      let set t =
         touch info;
         info.shell_proof.edit_set_params pl;
         display_proof info info.shell_proof LsOptionSet.empty
      in
         print_exn info set pl

   let check info =
      let check = function
         { shell_package = Some pack; shell_dir = mod_name :: name :: _ } ->
            begin
               try
                  let deps = Refine.compute_dependencies (Package_info.get_refiner pack) (make_opname [name;mod_name]) in
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
      in
         print_exn info check info

   let expand info =
      let set info =
         let start = Unix.times () in
         let start_time = Unix.gettimeofday () in
         let () = info.shell_proof.edit_interpret ProofExpand in
         let finish = Unix.times () in
         let finish_time = Unix.gettimeofday () in
            display_proof info info.shell_proof LsOptionSet.empty;
            eprintf "User time %f; System time %f; Real time %f%t" (**)
               ((finish.Unix.tms_utime +. finish.Unix.tms_cutime)
                -. (start.Unix.tms_utime +. start.Unix.tms_cstime))
               ((finish.Unix.tms_stime +. finish.Unix.tms_cstime)
                -. (start.Unix.tms_stime +. finish.Unix.tms_cstime))
               (finish_time -. start_time)
               eflush
      in
         print_exn info set info

   let refine info tac =
      let set info =
         let str, ast = Shell_state.get_tactic info.shell_shell in
            touch info;
            if !debug_refine then
               eprintf "Starting refinement%t" eflush;
            info.shell_proof.edit_refine str ast tac;
            if !debug_refine then
               eprintf "Displaying proof%t" eflush;
            if Shell_state.is_interactive info.shell_shell then
               display_proof info info.shell_proof LsOptionSet.empty;
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
         (info.shell_proof.edit_info ()).edit_goal
      in
         print_exn info get info

   let undo info =
      let set info =
         touch info;
         info.shell_proof.edit_undo ();
         display_proof info info.shell_proof LsOptionSet.empty
      in
         print_exn info set info

   let redo info =
      let set info =
         touch info;
         info.shell_proof.edit_redo ();
         display_proof info info.shell_proof LsOptionSet.empty
      in
         print_exn info set info

   let interpret info command =
      let set info =
         touch info;
         info.shell_proof.edit_interpret command;
         display_proof info info.shell_proof LsOptionSet.empty
      in
         print_exn info set info

   let rec apply_all info f time clean_res =
      let dir = info.shell_dir in
      let apply_it item mod_name name =
         (*
          * Here we indeed want to ignore absolutely any kind of error.
          * Whatever went wrong, we just want to skip the bad item and continue to the next one.
          *)
         (try Shell_state.set_so_var_context info.shell_shell (Some (item.edit_get_terms ())) with _ -> ());
         (try f item (get_db info) with _ -> ());
         if clean_res then Mp_resource.clear_results (mod_name, name)
      in
      let rec apply_all_exn time =
         let parse_arg = get_parse_arg info in
         let display_mode = get_display_mode info in
         match info.shell_package with
            Some pack ->
               touch info;
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
                     chdir info false true [name];
                     apply_all_exn false
               in
                  List.iter expand (all_packages());
      in
         print_exn info apply_all_exn time;
         chdir info false false [];
         chdir info false false dir

   and expand_all info =
      let f item db =
         item.edit_interpret ProofExpand
      in
         apply_all info f true true

   (*
    * Change directory.
    *)
   and chdir info need_shell verbose path =
      let shell = info.shell_shell in
      match path with
         [] ->
            (* go to toplevel *)
            info.shell_dir <- [];
            info.shell_package <- None;
            set_packages info;
            Shell_state.set_dfbase shell None;
            Shell_state.set_mk_opname shell None;
            Shell_state.set_so_var_context shell None;
            Shell_state.set_infixes shell None;
            Shell_state.set_module shell "shell"
       | (modname :: item) as dir ->
            (* change module only if in another (or at top) *)
            if info.shell_dir = [] || List.hd info.shell_dir <> modname then
               begin
                  if modname <> String.uncapitalize modname then
                     raise (Failure "Shell.chdir: module name should not be capitalized");

                  (* See if the theory exists *)
                  ignore (Theory.get_theory modname);
                  let pkg = Package_info.load packages (get_parse_arg info) modname in
                     if need_shell && not (shell_package pkg) then
                        failwith ("Module " ^ modname ^ " does not contain shell commands");
                     info.shell_package <- Some pkg;
                     Shell_state.set_dfbase shell (Some (get_db info));
                     Shell_state.set_mk_opname shell (Some (Package_info.mk_opname pkg));
                     Shell_state.set_infixes shell (Some (Package_info.get_infixes pkg));
                     Shell_state.set_module shell modname;
                     if verbose then
                        eprintf "Module: /%s%t" modname eflush
               end;

            if item = [] then
               begin
                  (* top of module *)
                  info.shell_dir <- dir;
                  Shell_state.set_so_var_context shell None;
                  set_package info modname
               end
            else
               begin
                  (* select an item (if not there already), then go down the proof. *)
                  begin match info.shell_dir with
                     old_modname::old_item::_ when old_modname = modname && old_item = List.hd item ->
                        info.shell_proof.edit_addr (List.map int_of_string (List.tl item))
                   | _ ->
                        try
                           (* Leave the old proof at the root *)
                           info.shell_proof.edit_addr [];
                           let proof = get_item info modname (List.hd item) in
                              Shell_state.set_so_var_context shell (Some (proof.edit_get_terms ()));
                              proof.edit_addr (List.map int_of_string (List.tl item));
                              info.shell_proof <- proof
                        with exn -> begin
                           (* Bring things back to where they were *)
                           let dir = info.shell_dir in
                              if (dir <> []) && (List.hd dir = modname) then info.shell_dir <- [modname] else info.shell_dir <- [];
                              chdir info false verbose dir;
                              raise exn
                        end;
                  end;
                  info.shell_dir <- dir
               end

   and cd info name =
      print_exn info (chdir info true true) (parse_path info name);
      string_of_path info.shell_dir

   (*
    * TeX functions.
    *)
   and print_theory info name =
      let f () =
         let mode = info.shell_df_mode in
         let dir = info.shell_dir in
            info.shell_df_mode <- "tex";
            chdir info false true [name];
            expand_all info;
            view info LsOptionSet.empty ".";
            info.shell_df_mode <- mode;
            chdir info false false dir
      in
         print_exn info f ()

   let extract info path () =
      let dir = info.shell_dir in
      try
         chdir info false false path;
         let res = info.shell_proof.edit_get_extract () in
            chdir info false false dir;
            res
      with
         exn ->
            chdir info false false dir;
            raise exn

   let term_of_extract info terms =
      match info with
         { shell_package = Some pack; shell_dir = mod_name :: name :: _ } ->
            Refine.extract_term (Package_info.get_refiner pack) (make_opname [name;mod_name]) terms
       | _ ->
            raise (Failure "Shell.term_of_extract only works inside a proof")

   (************************************************************************
    * NUPRL5 INTERFACE                                                     *
    ************************************************************************)

   (*
    * Return the list of all module names.
    *)
   let edit_list_modules info =
      let list info =
         List.map Package_info.name (all_packages ())
      in
         print_exn info list info

   let edit_info info name =
      let parse_arg = get_parse_arg info in
      let edit info =
         try Package_info.info (Package_info.get packages name) parse_arg with
            NotLoaded _ ->
               eprintf "Loading package %s%t" name eflush;
               ignore (Package_info.load packages (get_parse_arg info) name);
               Package_info.info (Package_info.get packages name) parse_arg
      in
         print_exn info edit info

   let edit_save info name =
      let edit info =
         Package_info.save (Package_info.get packages name)
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
                  Rewrite { rw_name = name }
                | CondRewrite { crw_name = name }
                | Rule { rule_name = name } ->
                     name :: names
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

   let edit_list_dforms info name =
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

   let edit_cd_list_contents info mname =
      let objs = ref [] in
      let f obj _ = objs := (obj.edit_get_contents ())::(!objs) in begin
         print_exn info (chdir info false false) [mname];
         apply_all info f false false
      end;
      List.rev !objs

   (*
    * Create a new thm.
    *)
   let edit_cd_thm info mname name =
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
      let opens = collect (info_items (edit_info info mname)) in
      ignore (print_exn info (chdir info false false) [mname; name]; ShellP4.eval_opens info.shell_shell opens)

   let edit_create_thm info mname name =
      ignore (edit_info info mname);
      chdir info false false [mname];
      let create name =
         let package = get_current_package info in
         let item = Shell_rule.create package (get_parse_arg info) (get_display_mode info) name in
            item.edit_save ();
            touch info
      in
         print_exn info create name;
         edit_cd_thm info mname name

   let edit_create_rw info mname name =
      ignore (edit_info info mname);
      chdir info false false [mname];
      let create name =
         let package = get_current_package info in
         let item = Shell_rule.create package (get_parse_arg info) (get_display_mode info) name in
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
         info.shell_proof.edit_down addr
      in
         print_exn info set addr

   let edit_node info addr =
      let edit info =
         let proof = info.shell_proof in
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
         let proof = info.shell_proof in
         let expr = ShellP4.parse_string info.shell_shell str in
         let tac = ShellP4.eval_tactic info.shell_shell expr in
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
      info.shell_proof.edit_undo ()

   (************************************************************************
    * Toplevel functions.
    *)

   (*
    * Evaluate an expression in some shell.
    *)
   let eval_top shell text =
      try
         synchronize_shell_print_exn shell (fun text ->
               begin
                  match shell.shell_df_info with
                     DisplayJavaInfo port ->
                        eprintf "Shell [%d]: %s%t" (Java_mux_channel.id_of_session port) text eflush
                   | DisplayOtherInfo ->
                        ()
               end;
               ShellP4.eval_top shell.shell_shell text) text
      with
         End_of_file as exn ->
            raise exn
       | exn ->
            ()

   let chdir_top shell text =
      try
         synchronize_shell_print_exn shell (fun text ->
               begin
                  match shell.shell_df_info with
                     DisplayJavaInfo port ->
                        eprintf "Shell [%d]: %s%t" (Java_mux_channel.id_of_session port) text eflush
                   | DisplayOtherInfo ->
                        ()
               end;
               ignore (cd shell text);
               true) text
      with
         exn ->
            false

   (*
    * Evaluate an expression in some shell (for the Java interface).
    *)
   let find_shell id =
      synchronize !current_shell shell_of_id id

   (************************************************************************
    * INITIALIZATION                                                       *
    ************************************************************************)

   (*
    * Print out an initialization file, and parse it.
    *)
   let refresh_packages () =
      Package_info.refresh packages (Shell_state.get_includes ())

   let main () =
      refresh_packages ();
      let info = global in
         Shell_state.set_module info.shell_shell "shell";
         ShellP4.main info.shell_shell

   let wrap cmd arg = cmd !current_shell arg
   let wrap_unit cmd () = cmd !current_shell

   let _ =
      if commands.cd != uninitialized then
         raise (Invalid_argument "The Shell module was initialized twice");
      commands.cd <- wrap cd;
      commands.pwd <- (fun () -> !current_shell.shell_dir);
      commands.set_dfmode <- wrap set_dfmode;
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
      commands.extract <- (fun path () -> extract !current_shell path ());
      commands.term_of_extract <- wrap term_of_extract;
      commands.get_view_options <- wrap_unit get_view_options;
      commands.set_view_options <- wrap set_view_options;
      commands.clear_view_options <- wrap clear_view_options;
      ()
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
