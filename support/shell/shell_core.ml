(*
 * Basic shell functions that do not depend on a toploop.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2004 Mojave Group, Caltech
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
 * @email{jyh@cs.caltech.edu}
 * @end[license]
 *)
open Lm_debug
open Lm_rprintf

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

open Shell_sig
open Shell_util
open Shell_internal_sig

let debug_refine = load_debug "refine"

let debug_shell =
   create_debug (**)
      { debug_name = "shell";
        debug_description = "Display shell operations";
        debug_value = false
      }

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
   "/" ^ String.concat "/" (List.rev (dest_opname opname))

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
 * Get the current package.
 *)
let get_current_package shell =
   match shell.shell_package with
      Some pack ->
         pack
    | None ->
         raise (RefineError ("Shell.get_current_package", StringError "no current package"))

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
let get_ls_options shell =
   Session.get_view_options ()

let get_view_options shell =
   string_of_ls_options (get_ls_options ())

let set_view_options shell s =
   Session.add_view_options s

let clear_view_options shell s =
   Session.clear_view_options s

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

(*
 * Update the current item being edited.
 *)
let set_packages info =
   info.shell_proof.edit_addr [];
   info.shell_proof <- Shell_root.view packages (get_display_mode info)

let set_package parse_arg info modname =
   let pack = Package_info.get packages modname in
      info.shell_proof.edit_addr [];
      info.shell_proof <- Shell_package.view pack parse_arg (get_display_mode info)

let get_item parse_arg info modname name =
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
let view_package parse_arg info name options =
   let pack = Package_info.get packages name in
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
let view parse_arg shell options name =
   let dir = parse_path shell name in
      match dir with
         [] ->
            view_packages shell options
       | [modname] ->
            view_package parse_arg shell modname options
       | modname :: item :: _ ->
            view_item shell modname item options

(*
 * Filename for the current shell.
 *)
let filename parse_arg shell =
   match shell.shell_package with
      Some pack ->
         Some (Package_info.filename pack parse_arg)
    | None ->
         None

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

let create_ax_statement parse_arg shell seq name =
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
   let str, ast = Shell_state.get_tactic () in
      touch shell;
      if !debug_refine then
         eprintf "Starting refinement%t" eflush;
      shell.shell_proof.edit_refine str ast tac;
      if !debug_refine then
         eprintf "Displaying proof%t" eflush;
      if Shell_state.is_interactive () then
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

let interpret_modifies = function
   ProofRefine _
 | ProofUndo
 | ProofRedo
 | ProofKreitz
 | ProofRotate _
 | ProofPaste _
 | ProofMakeAssum ->
      true
 | ProofCp _
 | ProofExpand
 | ProofClean
 | ProofSquash
 | ProofCopy _
 | ProofAddr _
 | ProofUp _
 | ProofDown _
 | ProofRoot
 | ProofNop ->
      false

let interpret shell command =
   if interpret_modifies command then
      touch shell;
   shell.shell_proof.edit_interpret command;
   display_proof shell shell.shell_proof LsOptionSet.empty

(*
 * Change directory.
 *)
let rec chdir parse_arg shell need_shell verbose path =
   match path with
      [] ->
         (* go to toplevel *)
         shell.shell_dir <- [];
         shell.shell_package <- None;
         set_packages shell;
         Shell_state.set_dfbase None;
         Shell_state.set_mk_opname None;
         Shell_state.set_so_var_context None;
         Shell_state.set_infixes None;
         Shell_state.set_module "shell"

    | (modname :: item) as dir ->
         (* change module only if in another (or at top) *)
         if shell.shell_dir = [] || List.hd shell.shell_dir <> modname then
            begin
               if modname <> String.uncapitalize modname then
                  raise (Failure "Shell.chdir: module name should not be capitalized");

               (* See if the theory exists *)
               ignore (Theory.get_theory modname);
               let pkg = Package_info.load packages parse_arg modname in
                  if need_shell && not (shell_package pkg) then
                     failwith ("Module " ^ modname ^ " does not contain shell commands");
                  shell.shell_package <- Some pkg;
                  Shell_state.set_dfbase (Some (get_db shell));
                  Shell_state.set_mk_opname (Some (Package_info.mk_opname pkg));
                  Shell_state.set_infixes (Some (Package_info.get_infixes pkg));
                  Shell_state.set_module modname;
                  if verbose then
                     eprintf "Module: /%s%t" modname eflush
            end;

         if item = [] then
            begin
               (* top of module *)
               shell.shell_dir <- dir;
               Shell_state.set_so_var_context None;
               set_package parse_arg shell modname
            end
         else
            begin
               (* select an item (if not there already), then go down the proof. *)
               begin
                  match shell.shell_dir with
                     old_modname :: old_item :: _ when old_modname = modname && old_item = List.hd item ->
                        shell.shell_proof.edit_addr (List.map int_of_string (List.tl item))
                   | _ ->
                        try
                           (* Leave the old proof at the root *)
                           shell.shell_proof.edit_addr [];
                           let proof = get_item parse_arg shell modname (List.hd item) in
                              Shell_state.set_so_var_context (Some (proof.edit_get_terms ()));
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
                                    chdir parse_arg shell false verbose dir;
                                    raise exn
                              end;
               end;
               shell.shell_dir <- dir
            end

(*
 * Refresh the current directory.
 *)
let refresh parse_arg shell =
   let dir = shell.shell_dir in
      shell.shell_dir <- [];
      chdir parse_arg shell true true dir

(*
 * Apply a function to all elements.
 *)
let rec apply_all parse_arg shell f (modifies : bool) (time : bool) (clean_res : bool) =
   let dir = shell.shell_dir in
   let apply_it item mod_name name =
      (*
       * Here we indeed want to ignore absolutely any kind of error.
       * Whatever went wrong, we just want to skip the bad item and continue to the next one.
       *)
      (try Shell_state.set_so_var_context (Some (item.edit_get_terms ())) with
          _ ->
             ());
      (try f item (get_db shell) with
          _ ->
             ());
      if clean_res then
         Mp_resource.clear_results (mod_name, name)
   in
   let rec apply_all_exn time =
      let display_mode = get_display_mode shell in
         match shell.shell_package with
            Some pack ->
               if modifies then
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
                     chdir parse_arg shell false true [name];
                     apply_all_exn false
               in
                  List.iter expand (all_packages ())
   in
      apply_all_exn time;
      refresh parse_arg shell

and expand_all parse_arg shell =
   let f item db =
      item.edit_interpret ProofExpand
   in
      apply_all parse_arg shell f false true true

and cd parse_arg shell name =
   chdir parse_arg shell true true (parse_path shell name);
   string_of_path shell.shell_dir

(*
 * TeX functions.
 *)
and print_theory parse_arg shell name =
   let mode = shell.shell_df_mode in
   let dir = shell.shell_dir in
      shell.shell_df_mode <- "tex";
      chdir parse_arg shell false true [name];
      expand_all parse_arg shell;
      view parse_arg shell (LsOptionSet.singleton LsAll) ".";
      shell.shell_df_mode <- mode;
      chdir parse_arg shell false false dir

let extract parse_arg shell path () =
   let dir = shell.shell_dir in
      try
         chdir parse_arg shell false false path;
         let res = shell.shell_proof.edit_get_extract () in
            chdir parse_arg shell false false dir;
            res
      with
         exn ->
            chdir parse_arg shell false false dir;
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
 * MODULES                                                              *
 ************************************************************************)

(*
 * Make a new package.
 * Right now we only allow packages at the top level.
 *)
let create_pkg parse_arg shell name =
   match parse_path shell name with
      [modname] ->
         (* Top level *)
         let _ = Package_info.create_package packages parse_arg modname in
            view parse_arg shell LsOptionSet.empty name
    | [] ->
         raise (Failure "Shell.create_package: can't create root package")
    | _ ->
         raise (Failure "Shell.create_package: packages can't be nested right now")

(*
 * Save the current package.
 *)
let backup shell =
   match shell.shell_package with
      Some pack ->
         eprintf "Auto-saving %s@." (Package_info.name pack);
         Package_info.backup pack
    | None ->
         ()

let revert parse_arg shell =
   match shell.shell_package with
      Some pack ->
         eprintf "Reverting %s@." (Package_info.name pack);
         Package_info.revert pack;
         refresh parse_arg shell
    | None ->
         ()

let save parse_arg shell =
   touch shell;
   match shell.shell_package with
      Some pack ->
         eprintf "Saving %s@." (Package_info.name pack);
         Package_info.save parse_arg pack
    | None ->
         ()

(*
 * Walk over all packages.
 *)
let backup_all _ =
   let backup pack =
      if Package_info.status pack = PackModified then
         begin
            eprintf "Auto-saving %s@." (Package_info.name pack);
            Package_info.backup pack
         end
   in
      List.iter backup (all_packages ())

let revert_all shell =
   let revert pack =
      if Package_info.status pack = PackModified then
         begin
            eprintf "Reverting %s@." (Package_info.name pack);
            Package_info.revert pack
         end
   in
      List.iter revert (all_packages ());
      refresh shell

let save_all parse_arg _ =
   let save pack =
      if Package_info.status pack = PackModified then
         begin
            eprintf "Saving %s@." (Package_info.name pack);
            Package_info.save parse_arg pack
         end
   in
      List.iter save (all_packages ())

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
