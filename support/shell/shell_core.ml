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
 * Process control.
 *)

(*
 * Format strings.
 *)
let string_of_pid pid =
   let id, i = Lm_thread_shell.dest_pid pid in
      sprintf "%s.%d" id i

let pid_of_string s =
   try
      let index = String.rindex s '.' in
      let id = String.sub s 0 index in
      let i = int_of_string (String.sub s (succ index) (String.length s - index - 1)) in
         Lm_thread_shell.make_pid id i
   with
      Failure _
    | Not_found ->
         raise (RefineError ("Shell_core.pid_of_string", StringStringError ("illegal process identifier", s)))

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
      ignore (List.fold_left (fun first pid ->
                    if not first then
                       Buffer.add_string buf " ";
                    Buffer.add_string buf (string_of_pid pid);
                    false) true pids);
      Buffer.contents buf

(*
 * Switch jobs.
 *)
let fg shell pid =
   Lm_thread_shell.set_pid (pid_of_string pid)

(************************************************************************
 * Package management.
 *)

(*
 * Update the current item being edited.
 *)
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
 * Filename for the current shell.
 *)
let filename parse_arg shell =
   match shell.shell_package with
      Some pack ->
         Some (Package_info.filename pack parse_arg)
    | None ->
         None

(*
 * Display the current proof.
 *)
let display_proof info proof options =
   proof.edit_display options

(************************************************************************
 * Objects.
 *)

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
      { shell_package = Some pack; shell_dir = DirProof (mod_name, name, _) } ->
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
 | ProofMakeAssum
 | ProofExpand
 | ProofClean
 | ProofSquash ->
      true
 | ProofCp _
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

(************************************************************************
 * Directories and mounting.
 *)

(*
 * This is the critical function that decides mount points!!!
 *)
let dir_of_path path =
   match path with
      [] ->
         DirRoot
    | "fs" :: rest ->
         DirFS rest
    | [modname] ->
         DirModule modname
    | modname :: itemname :: subdir ->
         DirProof (modname, itemname, subdir)

(*
 * Turn the directory into a simple string path.
 *)
let path_of_dir dir =
   match dir with
      DirRoot ->
         []
    | DirFS rest ->
         "fs" :: rest
    | DirModule modname ->
         [modname]
    | DirProof (modname, itemname, subdir) ->
         modname :: itemname :: subdir

(*
 * Home directory for the current directory.
 *)
let home_of_dir dir =
   match dir with
      DirRoot
    | DirModule _ ->
         dir
    | DirFS _ ->
         DirFS []
    | DirProof (modname, _, _) ->
         DirModule modname

let root_of_dir dir =
   match dir with
      DirRoot
    | DirFS []
    | DirModule _ ->
         dir
    | DirFS (name :: _) ->
         DirFS [name]
    | DirProof (modname, itemname, _) ->
         DirProof (modname, itemname, [])

(*
 * Get the string version of the current directory.
 *)
let string_of_dir dir =
   match dir with
      DirRoot ->
         "/"
    | DirFS rest ->
         Lm_string_util.prepend "/" ("fs" :: rest)
    | DirModule modname ->
         "/" ^ modname
    | DirProof (modname, itemname, rest) ->
         Lm_string_util.prepend "/" (modname :: itemname :: rest)

(*
 * Turn a string into a path, relative to the current directory.
 *
 * The string is "/"-separated;
 *     "." means current directory,
 *     ".." its parent,
 *     "..." its grandparent etc.
 *
 * "~" refers to the second level from top.
 *)
let parse_path shell name =
   let home_dir = path_of_dir (home_of_dir shell.shell_dir) in
   let updir rev_dir count =
      let length = List.length rev_dir in
         if count >= List.length rev_dir then
            []
         else
            Lm_list_util.nth_tl count rev_dir
   in
   let rec collect rev_dir names =
      match names with
         [] ->
            rev_dir
       | "" :: ns ->
            collect rev_dir ns
       | "~" :: ns ->
            collect home_dir ns
       | n :: ns ->
            if (Lm_string_util.for_all (fun c -> c = '.') n) then
               let count = String.length n in
               let rev_dir = updir rev_dir (count - 1) in
                  collect rev_dir ns
            else
               collect (n :: rev_dir) ns
   in
   let rev_dir =
      if String.length name <> 0 && name.[0] = '/' then
         []
      else
         List.rev (path_of_dir shell.shell_dir)
   in
   let path = Lm_string_util.split "/" name in
      dir_of_path (List.rev (collect rev_dir path))

(*
 * Mount the root directory.
 *)
let mount_root parse_arg shell need_shell verbose =
   let proof = Shell_root.view packages (get_display_mode shell) in
      shell.shell_package <- None;
      shell.shell_proof <- proof;
      Shell_state.set_dfbase None;
      Shell_state.set_mk_opname None;
      Shell_state.set_so_var_context None;
      Shell_state.set_infixes None;
      Shell_state.set_module "shell_theory"

(*
 * Mount the FS directory.
 *)
let mount_fs parse_arg shell need_shell verbose =
   let proof = Shell_fs.view (get_display_mode shell) in
      shell.shell_package <- None;
      shell.shell_proof <- proof;
      Shell_state.set_dfbase None;
      Shell_state.set_mk_opname None;
      Shell_state.set_so_var_context None;
      Shell_state.set_infixes None;
      Shell_state.set_module "shell_theory"

(*
 * Helper for mounting a module.
 *)
let mount_current_module modname parse_arg shell need_shell verbose =
   let update =
      match shell.shell_dir with
         DirRoot
       | DirFS _ ->
            true
       | DirModule modname'
       | DirProof (modname', _, _) ->
            modname' <> modname
   in
      if update then
         begin
            (* Make sure the module name is well-formed *)
            if modname <> String.uncapitalize modname then
               raise (Failure "Shell_core: module name should not be capitalized");

            (* See if the theory exists *)
            let _ = Theory.get_theory modname in
            let pack = Package_info.load packages parse_arg modname in
               if need_shell && not (shell_package pack) then
                  failwith ("Module " ^ modname ^ " does not contain shell commands");
               shell.shell_package <- Some pack;
               Shell_state.set_dfbase (Some (get_db shell));
               Shell_state.set_mk_opname (Some (Package_info.mk_opname pack));
               Shell_state.set_infixes (Some (Package_info.get_infixes pack));
               Shell_state.set_module modname;
               if verbose then
                  eprintf "Module: /%s%t" modname eflush
         end

(*
 * Actually mount the module.
 *)
let mount_module modname parse_arg shell need_shell verbose =
   (* Make sure the module is mounted *)
   mount_current_module modname parse_arg shell need_shell verbose;

   (* Set the state *)
   Shell_state.set_so_var_context None;
   set_package parse_arg shell modname;

   (* Set the proof *)
   let pack = Package_info.get packages modname in
   let display_mode = get_display_mode shell in
   let proof = Shell_package.view pack parse_arg display_mode in
      (* Set the proof *)
      shell.shell_proof <- proof

(*
 * Mount a specific proof.
 *)
let mount_proof modname itemname parse_arg shell need_shell verbose =
   (* Make sure the module is mounted *)
   mount_current_module modname parse_arg shell need_shell verbose;

   (* Install the proof *)
   let proof = get_item parse_arg shell modname itemname in
      Shell_state.set_so_var_context (Some (proof.edit_get_terms ()));
      shell.shell_proof <- proof

(*
 * We use mount points as abstract names to do not include
 * the subdirectory.
 *)
type shell_mount =
   MountRoot
 | MountFS
 | MountModule of string
 | MountProof of string * string

(*
 * We use "mount" points to decide what to edit.
 * The mount function returns a description of the
 * mount point, and the mount function.
 *)
let mount_of_dir dir =
   match dir with
      DirRoot ->
         MountRoot, mount_root, []
    | DirFS rest ->
         MountFS, mount_fs, rest
    | DirModule modname ->
         MountModule modname, mount_module modname, []
    | DirProof (modname, itemname, rest) ->
         MountProof (modname, itemname), mount_proof modname itemname, rest

(*
 * When the mount point changes,
 * the old mount point may need to be unmounted.
 *)
let umount shell mount =
   match mount with
      MountProof (modname, itemname) ->
         shell.shell_proof.edit_addr []
    | MountModule _
    | MountRoot
    | MountFS ->
         ()

(*
 * Change directory.
 *)
let rec chdir parse_arg shell need_shell verbose path =
   let dir = shell.shell_dir in
   let old_mount_name, _, _ = mount_of_dir dir in
   let new_mount_name, mount, subdir = mount_of_dir path in
      try
         (* Change the mount point if needed *)
         if new_mount_name <> old_mount_name then
            begin
               umount shell old_mount_name;
               mount parse_arg shell need_shell verbose
            end;

         (* Save the directory *)
         shell.shell_dir <- path;

         (* Change to the subdirectory *)
         shell.shell_proof.edit_addr subdir
      with
         exn ->
            (* Some kind of failure happened, so change back to where we came from *)
            eprintf "Chdir to \"%s\" failed@." (string_of_dir path);
            eprintf "Going back to \"%s\"@." (string_of_dir dir);
            chdir parse_arg shell false verbose dir;
            raise exn

(*
 * Refresh the current directory.
 *)
let refresh parse_arg shell =
   let dir = shell.shell_dir in
      chdir parse_arg shell false false DirRoot;
      chdir parse_arg shell true true dir

let cd parse_arg shell name =
   chdir parse_arg shell true true (parse_path shell name);
   string_of_dir shell.shell_dir

let root parse_arg shell =
   let dir = root_of_dir shell.shell_dir in
      chdir parse_arg shell true true dir;
      string_of_dir shell.shell_dir

let pwd shell =
   string_of_dir shell.shell_dir

let fs_cwd shell =
   shell.shell_proof.edit_fs_cwd ()

(************************************************************************
 * Viewing.
 *)

(*
 * Window width.
 *)
let set_window_width shell i =
   shell.shell_width <- max !Mp_term.min_screen_width i

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
      Some pack ->
         let mk_opname = Package_info.mk_opname pack in
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

(*
 * General purpose displayer.
 *)
let view parse_arg shell options =
   display_proof shell shell.shell_proof options

(************************************************************************
 * Proof operations.
 *)

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
                     chdir parse_arg shell false true (DirModule name);
                     apply_all_exn false
               in
                  List.iter expand (all_packages ())
   in
      apply_all_exn time;
      refresh parse_arg shell

let expand_all parse_arg shell =
   let f item db =
      item.edit_interpret ProofExpand
   in
      apply_all parse_arg shell f false true true

(*
 * TeX functions.
 *)
let print_theory parse_arg shell name =
   let mode = shell.shell_df_mode in
   let dir = shell.shell_dir in
      shell.shell_df_mode <- "tex";
      chdir parse_arg shell false true (DirModule name);
      expand_all parse_arg shell;
      view parse_arg shell (LsOptionSet.singleton LsAll);
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
      { shell_package = Some pack; shell_dir = DirProof (mod_name, name, _) } ->
         Refine.extract_term (Package_info.get_refiner pack) (make_opname [name;mod_name]) terms
    | _ ->
         raise (Failure "Shell.term_of_extract only works inside a proof")

let edit_find info i =
   match info.shell_dir with
      DirProof (modname, name, _) ->
         let dir = DirProof (modname, name, List.map string_of_int (info.shell_proof.edit_find i)) in
            info.shell_dir <- dir;
            string_of_dir dir
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
      DirModule modname ->
         (* Top level *)
         let _ = Package_info.create_package packages parse_arg modname in
            view parse_arg shell LsOptionSet.empty
    | DirRoot ->
         raise (Failure "Shell.create_package: can't create root package")
    | DirFS _ ->
         raise (Failure "Shell.create_package: can't create a filesystem package")
    | DirProof _ ->
         raise (Failure "Shell.create_package: packages can't be nested right now")

(*
 * Save the current package.
 *)
let backup parse_arg shell =
   match shell.shell_package with
      Some pack ->
         eprintf "Auto-saving %s@." (Package_info.name pack);
         Package_info.backup parse_arg pack
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

let export parse_arg shell =
   touch shell;
   match shell.shell_package with
      Some pack ->
         eprintf "Exporting %s@." (Package_info.name pack);
         Package_info.export parse_arg pack
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

(*
 * Walk over all packages.
 *)
let backup_all parse_arg _ =
   let backup pack =
      if Package_info.status pack = PackModified then
         begin
            eprintf "Auto-saving %s@." (Package_info.name pack);
            Package_info.backup parse_arg pack
         end
   in
      List.iter backup (all_packages ())

let save_all parse_arg _ =
   let save pack =
      if Package_info.status pack = PackModified then
         begin
            eprintf "Saving %s@." (Package_info.name pack);
            Package_info.save parse_arg pack
         end
   in
      List.iter save (all_packages ())

let export_all parse_arg _ =
   let save pack =
      if Package_info.status pack = PackModified then
         begin
            eprintf "Exporting %s@." (Package_info.name pack);
            Package_info.export parse_arg pack
         end
   in
      List.iter save (all_packages ())

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

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
