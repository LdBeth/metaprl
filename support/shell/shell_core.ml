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
open Lm_string_set

open Opname
open Term_sig
open Refiner.Refiner
open Refiner.Refiner.Term
open Refiner.Refiner.TermType
open Refiner.Refiner.TermShape
open Refiner.Refiner.RefineError

open Dform
open Theory

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
 * We should skip the packages that do not have basic shell commands in them.
 *)
let shell_package_name name =
   try Mptop.mem (Mptop.get_toploop_resource (Mp_resource.find (Mp_resource.theory_bookmark name)) []) "cd" with
      Not_found ->
         false

let shell_package pkg =
   shell_package_name (Package_info.name pkg)

(*
 * All loaded modules.
 *)
let packages = Package_info.create (Shell_state.get_includes ())

let modified_packages () =
   List.filter shell_package (Package_info.modified_packages packages)

let all_theories () =
   let names = StringSet.empty in
   let names =
      List.fold_left (fun names pkg ->
            let name = Package_info.name pkg in
               if shell_package_name name then
                  StringSet.add names name
               else
                  names) names (Package_info.modified_packages packages)
   in
   let names =
      List.fold_left (fun names thy ->
            let name = thy.thy_name in
               if shell_package_name name then
                  StringSet.add names name
               else
                  names) names (get_theories ())
   in
      StringSet.to_list names

let default_mode_base = Mp_resource.theory_bookmark "shell_theory"

(*
 * Get the current printing base.
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

let get_display_mode info () =
   info.shell_df_method

let get_db info =
   let dfbase = get_dfbase info in
      get_mode_base dfbase info.shell_df_method.df_mode

let update_dfbase info =
   let dfbase = get_dfbase info in
      info.shell_df_method <- { info.shell_df_method with df_base = dfbase };
      Shell_state.set_dfbase (Some (get_mode_base dfbase info.shell_df_method.df_mode))

let set_dfmode info mode =
   info.shell_df_method <- { info.shell_df_method with df_mode = mode };
   Shell_state.set_dfbase (Some (get_mode_base info.shell_df_method.df_base mode))

let set_dftype info dft =
   info.shell_df_method <- { info.shell_df_method with df_type = dft }

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
   let pack = Package_info.load packages parse_arg modname in
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
       | DefineTerm (term, def) ->
            Shell_rule.view_def pack parse_arg display_mode term def
       | DeclareTypeClass _
       | DeclareType _
       | DeclareTerm _
       | DeclareTypeRewrite _ ->
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
       | MLGramUpd _ ->
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
       | PRLGrammar _ ->
            eprintf "Editing grammar '/%s/%s' not implemented%t" modname name eflush;
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
let display_proof info options =
   info.shell_proof.edit_display info.shell_subdir options

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
   display_proof shell LsOptionSet.empty

let set_redex shell t =
   touch shell;
   shell.shell_proof.edit_set_redex t;
   display_proof shell LsOptionSet.empty

let set_contractum shell t =
   touch shell;
   shell.shell_proof.edit_set_contractum t;
   display_proof shell LsOptionSet.empty

let set_assumptions shell tl =
   touch shell;
   shell.shell_proof.edit_set_assumptions tl;
   display_proof shell LsOptionSet.empty

let set_params shell pl =
   touch shell;
   shell.shell_proof.edit_set_params pl;
   display_proof shell LsOptionSet.empty

let mk_dep_name opname =
   Lm_string_util.prepend "/" (List.rev (dest_opname opname))

let check shell =
   match shell with
      { shell_package = Some pack; shell_fs = DirProof (_, mod_name, name) } ->
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
   let () = if shell.shell_proof.edit_interpret shell.shell_subdir ProofExpand then touch shell in
   let finish = Unix.times () in
   let finish_time = Unix.gettimeofday () in
      display_proof shell LsOptionSet.empty;
      eprintf "User time %f; System time %f; Real time %f%t" (**)
         ((finish.Unix.tms_utime +. finish.Unix.tms_cutime)
          -. (start.Unix.tms_utime +. start.Unix.tms_cstime))
         ((finish.Unix.tms_stime +. finish.Unix.tms_cstime)
          -. (start.Unix.tms_stime +. finish.Unix.tms_cstime))
         (finish_time -. start_time)
         eflush

let refine shell tac =
   let str, ast = Shell_state.get_tactic () in
      if !debug_refine then
         eprintf "Starting refinement%t" eflush;
      if shell.shell_proof.edit_interpret shell.shell_subdir (ProofRefine(str, ast, tac)) then
         touch shell;
      if !debug_refine then
         eprintf "Displaying proof%t" eflush;
      if Shell_state.is_interactive () then
         display_proof shell LsOptionSet.empty;
      if !debug_refine then
         eprintf "Proof displayed%t" eflush

(*
 * Get the current goal.
 *)
let goal shell =
   (shell.shell_proof.edit_info shell.shell_subdir).edit_goal

let interpret shell command =
   if shell.shell_proof.edit_interpret shell.shell_subdir command then
      touch shell;
   display_proof shell LsOptionSet.empty

(************************************************************************
 * Directories and mounting.
 *)

(*
 * This is the critical function that decides mount points!!!
 *)
let rec dir_of_path = function
   [] ->
      DirRoot, []
 | "fs" :: rest ->
      DirFS, rest
 | modname :: rest as path when not (Package_info.group_exists packages modname) ->
      let group =
         try Package_info.group_of_module packages modname with
            Not_found ->
               raise (Failure("Shell_core.dir_of_path: module " ^ modname ^ " does not exist"))
      in
         dir_of_path (group :: path)
 | [group] as path ->
      DirRoot, path
 | [group; modname] ->
      DirModule (group, modname), []
 | group :: modname :: itemname :: subdir ->
      DirProof (group, modname, itemname), subdir

(*
 * Turn the directory into a simple string path.
 *)
let path_of_dir = function
   DirRoot, addr ->
      addr
 | DirFS, rest ->
      "fs" :: rest
 | DirModule (group, modname), [] ->
      [group; modname]
 | DirProof (group, modname, itemname), rest ->
      group :: modname :: itemname :: rest
 | _ ->
      raise (Invalid_argument "Shell_core.path_of_dir: internal error")

(*
 * Home directory for the current directory.
 *)
let home_of_fs = function
   DirRoot
 | DirModule _
 | DirFS as fs ->
      fs
 | DirProof (group, modname, _) ->
      DirModule (group, modname)

let root_of_dir = function
   DirRoot, _ ->
      DirRoot, []
 | DirFS, []
 | DirModule _, [] as dir ->
      dir
 | DirFS, (name :: _) ->
      DirFS, [name]
 | DirProof _ as fs, _ ->
      fs, []
 | _ ->
      raise (Invalid_argument "Shell_core.root_of_dir: internal error")

(*
 * Get the string version of the current directory.
 *)
let string_of_dir dir =
   Lm_string_util.prepend "/" (path_of_dir dir)

let module_dir name =
   DirModule (Package_info.group_of_module packages name, name), []

let proof_dir modname name =
   DirProof (Package_info.group_of_module packages modname, modname, name), []

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
            collect (List.rev (path_of_dir ((home_of_fs shell.shell_fs), []))) ns
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
         List.rev (path_of_dir (shell.shell_fs, shell.shell_subdir))
   in
   let path = Lm_string_util.split "/" name in
      dir_of_path (List.rev (collect rev_dir path))

(*
 * Mount the root directory.
 *)
let mount_root parse_arg shell force_flag need_shell verbose =
   let proof = Shell_root.view packages (get_display_mode shell) in
      shell.shell_package <- None;
      update_dfbase shell;
      shell.shell_proof <- proof;
      Shell_state.set_mk_opname None;
      Shell_state.set_infer_term None;
      Shell_state.set_so_var_context None;
      Shell_state.set_infixes None;
      Shell_state.set_grammar None;
      Shell_state.set_module "shell_theory"

(*
 * Mount the FS directory.
 *)
let mount_fs parse_arg shell force_flag need_shell verbose =
   let proof = Shell_fs.view (get_display_mode shell) in
      shell.shell_package <- None;
      update_dfbase shell;
      shell.shell_proof <- proof;
      Shell_state.set_mk_opname None;
      Shell_state.set_infer_term None;
      Shell_state.set_so_var_context None;
      Shell_state.set_infixes None;
      Shell_state.set_grammar None;
      Shell_state.set_module "shell_theory"

(*
 * Helper for mounting a module.
 *)
let mount_current_module modname parse_arg shell force_flag need_shell verbose =
   let update =
      match shell.shell_fs with
         DirRoot
       | DirFS ->
            true
       | DirModule (_, modname')
       | DirProof (_, modname', _) ->
            modname' <> modname
   in
      if force_flag || update then
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
               update_dfbase shell;
               Shell_state.set_mk_opname (Some (Package_info.opname_prefix pack, Package_info.mk_opname_kind pack));
               Shell_state.set_infer_term (Some (Package_info.infer_term pack,
                                                 Package_info.check_rule pack,
                                                 Package_info.infer_rewrite pack,
                                                 Package_info.check_type_rewrite pack,
                                                 Package_info.check_iform pack,
                                                 Package_info.check_dform pack,
                                                 Package_info.check_production pack));
               Shell_state.set_infixes (Some (Package_info.get_infixes pack));
               Shell_state.set_grammar (Some (Package_info.get_grammar pack));
               Shell_state.set_module modname;
               if verbose then
                  eprintf "Module: /%s%t" modname eflush
         end

(*
 * Actually mount the module.
 *)
let mount_module modname parse_arg shell force_flag need_shell verbose =
   (* Make sure the module is mounted *)
   mount_current_module modname parse_arg shell force_flag need_shell verbose;

   (* Set the state *)
   Shell_state.set_so_var_context None;
   set_package parse_arg shell modname;

   (* Set the proof *)
   let pack = Package_info.load packages parse_arg modname in
   let display_mode = get_display_mode shell in
   let proof = Shell_package.view pack parse_arg display_mode in
      (* Set the proof *)
      shell.shell_proof <- proof

(*
 * Mount a specific proof.
 *)
let mount_proof modname itemname parse_arg shell force_flag need_shell verbose =
   (* Make sure the module is mounted *)
   mount_current_module modname parse_arg shell force_flag need_shell verbose;

   (* Install the proof *)
   let proof = get_item parse_arg shell modname itemname in
      Shell_state.set_so_var_context (Some (proof.edit_get_terms ()));
      shell.shell_proof <- proof

(*
 * We use "mount" points to decide what to edit.
 * The mount function returns a description of the
 * mount point, and the mount function.
 *)
let mount_of_fs = function
   DirRoot -> mount_root
 | DirFS -> mount_fs
 | DirModule (_, modname) -> mount_module modname
 | DirProof (_, modname, itemname) -> mount_proof modname itemname

(*
 * Change directory.
 *)
let rec chdir_full parse_arg shell force_flag need_shell verbose (new_fs, new_subdir) =
   let need_mount = force_flag || new_fs <> shell.shell_fs in
      if need_mount then begin
         try
            mount_of_fs new_fs parse_arg shell force_flag need_shell verbose;
            shell.shell_fs <- new_fs
         with
            exn ->
               mount_of_fs shell.shell_fs parse_arg shell false false false;
               raise exn
      end;
      try
         shell.shell_proof.edit_check_addr new_subdir;
         shell.shell_subdir <- new_subdir
      with
         exn ->
            if need_mount then begin
               if verbose then
                  eprintf "Warning: chdir partially successful%t" eflush;
               shell.shell_subdir <- []
            end;
            raise exn

(*
 * Default chdir does not force the remount.
 *)
let chdir parse_arg shell need_shell verbose path =
   chdir_full parse_arg shell false need_shell verbose path

(*
 * Refresh the current directory.
 *)
let refresh parse_arg shell =
   try
      chdir_full parse_arg shell true true true (shell.shell_fs, shell.shell_subdir)
   with _ -> begin
      try
         chdir_full parse_arg shell true true true (shell.shell_fs, [])
      with _ ->
         chdir_full parse_arg shell true true true (DirRoot, [])
   end

let pwd shell =
   string_of_dir (shell.shell_fs, shell.shell_subdir)

let cd parse_arg shell name =
   chdir parse_arg shell true true (parse_path shell name);
   pwd shell

let root parse_arg shell =
   chdir parse_arg shell true true (root_of_dir (shell.shell_fs, shell.shell_subdir));
   pwd shell

let unredo unredo_fun shell =
   touch shell;
   let dir = unredo_fun shell.shell_subdir in
      if dir <> shell.shell_subdir then
         begin
            shell.shell_subdir <- dir;
            eprintf "CWD now: %s%t" (pwd shell) eflush
         end;
      display_proof shell LsOptionSet.empty

let undo shell =
   unredo shell.shell_proof.edit_undo shell

let redo shell =
   unredo shell.shell_proof.edit_redo shell

let fs_pwd shell =
   match shell.shell_fs, shell.shell_subdir with
      DirFS, rest when rest <> [] ->
         String.concat "/" rest
    | _ ->
         "."

let relative_pwd shell =
   String.concat "/" shell.shell_subdir

(************************************************************************
 * Viewing.
 *)

(*
 * Window width.
 *)
let set_window_width shell i =
   shell.shell_df_method <- {shell.shell_df_method with df_width = max !Mp_term.min_screen_width i}

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
         let mk_opname = Package_info.mk_opname_kind pack NormalKind in
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
   display_proof shell options

(************************************************************************
 * Proof operations.
 *)

(*
 * Apply a function to all elements.
 *)
let rec apply_all parse_arg shell (f : item_fun) (time : bool) (clean_item : clean_item_fun) (clean_module : clean_module_fun) =
   let dir = shell.shell_fs, shell.shell_subdir in
   let apply_it item mod_name name =
      (*
       * Here we indeed want to ignore absolutely any kind of error.
       * Whatever went wrong, we just want to skip the bad item and continue to the next one.
       *)
      (try Shell_state.set_so_var_context (Some (item.edit_get_terms ())) with
          _ ->
             ());

      if
         try f item (get_db shell) with
            _ ->
               false
      then
         touch shell;

      clean_item mod_name name
   in
   let rec apply_all_exn time =
      let display_mode = get_display_mode shell in
         match shell.shell_fs, shell.shell_subdir, shell.shell_package with
            DirModule (_, mod_name), [], Some pack ->
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
                  else begin
                     List.iter apply_item items;
                     clean_module pack
                  end

          | DirModule _, _, _ ->
               raise (Invalid_argument "Shell_core.apply_all: internal error")

          | DirRoot, subdir, _ ->
               (match subdir with
                   [] ->
                      apply_all_packages_exn (all_theories ())
                 | [group] ->
                      apply_all_packages_exn (List.filter shell_package_name (snd (Package_info.group_packages packages group)))
                 | _ ->
                      raise (Invalid_argument "Shell_core.apply_all: internal error"))

          | DirFS, _, _ ->
               raise (Failure "Shell_core.apply_all: not applicable in a filesystem")

          | DirProof _, _, _ ->
               raise (Failure "Shell_core.apply_all: not applicable inside an individual proof")
   and apply_all_packages_exn names =
      let expand name =
         eprintf "Entering %s%t" name eflush;
         chdir parse_arg shell false true (module_dir name);
         apply_all_exn false
      in
         List.iter expand names
   in
      apply_all_exn time;
      chdir_full parse_arg shell true false false dir

let dont_clean_item mod_name name =
   ()

let dont_clean_module pack =
   ()

let clean_resources mod_name name =
   Mp_resource.clear_results (mod_name, name)

let clean_and_abandon pack =
   Package_info.abandon pack;
   Proof_boot.Proof.clear_cache ()

let expand_all parse_arg shell =
   let f item db =
      item.edit_interpret [] ProofExpand
   in
      apply_all parse_arg shell f true clean_resources dont_clean_module

(*
 * TeX functions.
 *)
let print_theory parse_arg shell name =
   let dfm = shell.shell_df_method in
   let dir = shell.shell_fs, shell.shell_subdir in
      shell.shell_df_method <- { dfm with df_type = DisplayTex; df_width = 60; df_mode = "tex" };
      chdir parse_arg shell false true (module_dir name);
      expand_all parse_arg shell;
      view parse_arg shell (LsOptionSet.singleton LsAll);
      shell.shell_df_method <- dfm;
      chdir parse_arg shell false false dir

let extract parse_arg shell path () =
   let dir = shell.shell_fs, shell.shell_subdir in
      try
         chdir parse_arg shell false false path;
         let modified, res = shell.shell_proof.edit_get_extract () in
            if modified then touch shell;
            chdir parse_arg shell false false dir;
            res
      with
         exn ->
            chdir parse_arg shell false false dir;
            raise exn

let term_of_extract shell terms =
   match shell with
      { shell_package = Some pack; shell_fs = DirProof (_, mod_name, name) } ->
         Refine.extract_term (Package_info.get_refiner pack) (make_opname [name; mod_name]) terms
    | _ ->
         raise (Failure "Shell.term_of_extract only works inside a proof")

let edit_find info i =
   match info.shell_fs with
      DirProof (_, modname, name) ->
         info.shell_subdir <- info.shell_proof.edit_find info.shell_subdir i;
         pwd info
    | _ ->
         raise (Invalid_argument "Shell.find_subgoal: not in a proof")

let edit_is_enabled shell name =
   shell.shell_proof.edit_is_enabled shell.shell_subdir name

(************************************************************************
 * MODULES                                                              *
 ************************************************************************)

(*
 * Make a new package.
 * Right now we only allow packages at the top level.
 *)
let create_pkg parse_arg shell name =
   match parse_path shell name with
      DirModule (_, modname), [] ->
         (* Top level *)
         let _ = Package_info.create_package packages parse_arg modname in
            view parse_arg shell LsOptionSet.empty
    | DirRoot, _ ->
         raise (Failure "Shell.create_package: can't create root package")
    | DirFS, _ ->
         raise (Failure "Shell.create_package: can't create a filesystem package")
    | DirProof _, _
    | DirModule _, _ :: _ ->
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

let abandon parse_arg shell =
   match shell.shell_package with
      Some pack ->
         eprintf "Abandoning %s@." (Package_info.name pack);
         Package_info.abandon pack;
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
      List.iter backup (modified_packages ())

let save_all parse_arg _ =
   let save pack =
      if Package_info.status pack = PackModified then
         begin
            eprintf "Saving %s@." (Package_info.name pack);
            Package_info.save parse_arg pack
         end
   in
      List.iter save (modified_packages ())

let export_all parse_arg _ =
   let save pack =
      if Package_info.status pack = PackModified then
         begin
            eprintf "Exporting %s@." (Package_info.name pack);
            Package_info.export parse_arg pack
         end
   in
      List.iter save (modified_packages ())

let revert_all parse_arg shell =
   let revert pack =
      if Package_info.status pack = PackModified then
         begin
            eprintf "Reverting %s@." (Package_info.name pack);
            Package_info.revert pack
         end
   in
      List.iter revert (modified_packages ());
      refresh parse_arg shell

(*
 * This performs a massive cleanup of the cached state.
 *)
let abandon_all parse_arg shell =
   let abandon pack =
      eprintf "Abandoning %s@." (Package_info.name pack);
      Package_info.abandon pack
   in
      Mp_resource.clear ();
      List.iter abandon (modified_packages ());
      Proof_boot.Proof.clear_cache ();
      Package_info.clear_cache packages;
      refresh parse_arg shell

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
