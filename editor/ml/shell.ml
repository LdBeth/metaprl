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

include Tacticals

include Proof_edit
include Package_info
include Package_df
include Shell_null
include Shell_rewrite
include Shell_rule

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

open Filter_summary

open Tacticals
open Mptop

open Proof_type
open Package_type
open Package_info
open Package_df
open Shell_type
open Shell_p4_type

(*
 * Show that the file is loading.
 *)
let _ =
   if !debug_load then
      eprintf "Loading Shell%t" eflush

let debug_refine = load_debug "refine"

let debug_shell =
   create_debug (**)
      { debug_name = "shell";
        debug_description = "Display shell operations";
        debug_value = false
      }

module Shell (ShellP4 : ShellP4Sig) =
struct
   (************************************************************************
    * TYPES                                                                *
    ************************************************************************)

   (*
    * Info need for this shell.
    *)
   type shell_info =
      { (* Display *)
         mutable width : int;
         mutable df_mode : string;

         (* Current module and path and proof *)
         mutable dir : string list;
         mutable package : Package.package option;
         mutable proof : edit_object;

         (* All loaded modules *)
         packages : Package.t
      }

   (************************************************************************
    * INITIAL BASE                                                         *
    ************************************************************************)

   (*
    * This is the info for this shell.
    *)
   let info =
      { width = 80;
        df_mode = "prl";
        dir = [];
        package = None;
        packages = Package.create ShellP4.parse_string ShellP4.eval_tactic (ShellP4.get_includes ());
        proof = Shell_null.null_object
      }

   (************************************************************************
    * UTILITIES                                                            *
    ************************************************************************)

   (*
    * Get the current "prl" printing base.
    *)
   let get_db dbase =
      let dbase' =
         match info.package with
            Some mod_info ->
               if !debug_shell then
                  eprintf "Selecting display forms from %s%t" (Package.name mod_info) eflush;
               Package.dforms mod_info
          | None ->
               if !debug_shell then
                  eprintf "Restoring default display forms%t" eflush;
               dbase
      in
         get_mode_base dbase' info.df_mode

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
    * Update the timestamp.
    *)
   let touch () =
      let pack = get_current_package info in
         try Package.touch pack with
            Failure "touch" ->
               eprintf "The module %s is read-only.  Use set_writeable () to change it.%t" (Package.name pack) eflush;
               raise (Failure "touch")

   (*
    * Change the status so that we can write to the file.
    *)
   let set_writeable () =
      let pack = get_current_package info in
         Package.set_status pack Modified

   (*
    * Term printer.
    *)
   let print_term dbase t =
      let db = get_db dbase in
      let buf = new_buffer () in
         Dform.format_term db buf t;
         Format.print_string (print_to_string info.width buf)

   let print_error_term ofile t =
      let db = get_db null_mode_base in
      let buf = new_buffer () in
         Dform.format_term db buf t;
         output_string ofile (print_to_string info.width buf)

   let print_exn f x =
      let db = get_db null_mode_base in
         Filter_exn.print db f x

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
   let parse_path name =
      let home = (match info.dir with [] -> [] | modname::_ -> [modname]) in
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
   let set_item modname name =
      let pack = Package.get info.packages modname in
      let item =
         try Package.find pack name with
            Not_found ->
               eprintf "Item '/%s/%s' not found%t" modname name eflush;
               raise Not_found
      in
      let item =
         match item with
            Rewrite rw ->
               Shell_rewrite.view_rw pack rw
          | CondRewrite crw ->
               Shell_rewrite.view_crw pack crw
          | Axiom ax ->
               Shell_rule.view_axiom pack ax
          | Rule rule ->
               Shell_rule.view_rule pack rule
          | Opname _ ->
               eprintf "Editing opname '/%s/%s' not supported%t" modname name eflush;
               raise (Failure "view")
          | MLTerm _ ->
               eprintf "Editing mlterm '/%s/%s' not implemented%t" modname name eflush;
               raise (Failure "view")
          | Condition _ ->
               eprintf "Editing condition '/%s/%s' not implemented%t" modname name eflush;
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
      in
         info.proof <- item

   (*
    * Display the current proof.
    *)
   let display_proof () =
      (* Get the package and item *)
      let mod_info = get_current_package info in
      let edit = info.proof in
      let dformer = Package.dforms mod_info in
      let modname = Package.name mod_info in
      let buf = new_buffer () in
      let db = get_mode_base dformer info.df_mode in
         edit.edit_format db buf;
         print_to_channel info.width buf stdout;
         flush stdout

   (*
    * Display the "root" directory.
    * This is just a list of the "important" packages.
    *)
   let view_packages () =
      let buf = new_buffer () in
         format_packages buf info.packages;
         format_newline buf;
         output_string stdout "\n--+--modules--+--\nModule listing dir: modules; path: /\n--\n";
         print_to_channel info.width buf stdout;
         output_string stdout "--+--modules--+--\n";
         flush stdout

   (*
    * Display a particular package.
    *)
   let view_package name =
      let pack = Package.get info.packages name in
      let buf = new_buffer () in
         format_implementation info.df_mode buf pack;
         printf "\n--+--theory--+--\nTheory listing for package: %s; path: /%s\n" name name;
         print_to_channel info.width buf stdout;
         output_string stdout "--+--theory--+--\n";
         flush stdout

   (*
    * View an item in a package.
    *)
   let view_item modname name =
      (* Get the package and item *)
      let _ = eprintf "View item /%s/%s%t" modname name eflush in
      let buf = new_buffer () in
      let mod_info = get_current_package info in
      let db = get_mode_base (Package.dforms mod_info) info.df_mode in
         info.proof.edit_format db buf;
         print_to_channel info.width buf stdout;
         flush stdout

   (*
    * General purpose displayer.
    *)
   let view name =
      let dir = parse_path name in
         match dir with
            [] ->
               view_packages ()
          | [modname] ->
               view_package modname
          | modname :: item :: _ ->
               view_item modname item

   let ls () =
      view "."

   (*
    * Window width.
    *)
   let set_window_width i =
      info.width <- if i < min_screen_width then min_screen_width else i

   (*
    * Show the directory.
    *)
   let pwd () =
      string_of_path info.dir

   (************************************************************************
    * MODULES                                                              *
    ************************************************************************)

   (*
    * Load a package if it is not already loaded.
    *)
   let load name =
      let load name =
         let _ =
            try
               let pack = Package.get info.packages name in
                  match Package.status pack with
                     Modified ->
                        raise (Failure (sprintf "Shell.load: package '%s' is modified" name))
                   | _ ->
                        ()
            with
               Not_found ->
                  ()
         in
            Package.load info.packages name;
            ()
      in
         print_exn load name

   (*
    * Make a new package.
    * Right now we only allow packages at the top level.
    *)
   let create_pkg name =
      let create name =
         match parse_path name with
            [modname] ->
               (* Top level *)
               Package.create_package info.packages modname;
               view name
          | [] ->
               raise (Failure "Shell.create_package: can't create root package")
          | _ ->
               raise (Failure "Shell.create_package: packages can't be nested right now")
      in
         print_exn create name

   (*
    * Save the current package.
    *)
   let save () =
      let save () =
         match info.package with
            Some pack ->
               Package.save info.packages pack
          | None ->
               ()
      in
         print_exn save ()

   let save_all () =
      let save_all () =
         let save pack =
            if Package.status pack = Modified then
               Package.save info.packages pack
         in
            List.iter save (Package.packages info.packages)
      in
         print_exn save_all ()

   (************************************************************************
    * OBJECTS                                                              *
    ************************************************************************)

   (*
    * Creation functions.
    *)
   let create_rw name =
      let create name =
         touch ();
         raise (RefineError ("Shell.create_rw", StringError "not implemented"))
      in
         print_exn create name

   let create_axiom name =
      let create name =
         touch ();
         raise (RefineError ("Shell.create_axiom", StringError "not implemented"))
      in
         print_exn create name

   let create_thm name =
      let create name =
         touch ();
         raise (RefineError ("Shell.create_thm", StringError "not implemented"))
      in
         print_exn create name

   let create_tptp name =
      let create name =
         let seq = Tptp_load.load name in
         let package = get_current_package info in
         let item = Shell_rule.create package name in
            item.edit_set_goal seq;
            item.edit_save ();
            touch ()
      in
         print_exn create name

   let create_opname name =
      let create name =
         touch ();
         raise (RefineError ("Shell.create_opname", StringError "not implemented"))
      in
         print_exn create name

   let create_condition name =
      let create name =
         touch ();
         raise (RefineError ("Shell.create_condition", StringError "not implemented"))
      in
         print_exn create name

   let create_parent name =
      let create name =
         touch ();
         raise (RefineError ("Shell.create_parent", StringError "not implemented"))
      in
         print_exn create name

   let create_dform name =
      let create name =
         touch ();
         raise (RefineError ("Shell.create_dform", StringError "not implemented"))
      in
         print_exn create name

   let create_prec name =
      let create name =
         touch ();
         raise (RefineError ("Shell.create_prec", StringError "not implemented"))
      in
         print_exn create name

   let create_prec_rel name =
      let create name =
         touch ();
         raise (RefineError ("Shell.create_prec_rel", StringError "not implemented"))
      in
         print_exn create name

   let create_resource name =
      let create name =
         touch ();
         raise (RefineError ("Shell.create_resources", StringError "not implemented"))
      in
         print_exn create name

   let create_infix name =
      let create name =
         touch ();
         raise (RefineError ("Shell.create_infix", StringError "not implemented"))
      in
         print_exn create name

   let create_ml name =
      let create name =
         touch ();
         raise (RefineError ("Shell.create_ml", StringError "not implemented"))
      in
         print_exn create name

   (*
    * Proof operations.
    *)
   let set_goal t =
      let set t =
         touch ();
         info.proof.edit_set_goal t;
         display_proof ()
      in
         print_exn set t

   let set_redex t =
      let set t =
         touch ();
         info.proof.edit_set_redex t;
         display_proof ()
      in
         print_exn set t

   let set_contractum t =
      let set t =
         touch ();
         info.proof.edit_set_contractum t;
         display_proof ()
      in
         print_exn set t

   let set_assumptions tl =
      let set t =
         touch ();
         info.proof.edit_set_assumptions tl;
         display_proof ()
      in
         print_exn set tl

   let set_params pl =
      let set t =
         touch ();
         info.proof.edit_set_params pl;
         display_proof ()
      in
         print_exn set pl

   let check () =
      let set () =
         info.proof.edit_check ();
         display_proof ()
      in
         print_exn set ()

   let expand () =
      let set () =
         let start = Unix.times () in
         let start_time = Unix.gettimeofday () in
         let _ = info.proof.edit_expand (get_db null_mode_base) in
         let finish = Unix.times () in
         let finish_time = Unix.gettimeofday () in
            display_proof ();
            eprintf "User time %f; System time %f; Real time %f%t" (**)
               ((finish.Unix.tms_utime +. finish.Unix.tms_cutime)
                -. (start.Unix.tms_utime +. start.Unix.tms_cstime))
               ((finish.Unix.tms_stime +. finish.Unix.tms_cstime)
                -. (start.Unix.tms_stime +. finish.Unix.tms_cstime))
               (finish_time -. start_time)
               eflush
      in
         print_exn set ()

   (*
    * Redefined below as a shortcut for cd
    * let root () =
    *    let set () =
    *       info.proof.edit_root ();
    *       display_proof ()
    *    in
    *       print_exn set ()
    * let up i =
    *    let set () =
    *       info.proof.edit_up i;
    *       display_proof ()
    *    in
    *       print_exn set ()
    * let down i =
    *    let set i =
    *       info.proof.edit_down i;
    *       display_proof ()
    *    in
    *       print_exn set i
    *)
   let refine tac =
      let set () =
         let str, ast = ShellP4.get_tactic () in
            touch ();
            if !debug_refine then
               eprintf "Starting refinement%t" eflush;
            info.proof.edit_refine str ast tac;
            if !debug_refine then
               eprintf "Displaying proof%t" eflush;
            if ShellP4.is_interactive () then
               display_proof ();
            if !debug_refine then
               eprintf "Proof displayed%t" eflush
      in
         print_exn set ()

   (*
    * Get the current goal.
    *)
   let goal () =
      let get () =
         touch ();
         info.proof.edit_goal ()
      in
         print_exn get ()

   let undo () =
      let set () =
         touch ();
         info.proof.edit_undo ();
         display_proof ()
      in
         print_exn set ()

   let fold () =
      let set () =
         touch ();
         info.proof.edit_fold ();
         display_proof ()
      in
         print_exn set ()

   let fold_all () =
      let set () =
         touch ();
         info.proof.edit_fold_all ();
         display_proof ()
      in
         print_exn set ()

   let kreitz () =
      let set () =
         touch ();
         info.proof.edit_kreitz ();
         display_proof ()
      in
         print_exn set ()

   (*
    * Change directory.
    *)
   let rec cd name =
      let dir = parse_path name in
         begin
            match dir with
               [] ->
                  (* go to toplevel *)
                  info.dir <- [];
                  info.package <- None;
                  ShellP4.set_df None;
                  ShellP4.set_mk_opname None;
             | modname :: item ->
                  (* change module only if in another (or at top) *)
                  if (info.dir = []) or ((List.hd info.dir) <> modname) then begin
                     let pkg = Package.get info.packages modname in
                        info.package <- Some pkg;
                        ShellP4.set_df (Some (get_db null_mode_base));
                        ShellP4.set_mk_opname (Some (Package.mk_opname pkg));
                        ShellP4.set_module modname commands;
                  end;
                  if (item = []) then begin
                     (* top of module *)
                     info.dir <- dir;
                     info.proof <- Shell_null.null_object
                  end else begin
                     (* select an item (if not there already), then go down the proof. *)
                   if ((info.dir = []) or ((List.tl info.dir) = []) or
                         ((List.hd (List.tl info.dir)) <> (List.hd item))) then
                        set_item modname (List.hd item);
                     (* go down the proof with pf_path *)
                     info.proof.edit_addr (List.map int_of_string (List.tl item));
                     info.dir <- dir
                  end;
         end;
         pwd ()

   and root () =
      let set () =
         cd (String.make ((List.length info.dir)-1) '.');
         display_proof ()
      in
         if (List.length info.dir) >= 2 then
            print_exn set ()

   and up i =
      let set () =
         cd (String.make (i+1) '.');
         display_proof ()
      in
         print_exn set ()

   and down i =
      let set i =
         cd (string_of_int i);
         display_proof ()
      in
         print_exn set i

   (*
    * Commands.
    *)
   and commands =
      ["cd",               StringFunExpr   (fun s  -> StringExpr (cd s));
       "pwd",              UnitFunExpr     (fun () -> StringExpr (pwd ()));
       "set_window_width", IntFunExpr      (fun i  -> UnitExpr (set_window_width i));
       "load",             StringFunExpr   (fun s  -> UnitExpr (load s));
       "create_pkg",       StringFunExpr   (fun s  -> UnitExpr (create_pkg s));
       "save",             UnitFunExpr     (fun () -> UnitExpr (save ()));
       "save_all",         UnitFunExpr     (fun () -> UnitExpr (save_all ()));
       "create_rw",        StringFunExpr   (fun s  -> UnitExpr (create_rw s));
       "create_axiom",     StringFunExpr   (fun s  -> UnitExpr (create_axiom s));
       "create_thm",       StringFunExpr   (fun s  -> UnitExpr (create_thm s));
       "create_tptp",      StringFunExpr   (fun s  -> UnitExpr (create_tptp s));
       "create_opname",    StringFunExpr   (fun s  -> UnitExpr (create_opname s));
       "create_condition", StringFunExpr   (fun s  -> UnitExpr (create_condition s));
       "create_parent",    StringFunExpr   (fun s  -> UnitExpr (create_parent s));
       "create_dform",     StringFunExpr   (fun s  -> UnitExpr (create_dform s));
       "create_prec",      StringFunExpr   (fun s  -> UnitExpr (create_prec s));
       "create_prec_rel", (**)
          StringFunExpr (fun s1 ->
                StringFunExpr (fun s2 ->
                      StringFunExpr (fun s3 ->
                            UnitExpr (create_prec_rel s1 s2 s3))));
       "create_resource",  StringFunExpr   (fun s  -> UnitExpr (create_resource s));
       "create_infix",     StringFunExpr   (fun s  -> UnitExpr (create_infix s));
       "create_ml",        StringFunExpr   (fun s  -> UnitExpr (create_ml s));
       "view",             StringFunExpr   (fun s  -> UnitExpr (view s));
       "ls",               UnitFunExpr     (fun () -> UnitExpr (ls ()));
       "set_goal",         TermFunExpr     (fun t  -> UnitExpr (set_goal t));
       "set_redex",        TermFunExpr     (fun t  -> UnitExpr (set_redex t));
       "set_contractum",   TermFunExpr     (fun t  -> UnitExpr (set_contractum t));
       "set_assumptions",  TermListFunExpr (fun tl -> UnitExpr (set_assumptions tl));
       "check",            UnitFunExpr     (fun () -> UnitExpr (check ()));
       "expand",           UnitFunExpr     (fun () -> UnitExpr (expand ()));
       "root",             UnitFunExpr     (fun () -> UnitExpr (root ()));
       "up",               IntFunExpr      (fun i  -> UnitExpr (up i));
       "down",             IntFunExpr      (fun i  -> UnitExpr (down i));
       "refine",           TacticFunExpr   (fun t  -> UnitExpr (refine t));
       "undo",             UnitFunExpr     (fun () -> UnitExpr (undo ()));
       "fold",             UnitFunExpr     (fun () -> UnitExpr (fold ()));
       "fold_all",         UnitFunExpr     (fun () -> UnitExpr (fold_all ()));
       "kreitz",           UnitFunExpr     (fun () -> UnitExpr (kreitz ()))]

   (************************************************************************
    * NUPRL5 INTERFACE                                                     *
    ************************************************************************)

   (*
    * Return the list of all module names.
    *)
   let edit_list_modules () =
      List.map Package.name (Package.packages info.packages)

   let edit_info name =
      try Package.info (Package.get info.packages name) with
         NotLoaded _ ->
            eprintf "Loading package %s%t" name eflush;
            Package.load info.packages name;
            Package.info (Package.get info.packages name)

   let edit_list_module name =
      let info = edit_info name in
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

   (*
    * Navigating the hierarchy.
    *)
   let edit_list_parents name =
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
         collect (info_items (edit_info name))

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

   let edit_list_dforms name =
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
         collect (info_items (edit_info name))

   let edit_list_precs name =
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
         collect (info_items (edit_info name))

   let edit_list_prec_rels name =
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
         collect (info_items (edit_info name))

   (*
    * Create a new thm.
    *)
   let edit_cd_thm mname name =
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
      let opens = collect (info_items (edit_info mname)) in
         cd ("/" ^ mname ^ "/" ^ name);
         ShellP4.eval_opens opens;
         ()

   let edit_create_thm mname name =
      edit_info mname;
      cd ("/" ^ mname);
      let create name =
         let package = get_current_package info in
         let item = Shell_rule.create package name in
            item.edit_save ();
            touch ()
      in
         print_exn create name;
         edit_cd_thm mname name

   (*
    * Return the current node.
    *)
   let edit_addr addr =
      let set addr =
         info.proof.edit_down addr
      in
         print_exn set addr

   let edit_node addr =
      let proof = info.proof in
         proof.edit_addr addr;
         let tac =
            match proof.edit_tactic () with
               Some (str, _, _) ->
                  Some str
             | None ->
                  None
         in
         let goal = Sequent.msequent (proof.edit_goal ()) in
         let children = List.map Sequent.msequent (proof.edit_children ()) in
         let extras = List.map Sequent.msequent (proof.edit_extras ()) in
            tac, goal, children, extras

   let edit_refine addr str =
      let proof = info.proof in
      let refine () =
         let expr = ShellP4.parse_string str in
         let tac = ShellP4.eval_tactic expr in
            proof.edit_addr addr;
            proof.edit_refine str expr tac;
            let goal = Sequent.msequent (proof.edit_goal ()) in
            let children = List.map Sequent.msequent (proof.edit_children ()) in
            let extras = List.map Sequent.msequent (proof.edit_extras ()) in
               goal, children, extras
      in
         print_exn refine ()

   let edit_undo () =
      info.proof.edit_undo ()

   (************************************************************************
    * INITIALIZATION                                                       *
    ************************************************************************)

   (*
    * Print out an initialization file, and parse it.
    *)
   let main () =
      ShellP4.set_module "Mptop" commands;
      ShellP4.main ()
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "editor.top"
 * End:
 * -*-
 *)
