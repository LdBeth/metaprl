(*
 * The shell provides a command interface to the editor.
 * The commands defined here included:
 *    1. Creation. navigation of modules
 *    2. Creation, inspection of items in modules.
 *)

include Tacticals

include Proof_edit
include Package_info
include Package_df
include Shell_null
include Shell_rewrite
include Shell_rule

open Printf
open Nl_debug

open Refiner.Refiner.Term
open Refiner.Refiner.RefineError
open Dform
open Dform_print
open Rformat

open Filter_summary

open Tacticals
open Nltop

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
        packages = Package.create ShellP4.eval_tactic (ShellP4.get_includes ());
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
               eprintf "Selecting display forms from %s%t" (Package.name mod_info) eflush;
               Package.dforms mod_info
          | None ->
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
    * Turn a string into a path.
    *)
   let parse_path dir name =
      if name = "." then
         dir
      else if name = ".." then
         try fst (List_util.split_last dir) with
            Failure "split_last" ->
               []
      else if String.length name <> 0 & name.[0] = '.' then
         String_util.split '.' name
      else
         dir @ String_util.split '.' name

   let rec string_of_path = function
      [h] ->
         h
    | h::t ->
         h ^ "." ^ string_of_path t
    | [] ->
         ""

   (*
    * Update the current item being edited.
    *)
   let set_item modname name =
      let _ = eprintf "View item %s.%s%t" modname name eflush in
      let pack = Package.get info.packages modname in
      let item =
         try Package.find pack name with
            Not_found ->
               eprintf "Item '%s.%s' not found%t" modname name eflush;
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
               eprintf "Editing opname '%s.%s' not supported%t" modname name eflush;
               raise (Failure "view")
          | MLTerm _ ->
               eprintf "Editing mlterm '%s.%s' not implemented%t" modname name eflush;
               raise (Failure "view")
          | Condition _ ->
               eprintf "Editing condition '%s.%s' not implemented%t" modname name eflush;
               raise (Failure "view")
          | Parent _ ->
               eprintf "Editing parent '%s.%s' not supported%t" modname name eflush;
               raise (Failure "view")
          | Module _ ->
               eprintf "Editing module '%s.%s' not implemented%t" modname name eflush;
               raise (Failure "view")
          | DForm _ ->
               eprintf "Editing display form '%s.%s' not implemented%t" modname name eflush;
               raise (Failure "view")
          | Prec _ ->
               eprintf "Editing precedence '%s.%s' not supported%t" modname name eflush;
               raise (Failure "view")
          | PrecRel _ ->
               eprintf "Editing precedence relation '%s.%s' not supported%t" modname name eflush;
               raise (Failure "view")
          | Id _ ->
               eprintf "Editing magic number '%s.%s' not supported%t" modname name eflush;
               raise (Failure "view")
          | Resource _ ->
               eprintf "Editing resource '%s.%s' not supported%t" modname name eflush;
               raise (Failure "view")
          | Infix _ ->
               eprintf "Editing infix '%s.%s' not supported%t" modname name eflush;
               raise (Failure "view")
          | SummaryItem _
          | ToploopItem _ ->
               eprintf "Editing summary item '%s.%s' not implemented%t" modname name eflush;
               raise (Failure "view")
          | MagicBlock _ ->
               eprintf "Editing magic block '%s.%s' not implemented%t" modname name eflush;
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
      let _ = eprintf "View item %s.%s%t" modname name eflush in
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
      let dir = parse_path info.dir name in
         match dir with
            [] ->
               view_packages ()
          | [modname] ->
               view_package modname
          | [modname; item] ->
               view_item modname item
          | modname :: item1 :: item2 :: _ ->
               raise (Failure ("Shell.view: illegal module name " ^ string_of_path dir))

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
      let rec aux = function
         [h] ->
            h
       | h::t ->
            h ^ "." ^ (aux t)
       | [] ->
            ""
      in
         aux info.dir

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
         match parse_path info.dir name with
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
         info.proof.edit_expand (get_db null_mode_base);
         display_proof ()
      in
         print_exn set ()

   let root () =
      let set () =
         info.proof.edit_root ();
         display_proof ()
      in
         print_exn set ()

   let up i =
      let set () =
         info.proof.edit_up i;
         display_proof ()
      in
         print_exn set ()

   let down i =
      let set i =
         info.proof.edit_down i;
         display_proof ()
      in
         print_exn set i

   let refine tac =
      let set () =
         let str, ast = ShellP4.get_tactic () in
            touch ();
            if !debug_refine then
               eprintf "Starting refinement%t" eflush;
            info.proof.edit_refine str ast tac;
            if !debug_refine then
               eprintf "Displaying proof%t" eflush;
            display_proof ();
            if !debug_refine then
               eprintf "Proof displayed%t" eflush
      in
         print_exn set ()

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

   (*
    * Change directory.
    *)
   let rec cd name =
      let dir = parse_path info.dir name in
         info.dir <- dir;
         begin
            match dir with
               modname :: name ->
                  let pkg = Package.get info.packages modname in
                     info.package <- Some pkg;
                     ShellP4.set_df (Some (get_db null_mode_base));
                     ShellP4.set_mk_opname (Some (Package.mk_opname pkg));
                     ShellP4.set_module modname commands;
                     begin
                        match name with
                           [name] ->
                              set_item modname name
                         | [] ->
                              info.proof <- Shell_null.null_object
                         | _ ->
                              eprintf "Recursive modules not implemented%t" eflush;
                              raise (Failure "cd")
                     end
             | [] ->
                  info.package <- None;
                  ShellP4.set_df None;
                  ShellP4.set_mk_opname None;
         end;
         view ".";
         pwd ()

   (*
    * Commands.
    *)
   and commands =
      ["cd", StringFunExpr (fun s -> StringExpr (cd s));
       "pwd", UnitFunExpr (fun () -> StringExpr (pwd ()));
       "set_window_width", IntFunExpr (fun i -> UnitExpr (set_window_width i));
       "load", StringFunExpr (fun s -> UnitExpr (load s));
       "create_pkg", StringFunExpr (fun s -> UnitExpr (create_pkg s));
       "save", UnitFunExpr (fun () -> UnitExpr (save ()));
       "save_all", UnitFunExpr (fun () -> UnitExpr (save_all ()));
       "create_rw", StringFunExpr (fun s -> UnitExpr (create_rw s));
       "create_axiom", StringFunExpr (fun s -> UnitExpr (create_axiom s));
       "create_thm", StringFunExpr (fun s -> UnitExpr (create_thm s));
       "create_opname", StringFunExpr (fun s -> UnitExpr (create_opname s));
       "create_condition", StringFunExpr (fun s -> UnitExpr (create_condition s));
       "create_parent", StringFunExpr (fun s -> UnitExpr (create_parent s));
       "create_dform", StringFunExpr (fun s -> UnitExpr (create_dform s));
       "create_prec", StringFunExpr (fun s -> UnitExpr (create_prec s));
       "create_prec_rel", (**)
          StringFunExpr (fun s1 ->
                StringFunExpr (fun s2 ->
                      StringFunExpr (fun s3 ->
                            UnitExpr (create_prec_rel s1 s2 s3))));
       "create_resource", StringFunExpr (fun s -> UnitExpr (create_resource s));
       "create_infix", StringFunExpr (fun s -> UnitExpr (create_infix s));
       "create_ml", StringFunExpr (fun s -> UnitExpr (create_ml s));
       "view", StringFunExpr (fun s -> UnitExpr (view s));
       "ls", UnitFunExpr (fun () -> UnitExpr (ls ()));
       "set_goal", TermFunExpr (fun t -> UnitExpr (set_goal t));
       "set_redex", TermFunExpr (fun t -> UnitExpr (set_redex t));
       "set_contractum", TermFunExpr (fun t -> UnitExpr (set_contractum t));
       "set_assumptions", TermListFunExpr (fun tl -> UnitExpr (set_assumptions tl));
       "check", UnitFunExpr (fun () -> UnitExpr (check ()));
       "expand", UnitFunExpr (fun () -> UnitExpr (expand ()));
       "root", UnitFunExpr (fun () -> UnitExpr (root ()));
       "up", IntFunExpr (fun i -> UnitExpr (up i));
       "down", IntFunExpr (fun i -> UnitExpr (down i));
       "refine", TacticFunExpr (fun t -> UnitExpr (refine t));
       "undo", UnitFunExpr (fun () -> UnitExpr (undo ()));
       "fold", UnitFunExpr (fun () -> UnitExpr (fold ()));
       "fold_all", UnitFunExpr (fun () -> UnitExpr (fold_all ()));

       (* This is for debugging *)
       "test", UnitFunExpr (fun () -> UnitExpr (Itt_test.test ()))]

   (************************************************************************
    * INITIALIZATION                                                       *
    ************************************************************************)

   (*
    * Print out an initialization file, and parse it.
    *)
   let main () =
      ShellP4.set_module "Nltop" commands;
      ShellP4.main ()
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "editor.top"
 * End:
 * -*-
 *)
