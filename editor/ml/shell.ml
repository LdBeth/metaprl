(*
 * The shell provides a command interface to the editor.
 * The commands defined here included:
 *    1. Creation. navigation of modules
 *    2. Creation, inspection of items in modules.
 *)

include Tactic_type

include Proof_edit
include Package_info
include Package_df
include Shell_null
include Shell_rewrite

open Longident
open Parsetree

open Printf
open Debug

open Term
open Dform
open Dform_print
open Rformat
open Refine_sig

open Package_type
open Package_info
open Package_df
open Shell_type

(*
 * Show that the file is loading.
 *)
let _ =
   if !debug_load then
      eprintf "Loading Shell%t" eflush

(************************************************************************
 * TYPES                                                                *
 ************************************************************************)

(*
 * Info need for this shell.
 *)
type shellInfo =
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
 * ARGUMENT COLLECTION                                                  *
 ************************************************************************)

(*
 * -I <dir>
 *)
let includes = ref []
    
let add_include dir =
   includes := !includes @ [dir]

(*
 * Anonymous arguments are rejected.
 *)
let handle_anon_arg arg =
   raise (Failure ("illegal argument: " ^ arg))

(*
 * Argument specifications.
 *)
let spec =
   ["-I", Arg.String add_include, "add an directory to the path for include files"]

let _ =
   Arg.current := 0;
   Env_arg.parse spec handle_anon_arg "Nuprl-Light toploop" 

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
     packages = Package.create !includes;
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
         raise (RefineError (StringError "no current package"))

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
   let pack = Package.get info.packages name in
   let buf = new_buffer () in
   let mod_info = get_current_package info in
   let db = get_mode_base (Package.dforms mod_info) info.df_mode in
      raise (Failure "Shell.view_item: not implemented")

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

let ls () = view "."

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
         h ^ "/" ^ (aux t)
    | [] ->
         ""
   in
      aux info.dir

(*
 * Change directory.
 *)
let cd name =
   let dir = parse_path info.dir name in
      info.dir <- dir;
      begin
         match dir with
            modname::_ ->
               let pkg = Package.get info.packages modname in
                  info.package <- Some pkg;
                  Shell_p4.set_df (Some (get_db null_mode_base));
                  Shell_p4.set_mk_opname (Some (Package.mk_opname pkg))
          | [] ->
               info.package <- None;
               Shell_p4.set_df None;
               Shell_p4.set_mk_opname None;
      end;
      view ".";
      pwd ()

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
      raise (RefineError (StringError "not implemented"))
   in
      print_exn create name

let create_axiom name =
   let create name =
      raise (RefineError (StringError "not implemented"))
   in
      print_exn create name

let create_thm name =
   let create name =
      raise (RefineError (StringError "not implemented"))
   in
      print_exn create name

let create_opname name =
   let create name =
      raise (RefineError (StringError "not implemented"))
   in
      print_exn create name

let create_condition name =
   let create name =
      raise (RefineError (StringError "not implemented"))
   in
      print_exn create name

let create_parent name =
   let create name =
      raise (RefineError (StringError "not implemented"))
   in
      print_exn create name

let create_dform name =
   let create name =
      raise (RefineError (StringError "not implemented"))
   in
      print_exn create name

let create_prec name =
   let create name =
      raise (RefineError (StringError "not implemented"))
   in
      print_exn create name

let create_prec_rul name =
   let create name =
      raise (RefineError (StringError "not implemented"))
   in
      print_exn create name

let create_resource name =
   let create name =
      raise (RefineError (StringError "not implemented"))
   in
      print_exn create name

let create_infix name =
   let create name =
      raise (RefineError (StringError "not implemented"))
   in
      print_exn create name

let create_ml name =
   let create name =
      raise (RefineError (StringError "not implemented"))
   in
      print_exn create name

(*
 * Proof operations.
 *)
let set_goal t =
   let set t =
      info.proof.edit_set_goal t;
      display_proof ()
   in
      print_exn set t

let set_redex t =
   let set t =
      info.proof.edit_set_redex t;
      display_proof ()
   in
      print_exn set t

let set_contractum t =
   let set t =
      info.proof.edit_set_contractum t;
      display_proof ()
   in
      print_exn set t

let set_assumptions tl =
   let set t =
      info.proof.edit_set_assumptions tl;
      display_proof ()
   in
      print_exn set tl

let set_params pl =
   let set t =
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

let up () =
   let set () =
      info.proof.edit_up ();
      display_proof ()
   in
      print_exn set ()

let down i =
   let set i =
      info.proof.edit_down i;
      display_proof ()
   in
      print_exn set i

let refine str ast tac =
   let set (str, ast, tac) =
      info.proof.edit_refine str ast tac;
      display_proof ()
   in
      print_exn set (str, ast, tac)

let undo () =
   let set () =
      info.proof.edit_undo ();
      display_proof ()
   in
      print_exn set ()

let fold () =
   let set () =
      info.proof.edit_fold ();
      display_proof ()
   in
      print_exn set ()

let fold_all () =
   let set () =
      info.proof.edit_fold_all ();
      display_proof ()
   in
      print_exn set ()

(************************************************************************
 * INITIALIZATION                                                       *
 ************************************************************************)

(*
 * Print out an initialization file, and parse it.
 *)
let init () =
   let init () =
      let nllib =
         try Sys.getenv "NLLIB" with
            Not_found ->
               "/usr/local/lib/nuprl-light"
      in
      let eval_include inc =
         Toploop.execute_phrase false (Ptop_dir ("directory", Pdir_string inc));
         ()
      in
         eval_include nllib;
         List.iter eval_include !includes;
         Toploop.execute_phrase false (Ptop_dir ("install_printer", Pdir_ident (Ldot (Lident "Shell_p4", "print_term"))));
         Toploop.execute_phrase false (Ptop_def [{ pstr_desc = Pstr_open (Lident "Shell");
                                                   pstr_loc = Location.none
                                                 }]);
         ()
   in
      Printexc.catch init ()

(*
 *
 * $Log$
 * Revision 1.8  1998/05/04 23:46:06  jyh
 * Most display forms now work.
 *
 * Revision 1.7  1998/04/28 18:29:52  jyh
 * ls() works, adding display.
 *
 * Revision 1.6  1998/04/24 19:38:07  jyh
 * Updated debugging.
 *
 * Revision 1.5  1998/04/24 02:41:34  jyh
 * Added more extensive debugging capabilities.
 *
 * Revision 1.4  1998/04/23 20:04:02  jyh
 * Initial rebuilt editor.
 *
 * Revision 1.3  1998/04/17 02:25:32  jyh
 * Implementing shell.
 *
 * Revision 1.2  1998/04/17 01:30:49  jyh
 * Editor is almost constructed.
 *
 * Revision 1.1  1997/08/06 16:17:25  jyh
 * This is an ocaml version with subtyping, type inference,
 * d and eqcd tactics.  It is a basic system, but not debugged.
 *
 * Revision 1.5  1996/10/23 15:17:51  jyh
 * First working version of dT tactic.
 *
 * Revision 1.4  1996/09/02 19:33:40  jyh
 * Semi-working package management.
 *
 * Revision 1.3  1996/05/21 02:25:45  jyh
 * This is a semi-working version before Wisconsin vacation.
 *
 * Revision 1.2  1996/05/20 17:00:12  jyh
 * This is an intermediate form of the editor with modules
 * before debugging.  Will be removing theoryGraph files next.
 *
 * Revision 1.1  1996/05/01 15:04:29  jyh
 * This is the initial checkin of the NuprlLight editor.  This editor provides
 * an emacs interface, a library navigator, and a proof editor.
 *
 * -*-
 * Local Variables:
 * Caml-master: "editor.top"
 * End:
 * -*-
 *)
