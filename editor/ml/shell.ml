(*
 * The shell provides a command interface to the editor.
 * The commands defined here included:
 *    1. Creation. navigation of modules
 *    2. Creation, inspection of items in modules.
 *
 * $Log$
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
 *)

open Util;;
open Printf;;
open Rformat;;
open Term;;
open TermParse;;
open MlPrint;;
open EditUtil;;
open FileUtil;;
open Theory;;
open PackageInfo;;
open PackageUtil;;
open PackageGraph;;
open PackageIO;;
open DisplayPackageGraph;;
open DisplayPackage;;
open DisplayTheoryItem;;
open Proof;;
open ProofStep;;
open ProofEdit;;
open Tacticals;;

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
      mutable cmod : packageInfo option;
      mutable proof : (string * ped) option;
   
      (* All loaded modules *)
      mutable packages : packageInfo list
   };;

(************************************************************************
 * INITIAL BASE                                                         *
 ************************************************************************)

(*
 * These functions obtain the package listing of "important" theories.
 * An "important" (or "big") theory is any theory that has an element other than
 * just a label.
 *)
let big_theory_p { thy_name = name; thy_refiner = refiner; thy_dformer = base } =
   let is_big_refiner' =
      if is_null_refiner refiner then
         false
      else
         let item, refiner' = dest_refiner refiner in
            if is_null_refiner refiner' then
               match item with
                  RILabel _ -> false
                | _ -> true
            else
               true
   in
   let is_big_dformer' = not (is_null_mode_base base) in
      is_big_refiner' or is_big_dformer';;

(*
 * This is the info for this shell.
 *)
let info =
   let big_theories = filter big_theory_p (Theory.get_theories ()) in
      { width = 80;
        df_mode = "prl";
        dir = [];
        cmod = None;
        packages = map new_package big_theories;
        proof = None
      };;

(************************************************************************
 * UTILITIES                                                            *
 ************************************************************************)

(*
 * Get the current "prl" printing base.
 *)
let get_db dbase =
   let dbase' =
      match info.cmod with
         Some mod_info ->
            package_dforms mod_info
       | None ->
            dbase
   in
      get_mode_base dbase' info.df_mode;;

(*
 * Term printer.
 *)
let print_term dbase t =
   let db = get_db dbase in
   let buf = new_buffer () in
      Dform.format_term db buf t;
      Format.print_string (print_to_string info.width buf);;

let print_error_term ofile t =
   let db = get_db null_mode_base in
   let buf = new_buffer () in
      Dform.format_term db buf t;
      output_string ofile (print_to_string info.width buf);;

(*
 * Find an item by name within a refiner.
 *)
let find_refiner_item name pack =
   let refiner = package_refiner pack in
   let rec aux refiner =
      if is_null_refiner refiner then
         raise Not_found
      else
         let item, refiner' = dest_refiner refiner in
         let n = unknown_item_name item in
            if n = name then
               item
            else
               aux refiner'
   in
      aux refiner;;

(*
 * Find an element of a module.
 *)
let find_package_thm name pack =
   let rec aux = function
      { thm_name = n; thm_ped = ped }::t ->
         if name = n then
            ped
         else
            aux t
    | [] ->
         raise Not_found
   in
      aux (package_thms pack);;

(*
 * Find a theory in a list of packages.
 *)
let find_package name =
   let rec aux = function
      mod_info::tl ->
         if package_name mod_info = name then
            mod_info
         else
            aux tl
    | [] ->
        raise (NoPackage [name])
   in
      aux;;

(*
 * Get the current package.  Raise an exception if
 * there isn't one.
 *)
let get_current_package = function
   { cmod = Some mod_info } -> mod_info
 | _ -> raise (Invalid_argument "get_current_package");;

(*
 * Get the proof.
 *)
let get_current_proof = function
   { proof = Some (name, ped) } -> name, ped
 | _ -> raise (Invalid_argument "get_current_proof");;

(************************************************************************
 * VIEWING                                                              *
 ************************************************************************)

(*
 * Display the current proof.
 *)
let display_proof () =
   (* Get the package and item *)
   let mod_info = get_current_package info in
   let name, ped = get_current_proof info in
   let dformer = package_dforms mod_info in
   let modname = package_name mod_info in
   let buf = new_buffer () in
   let db = get_mode_base dformer info.df_mode in
      display_ped db buf ped;
      printf "\n--+--proof--+--\nTheory item: %s; path: /%s/%s\n--\n"
         name modname name;
      print_to_channel info.width buf stdout;
      output_string stdout "\n--+--proof--+--\n";
      flush stdout;;

(*
 * When a package item is displayed, we may want to record some information.
 *)
let display_package_thm = function
   { thm_name = name; thm_ped = ped } ->
      info.proof <- Some (name, ped);
      display_proof ();;

(*
 * Display the "root" directory.
 * This is just a list of the "important" packages.
 *)
let view_packages () =
   let roots = compute_roots info.packages in
   let buf = new_buffer () in
      display_package_graph buf roots;
      format_newline buf;
      output_string stdout "\n--+--modules--+--\nModule listing dir: modules; path: /\n--\n";
      print_to_channel info.width buf stdout;
      output_string stdout "--+--modules--+--\n";
      flush stdout;;

(*
 * Display a particular package.
 *)
let view_package name =
   let thy = find_package name info.packages in
   let buf = new_buffer () in
      display_package info.df_mode buf thy;
      fprintf stdout "\n--+--theory--+--\nTheory listing for package: %s; path: /%s\n"
         name name;
      print_to_channel info.width buf stdout;
      output_string stdout "--+--theory--+--\n";
      flush stdout;;

(*
 * View an item in a package.
 *)
let view_item modname name =
   (* Get the package and item *)
   let mod_info = find_package modname info.packages in
   let buf = new_buffer () in
   let db = get_mode_base (package_dforms mod_info) info.df_mode in
      begin
         try
            let item = find_package_thm name mod_info in
               info.cmod <- Some mod_info;
               display_package_thm { thm_name = name; thm_ped = item }
         with Not_found ->
               try
                  let item = find_refiner_item name mod_info in
                     display_theory_item db buf item;
                     printf "\n--+--item--+--\nTheory item: %s; path: /%s/%s\n"
                     name modname name;
                     print_to_channel info.width buf stdout;
                     output_string stdout "\n--+--item--+--\n"
               with
                  Not_found ->
                     fprintf stderr "No such item: %s\n" name
      end;
      flush stdout;;

(*
 * General purpose displayer.
 *)
let view name =
   let dir = parse_path info.dir name in
      match dir with
         [] -> view_packages ()
       | [modname] -> view_package modname
       | [modname; item] ->
            view_item modname item
       | modname::item1::item2::_ ->
            raise (NoPackage [modname; item1; item2]);;

(*
 * Window width.
 *)
let set_window_width i =
   info.width <- if i < 40 then 40 else i;;

(*
 * Show the directory.
 *)
let pwd () =
   let rec aux = function
      [h] -> h
    | h::t -> h ^ "/" ^ (aux t)
    | [] -> ""
   in
      aux info.dir;;

(*
 * Change directory.
 *)
let cd name =
   let dir = parse_path info.dir name in
      info.dir <- dir;
      begin
         match dir with
            modname::_ ->
               info.cmod <- Some (find_package modname info.packages)
          | [] ->
               info.cmod <- None
      end;
      view ".";
      pwd ();;

(************************************************************************
 * MODULES                                                              *
 ************************************************************************)

(*
 * Make a new package.
 * Right now we only allow packages at the top level.
 *)
let create_package name =
   match parse_path info.dir name with
      [modname] ->
         (* Top level *)
         let mod_info = new_empty_package modname in
            info.packages <- mod_info :: info.packages;
            view name
    | [] ->
         raise (BadCommand "can't create root package")
    | _ ->
         raise (BadCommand "packages can't be nested right now");;

(*
 * Install a package with the given name.
 *)
let install_package mod_info =
   (* Maybe have to replace an old one *)
   let name = package_name mod_info in
   let rec replace_package = function
      h::t when (package_name h) = name ->
         mod_info :: t
    | h::t ->
         h :: (replace_package t)
    | [] ->
         [mod_info]
   in
      info.packages <- replace_package info.packages;;

(*
 * Save the current package.
 *)
let save () =
   match info.cmod with
      Some mod_info -> PackageIO.save mod_info
    | None -> ();;

let save_as name =
   match info.cmod with
      Some mod_info -> PackageIO.save_as mod_info name
    | None ->
         raise (BadCommand "No current package");;

let save_all () =
   do_list PackageIO.save info.packages;;
         
(************************************************************************
 * OBJECTS                                                              *
 ************************************************************************)

(*
 * Add a parent theory.
 * Eventually we will have to open up the filterPackage package,
 * and expose the interface definition, because we need it 
 * for describing opnames among other things.  For now, we just
 * inherit the refiner and display forms.
 *)
let add_parent name =
   let mod_info = find_package name info.packages in
   let cmod = get_current_package info in
      package_add_parent cmod mod_info;;

(*
 * Start a new proof.
 *)
let create_thm name term =
   let mod_info = get_current_package info in
   let ped = new_ped (new_proof term) in
      package_add_thm mod_info name ped;
      view name;;

(*
 * Navigate the proof.
 *)
let move_up () =
   let _, ped = get_current_proof info in
      up_ped ped;
      display_proof ();;

(*
 * Navigate the proof.
 *)
let move_root () =
   let _, ped = get_current_proof info in
      root_ped ped;
      display_proof ();;

let move_down i =
   let _, ped = get_current_proof info in
      down_ped ped i;
      display_proof ();;

let refine str tac =
   let _, ped = get_current_proof info in
      refine_ped ped str tac;
      display_proof ();;

let undo () =
   let _, ped = get_current_proof info in
      undo_ped ped;
      display_proof ();;

(************************************************************************
 * DEBUGGING                                                            *
 ************************************************************************)

let pf = ref (| 'a |);;

let z p =
   pf := fst p;
   idT p;;

(*
 * -*-
 * Local Variables:
 * Caml-master: "editor.top"
 * End:
 * -*-
 *)
