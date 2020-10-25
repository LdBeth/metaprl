(*
 * Dependency analysis customized for scanning MetaPRL files.
 *
 * Based on original code by Xavier Leroy, project Cristal,
 * INRIA Rocquencourt, (c) 1996.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 1998-2006 Mojave Group, Caltech
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
 * Author: Jason Hickey @email{jyh@cs.caltech.edu}
 * Modified by: Aleksey Nogin @email{nogin@cs.caltech.edu}
 * @end[license]
 *)
{
module StringCompare =
struct
   type t = string
   let compare = Stdlib.compare
end;;

module StringSet = Set.Make (StringCompare);;
module StringTable = Map.Make (StringCompare);;

type summary =
   { summ_basename        : string;
     summ_is_impl         : bool;
     summ_free_structures : StringSet.t;
     summ_prl_structures  : StringSet.t;
     summ_includes        : StringSet.t;
     summ_contains_topval : bool;
     summ_contains_rules  : bool
   }

let create_summary name is_impl =
   { summ_basename        = name;
     summ_is_impl         = is_impl;
     summ_free_structures = StringSet.empty;
     summ_prl_structures  = StringSet.empty;
     summ_includes        = StringSet.empty;
     summ_contains_topval = false;
     summ_contains_rules  = false
   }

let add_structure summ name =
   { summ with summ_free_structures = StringSet.add name summ.summ_free_structures }

let add_prl_structure summ name =
   { summ with summ_prl_structures = StringSet.add name summ.summ_prl_structures }

let add_include summ name =
   { summ with summ_includes = StringSet.add name summ.summ_includes }

let add_topval summ =
   { summ with summ_contains_topval = true }

let add_rule summ =
   { summ with summ_contains_rules = true }

(*
 * These are the implicit names.
 *)
let prl_cmi_names =
   ["Tactic_type";
    "Precedence";
    "Refiner";
    "Mp_resource"]

let prl_names =
   ["Obj";
    "Lm_debug";
    "Filter_exn";
    "Theory";
    "Dform";
    "Tactic_boot_sig";
    "Ml_term"] @ prl_cmi_names

(*
 * These are the names used by "topval" declarations
 *)
let topval_names =
   ["Mptop";
    "Shell_sig"]

(*
 * These are the names used by rule/rewrite implementation
 *)
let rule_names =
   ["Shell_command";
    "Lm_symbol";
    "Rewriter_sig";
    "Perv";
    "Top_resource"]

(*
 * Names of the reflected theories.
 *)
let reflect_names =
   ["Itt_hoas_theory"]

let reflect_modules =
   ["Basic_tactics"]

let reflect_prefix = "reflect_"
let reflect_length = String.length reflect_prefix

let rec compare_reflect i name =
   i = reflect_length || (name.[i] = reflect_prefix.[i] && compare_reflect (succ i) name)

let is_reflect_prefix name =
   String.length name > reflect_length && compare_reflect 0 name

let chop_reflect_prefix name =
   String.sub name reflect_length (String.length name - reflect_length)

let reflect_module_name name =
   String.capitalize_ascii (reflect_prefix ^ String.uncapitalize_ascii name)

(*
 * Actual dependency table.
 *)
let add_dependency table name1 name2 =
   if StringTable.mem name1 table then
      let s = StringTable.find name1 table in
      let s = StringSet.add name2 s in
         StringTable.add name1 s table
   else
      StringTable.add name1 (StringSet.singleton name2) table
}

let white = [' ' '\010' '\013' '\009' '\012']
let modname = ['A'-'Z' '\192'-'\214' '\216'-'\222' ]
              (['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255'
                '\'' '0'-'9' ]) *
let quotestart = '<' ':' (['a'-'z'])+ '<'
let filename = (['A'-'Z' 'a'-'z' '_' '-' '.' '0'-'9' '/'])*

rule main summ = parse
    "open" white+
  | "include" white+
  | "module" white+ modname white+ '=' white+
      { let summ = struct_name summ lexbuf in
           main summ lexbuf
      }
  | "derive" white+
  | "extends" white+
      { let summ = prl_struct_name summ lexbuf in
           main summ lexbuf
      }
  | "topval" white+
      { main (add_topval summ) lexbuf }
  | white "prim" white+
  | white "prim_rw" white+
  | white "interactive" white+
  | white "interactive_rw" white+
  | white "derived" white+
      { main (add_topval (add_rule summ)) lexbuf }
  | "INCLUDE" white+
      { let summ = include_name summ lexbuf in
           main summ lexbuf
      }
  | modname '.'
      { let s = Lexing.lexeme lexbuf in
        let summ = add_structure summ (String.sub s 0 (String.length s - 1)) in
           main summ lexbuf
      }
  | "\""
      { string lexbuf;
        main summ lexbuf
      }
  | "(*" | "<<" | quotestart
      { comment lexbuf;
        main summ lexbuf
      }
  | "'" [^ '\\'] "'"
  | "'" '\\' ['\\' '\'' 'n' 't' 'b' 'r'] "'"
  | "'" '\\' ['0'-'9'] ['0'-'9'] ['0'-'9'] "'"
    { main summ lexbuf }

  | eof
      { summ }
  | _
      { main summ lexbuf }

and struct_name summ = parse
    modname
      { add_structure summ (Lexing.lexeme lexbuf) }
  | ""
      { summ }

and prl_struct_name summ = parse
    modname
      { add_prl_structure summ (Lexing.lexeme lexbuf) }
  | ""
      { summ }

and include_name summ = parse
    '"' filename '"'
      { let s = Lexing.lexeme lexbuf in
           add_include summ (String.sub s 1 (String.length s - 2))
      }
  | ""
      { summ }

and comment = parse
    "(*" | "<<" | quotestart
      { comment lexbuf; comment lexbuf }
  | "*)" | ">>"
      { () }
  | "\""
      { string lexbuf; comment lexbuf }
  | "''"
  | "'" [^ '\\' '\''] "'"
  | "'\\" ['\\' '\'' 'n' 't' 'b' 'r'] "'"
  | "'\\" ['0'-'9'] ['0'-'9'] ['0'-'9'] "'"
      { comment lexbuf }
  | eof
      { () }
  | _
      { comment lexbuf }

and string = parse
    '"'
      { () }
  | '\\' ("\010" | "\013" | "\010\013") [' ' '\009'] *
  | '\\' ['\\' '"' 'n' 't' 'b' 'r']
  | '\\' ['0'-'9'] ['0'-'9'] ['0'-'9']
      { string lexbuf }
  | eof
      { () }
  | _
      { string lexbuf }

{

let () = ();;

(*
 * State of the processor.
 *)
type options =
   { mutable opt_path           : string list;
     mutable opt_prl_flag       : bool;
     mutable opt_omake_flag     : bool;
     mutable opt_ml_topval_flag : bool;
     mutable opt_modules_flag   : bool;
     mutable opt_reflect_flag   : bool
   }

let options =
   { opt_path           = [""];
     opt_prl_flag       = false;
     opt_omake_flag     = false;
     opt_ml_topval_flag = true;
     opt_modules_flag   = false;
     opt_reflect_flag   = false
   }

(************************************************************************
 * Printing.
 *)
let depends_on, escaped_eol =
  match Sys.os_type with
     "Unix"
   | "Win32"
   | "Cygwin" ->
        ": ", "\\\n    "
   | "MacOS" ->
        "\196 ", "\182\n    "
   | x ->
        invalid_arg ("ocamldep: unknown system type: " ^ x)

let print_file s =
   for i = 0 to String.length s - 1 do
      match s.[i] with
         ' '  -> print_string "\\ "
       | '\t' -> print_string "\\t"
       | '\n' -> print_string "\\n"
       | '\r' -> print_string "\\r"
       | '\\' -> print_string "\\\\"
       | c    -> print_char c
   done

(*
 * Print the items.  Try to format them nicely so that
 * we don't overlow the line length.
 *)
let rec print_items pos = function
   [] ->
      print_string "\n"
 | dep :: rem ->
      let dep_len = String.length dep in
      let pos =
         if pos + dep_len <= 77 then
            pos + dep_len + 1
         else begin
            print_string escaped_eol;
            dep_len + 5
         end
      in
         print_file dep;
         print_string " ";
         print_items pos rem

let print_target target_file deps =
   match deps with
      [] ->
         ()
    | _ ->
         print_file target_file;
         print_string depends_on;
         print_items (String.length target_file + 2) deps

(*
 * Print the entire dependency table.
 *)
let print_dependencies table =
   StringTable.iter (fun dst deps ->
         let deps = StringSet.fold (fun name deps -> name :: deps) deps [] in
            print_target dst deps) table

(************************************************************************
 * Find files in the search path.
 *)

(*
 * Find the file in the search path given a set of extensions.
 * For reflected theories, allow searching on the non-reflected name.
 *)
let rec find_file_by_path stale_found name stale_ext ext = function
   dir :: path ->
      let fullname = Filename.concat dir name ^ ext in
         if Sys.file_exists fullname then begin
            match stale_found, stale_ext with
               Some dir', Some ext' ->
                  Format.eprintf "@[<hv3>@[<hv3>Ocamldep Error@ while running in@ %s:@]@ source file@ %s@ is shadowed by the (stale?) binary file@ %s@]@." (Unix.getcwd ()) fullname (Filename.concat dir' name ^ ext');
                  exit 3
             | Some dir', None ->
                  raise (Invalid_argument "internal error")
             | None, _ ->
                  Some dir
         end else
            let stale_found =
               match stale_found, stale_ext with
                  Some _, _ -> stale_found
                | None, Some ext when Sys.file_exists (Filename.concat dir name ^ ext) -> Some dir
                | _ -> None
            in
               find_file_by_path stale_found name stale_ext ext path
 | [] ->
      None

let rec find_file_by_ext name stale_ext = function
   ext :: exts ->
      let dir = find_file_by_path None name stale_ext ext options.opt_path in
         (match dir with
             Some _ ->
                dir
           | None ->
                find_file_by_ext name stale_ext exts)
 | [] ->
      None

let rec find_file_by_name names stale_ext exts =
   match names with
      (name, realname) :: names ->
         (match find_file_by_ext name stale_ext exts with
             Some dir ->
                Some (Filename.concat dir realname)
           | None ->
                find_file_by_name names stale_ext exts)
    | [] ->
         None

let find_file name exts stale_ext =
   (* Look for lowercase and uppercase forms *)
   let uc_name = name in
   let lc_name = String.uncapitalize_ascii name in
   let names = [lc_name, lc_name; uc_name, uc_name] in

   (* In --reflect mode, also search based on the original name without the reflect_ prefix *)
   let names =
      if options.opt_reflect_flag && is_reflect_prefix lc_name then
         let lc_orig_name = chop_reflect_prefix lc_name in
            names @ [lc_orig_name, lc_name]
      else
         names
   in
      find_file_by_name names stale_ext exts

(*
 * Find a dependency. Watch out for stale binaries.
 *)
let find_dependency modname src_exts bin_ext other_exts =
   match find_file modname src_exts (Some bin_ext) with
      Some filename ->
         Some (filename ^ bin_ext)
    | None ->
         begin match find_file modname (bin_ext :: other_exts) None with
            Some filename ->
               Some (filename ^ bin_ext)
          | None ->
               None
         end

(*
 * Find various kinds of files.
 *)
let find_dependency_cmi modname =
   find_dependency modname [".mli"; ".mlz"; ".ml"] ".cmi" [".cmo"; ".cmx"]

let find_dependency_cmiz modname =
   find_dependency modname [".mli"; ".mlz"] ".cmiz" [".ml"; ".cmi"; ".cmo"; ".cmx"]

let find_dependency_cmx modname =
   find_dependency modname [".ml"; ".mlz"] ".cmx" [".cmo"]

let find_include name =
   find_file name [""] None

(************************************************************************
 * Adding dependencies to the table.
 *)
let add_dependency_include dst src table =
   match find_include src with
      Some src ->
         List.fold_left (fun table dst ->
               add_dependency table dst src) table dst
    | None ->
         table

let add_dependency_cmi dst src table =
   match find_dependency_cmi src with
      Some src ->
         List.fold_left (fun table dst ->
               add_dependency table dst src) table dst
    | None ->
         table

let add_dependency_cmiz dst src table =
   match find_dependency_cmiz src with
      Some src ->
         List.fold_left (fun table dst ->
               add_dependency table dst src) table dst
    | None ->
         table

let add_dependency_cmo_cmx dst_cmo dst_cmx src table =
   let cmi_file = find_dependency_cmi src in
   let cmx_file = find_dependency_cmx src in
      match cmi_file, cmx_file with
         Some src_cmi, Some src_cmx ->
            add_dependency (add_dependency table dst_cmo src_cmi) dst_cmx src_cmx
       | Some src_cmi, None ->
            add_dependency (add_dependency table dst_cmo src_cmi) dst_cmx src_cmi
       | None, Some _ ->
            raise (Invalid_argument "Ocamldep.find_dependency_cmo_cmx: bug!")
       | None, None ->
            table

(************************************************************************
 * File scanning.
 *)
let scan_dependencies_exn source_file =
   let basename, is_impl =
      if Filename.check_suffix source_file ".ml" then
         Filename.chop_suffix source_file ".ml", true
      else if Filename.check_suffix source_file ".mli" then
         Filename.chop_suffix source_file ".mli", false
      else
         raise (Invalid_argument ("Unknown suffix in a file " ^ source_file))
   in
   let inx = open_in source_file in
   let lexbuf = Lexing.from_channel inx in
   let summ = create_summary basename is_impl in
   let summ = main summ lexbuf in
      close_in inx;
      summ

(*
 * Collect all the summaries for all the input files.
 *)
let summaries = ref []

let scan_dependencies source_file =
   try summaries := scan_dependencies_exn source_file :: !summaries with
      Sys_error _ ->
         ()

(************************************************************************
 * Summary processing.
 *)

(*
 * Create a new summary for a reflected file.
 * This file depends on the original file,
 * and on the reflected versions of all the prl_structures.
 *)
let summ_reflect summ =
   let { summ_basename       = basename;
         summ_is_impl        = is_impl;
         summ_prl_structures = prl_structures;
         _
       } = summ
   in

   (* Add the Reflect_ prefix to all the dependencies *)
   let prl_structures =
      StringSet.fold (fun name set ->
            if name = "Itt_hoas_theory" then
               set
            else
               StringSet.add (reflect_module_name name) set) prl_structures StringSet.empty
   in

   (* Add dependencies on the reflected theories *)
   let prl_structures =
      List.fold_left (fun set name -> StringSet.add name set) prl_structures reflect_names
   in

   (* The theories will be extending Basic_tactics *)
   let free_structures =
      if is_impl then
         List.fold_left (fun set name -> StringSet.add name set) StringSet.empty reflect_modules
      else
         StringSet.empty
   in
      { summ with summ_basename        = reflect_prefix ^ basename;
                  summ_free_structures = free_structures;
                  summ_prl_structures  = prl_structures;
                  summ_includes        = StringSet.empty;
                  summ_contains_topval = false
      }

(*
 * Postprocess the dependencies.
 *)
let summ_postproc summ =
   let { summ_basename        = basename;
         summ_is_impl         = is_impl;
         summ_free_structures = free_structures;
         summ_prl_structures  = prl_structures;
         summ_contains_topval = contains_topval;
         summ_contains_rules  = contains_rules;
         _
       } = summ
   in

   (* Free_structures includes PRL structures as well *)
   let free_structures = StringSet.union free_structures prl_structures in

   (* Add PRL dependencies *)
   let free_structures =
      if options.opt_prl_flag then
         (* Add default PRL dependencies *)
         let prl_names =
            if is_impl then
               prl_names
            else
               prl_cmi_names
         in
         let free_structures = List.fold_left (fun set name -> StringSet.add name set) free_structures prl_names in

         (*
          * Add dependencies on the topval files.
          *
          * XXX HACK: for an implementation, since we can not check whether the
          * corresponding .mli contains "topval", we always assume dependencies.
          * This is wrong for files in the support/shell directory, so we base
          * it on whether the ml_topval_flag option is set.
          *)
         let free_structures =
            if contains_topval || (is_impl && options.opt_ml_topval_flag) then
               List.fold_left (fun set name -> StringSet.add name set) free_structures topval_names
            else
               free_structures
         in

         (* If it contains a rule, add dependencies on rule files *)
         let free_structures =
            if contains_rules then
               List.fold_left (fun set name -> StringSet.add name set) free_structures rule_names
            else
               free_structures
         in
            free_structures
      else
         free_structures
   in
      { summ with summ_free_structures = free_structures }

(*
 * Add the dependencies for an interface.
 *)
let add_intf_dependencies table summ =
   let { summ_basename        = basename;
         summ_free_structures = free_structures;
         summ_prl_structures  = prl_structures;
         summ_includes        = includes;
         _
       } = summ
   in
   let name_cmi  = basename ^ ".cmi" in
   let name_ppi  = basename ^ ".ppi" in
   let name_cmiz = basename ^ ".cmiz" in
   let table = StringSet.fold (add_dependency_include [name_cmi]) includes table in
   let table = StringSet.fold (add_dependency_cmi [name_cmi]) free_structures table in
   let table =
      if options.opt_omake_flag && options.opt_prl_flag then
         StringSet.fold (add_dependency_cmiz [name_cmiz; name_ppi]) prl_structures table
      else
         table
   in
      table

(*
 * Add the dependencies for an implementation.
 *)
let add_impl_dependencies table summ =
   let { summ_basename        = basename;
         summ_free_structures = free_structures;
         summ_prl_structures  = prl_structures;
         summ_includes        = includes;
         _
       } = summ
   in
   let name_cmi  = basename ^ ".cmi" in
   let name_cmo  = basename ^ ".cmo" in
   let name_mli  = basename ^ ".mli" in
   let name_cmx  = basename ^ ".cmx" in
   let name_ppo  = basename ^ ".ppo" in
   let name_cmiz = basename ^ ".cmiz" in

   (* Add dependencies to the .mli if it exists *)
   let table =
      if Sys.file_exists name_mli then
         let table = add_dependency table name_cmo name_cmi in
         let table = add_dependency table name_cmx name_cmi in
         let table = add_dependency table name_ppo name_cmiz in
            table
      else
         table
   in

   (* The object files depend on the .cmi files for all the modules *)
   let table =
      StringSet.fold (add_dependency_cmo_cmx name_cmo name_cmx) free_structures table
   in

   (* The .ppo depends on the .cmiz files for the PRL modules *)
   let table =
      StringSet.fold (add_dependency_cmiz [name_ppo]) prl_structures table
   in

   (* Add the includes *)
   let table =
      if options.opt_prl_flag then
         StringSet.fold (add_dependency_include [name_ppo]) includes table
      else
         StringSet.fold (add_dependency_include [name_cmo; name_cmx]) includes table
   in
      table

(*
 * Create the dependency set from the summaries.
 *)
let compile_summary_core table summ =
   let summ = summ_postproc summ in
   let table =
      if summ.summ_is_impl then
         add_impl_dependencies table summ
      else
         add_intf_dependencies table summ
   in
      table

(*
 * We may also want to generate dependencies for the reflected theories.
 *)
let compile_summary table summ =
   let table = compile_summary_core table summ in
      if options.opt_reflect_flag then
         compile_summary_core table (summ_reflect summ)
      else
         table

let compile summaries =
   List.fold_left compile_summary StringTable.empty summaries

(************************************************************************
 * Entry point.
 *)
let usage = "Usage: ocamldep [options] <files>"

let args =
   ["-I",        Arg.String (fun dir -> options.opt_path <- options.opt_path @ [dir]), (**)
       "<dir>  Add <dir> to the list of include directories";
    "-prl",      Arg.Unit  (fun () -> options.opt_prl_flag <- true), (**)
       "add dependencies for PRL files";
    "-omake",    Arg.Unit  (fun () -> options.opt_omake_flag <- true), (**)
       "add dependencies on PRL files";
    "-noprl",    Arg.Unit  (fun () -> options.opt_prl_flag <- false), (**)
       "do not add dependencies for PRL files";
    "-notopval", Arg.Unit  (fun () -> options.opt_ml_topval_flag <- false), (**)
       "do not add topval dependencies for .ml files";
    "-modules",  Arg.Unit  (fun () -> options.opt_modules_flag <- true), (**)
       "print modules";
    "-reflect",  Arg.Unit  (fun () -> options.opt_reflect_flag <- true), (**)
       "print reflection dependencies";
    "-noreflect", Arg.Unit (fun () -> options.opt_reflect_flag <- false), (**)
       "do not print reflection dependencies";
    "-native",   Arg.Unit (fun () -> ()), (**)
       "compatibility with OCaml distribtion (ignored)"]

let () =
   Arg.parse args scan_dependencies usage;
   print_dependencies (compile !summaries);
   flush stdout;
   exit 0
}

(*
 * -*-
 * Local Variables:
 * Mode: caml-mode
 * Fill-column: 100
 * End:
 * -*-
 * vim:ts=3:et:tw=100
 *)
