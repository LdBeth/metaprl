(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

{
(* Remember the possibly free structure identifiers *)

open Printf

module StringSet =
  Set.Make(struct type t = string let compare = compare end)

let free_structure_names = ref StringSet.empty

let add_structure name =
  free_structure_names := StringSet.add name !free_structure_names

(* For nested comments *)

let comment_depth = ref 0

(*
 * These are the implicit names.
 *)
let prl_init_names =
   ["Printf";
    "Mp_debug";
    "Refiner";
    "Refine_exn";
    "Term";
    "Term_util";
    "Theory";
    "Dform";
    "Dform_print";
    "Tactic";
    "Mp_resource";
    "Precedence";
    "Filter_summary"]

let prl_names =
   ["Tactic_type"]
}

rule main = parse
    "open" [' ' '\010' '\013' '\009' '\012'] +
      { struct_name lexbuf; main lexbuf }
  | "include" [' ' '\010' '\013' '\009' '\012'] +
      { struct_name lexbuf; main lexbuf }
  | "derive" [' ' '\010' '\013' '\009' '\012'] +
      { struct_name lexbuf; main lexbuf }
  | ['A'-'Z' '\192'-'\214' '\216'-'\222' ]
    (['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255'
      '\'' '0'-'9' ]) * '.'
      { let s = Lexing.lexeme lexbuf in
        add_structure(String.sub s 0 (String.length s - 1));
        main lexbuf }
  | "\""
      { string lexbuf; main lexbuf }
  | "(*"
      { comment_depth := 1; comment lexbuf; main lexbuf }
  | "'" [^ '\\'] "'"
    { main lexbuf }
  | "'" '\\' ['\\' '\'' 'n' 't' 'b' 'r'] "'"
    { main lexbuf }
  | "'" '\\' ['0'-'9'] ['0'-'9'] ['0'-'9'] "'"
    { main lexbuf }
  | eof
      { () }
  | _
      { main lexbuf }

and struct_name = parse
    ['A'-'Z' '\192'-'\214' '\216'-'\222' ]
    (['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255'
      '\'' '0'-'9' ]) *
      { add_structure(Lexing.lexeme lexbuf) }
  | ""
      { () }

and comment = parse
    "(*"
      { comment_depth := succ !comment_depth; comment lexbuf }
  | "*)"
      { comment_depth := pred !comment_depth;
        if !comment_depth > 0 then comment lexbuf }
  | "\""
      { string lexbuf; comment lexbuf }
  | "''"
      { comment lexbuf }
  | "'" [^ '\\' '\''] "'"
      { comment lexbuf }
  | "'\\" ['\\' '\'' 'n' 't' 'b' 'r'] "'"
      { comment lexbuf }
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
      { string lexbuf }
  | '\\' ['\\' '"' 'n' 't' 'b' 'r']
      { string lexbuf }
  | '\\' ['0'-'9'] ['0'-'9'] ['0'-'9']
      { string lexbuf }
  | eof
      { () }
  | _
      { string lexbuf }

{
(* Print the dependencies *)

let load_path = ref [""]

let prl_flag = ref false

let prl_init_flag = ref false

let rec find_file name = function
   [] ->
      raise Not_found
 | ext::exts ->
      try
         Filename.chop_suffix (Misc.find_in_path !load_path (name ^ ext)) ext
      with Not_found ->
         find_file name exts

let find_dependency_cmi modname deps =
   try
      ((find_file (String.uncapitalize modname) [".mli"; ".cmi"])^".cmi")::deps
   with Not_found ->
      deps

let find_dependency_cmo_cmx modname (cmo_deps,cmx_deps) =
   let name = String.uncapitalize modname in
   let cmi_file =
      try
         Some ((find_file name [".mli"; ".cmi"])^".cmi")
      with Not_found ->
         None
   in
   let cmx_file =
      try
         Some ((find_file name [".ml"; ".cmx"; ".cmo"])^".cmx")
      with Not_found ->
         None
   in
   match cmi_file, cmx_file with
      Some cmi, Some cmx ->
         cmi :: cmo_deps, cmx :: cmx_deps
    | Some cmi, None ->
         cmi :: cmo_deps, cmi :: cmx_deps
    | None, Some cmx ->
         cmo_deps, cmx :: cmx_deps
    | None, None ->
         cmo_deps, cmx_deps

let (depends_on, escaped_eol) =
  match Sys.os_type with
  | "Unix" | "Win32" -> (": ", "\\\n    ")
  | "MacOS" -> ("\196 ", "\182\n    ")
  | x -> Misc.fatal_error ("Ocamldep: unknown system type" ^ x)
;;

let print_dependencies target_file deps =
  match deps with
    [] -> ()
  | _ ->
    print_string target_file; print_string depends_on;
    let rec print_items pos = function
      [] -> print_string "\n"
    | dep :: rem ->
        if pos + String.length dep <= 77 then begin
          print_string dep; print_string " ";
          print_items (pos + String.length dep + 1) rem
        end else begin
          print_string escaped_eol; print_string dep; print_string " ";
          print_items (String.length dep + 5) rem
        end in
    print_items (String.length target_file + 2) deps

let file_dependencies source_file =
  try
    free_structure_names := StringSet.empty;
    let ic = open_in source_file in
    let lb = Lexing.from_channel ic in
    main lb;
    if !prl_flag or !prl_init_flag then
      List.iter add_structure prl_init_names;
    if !prl_flag then
      List.iter add_structure prl_names;
    if Filename.check_suffix source_file ".ml" then begin
      let basename = Filename.chop_suffix source_file ".ml" in
      let init_deps =
        if Sys.file_exists (basename ^ ".mli")
        then let cmi_name = basename ^ ".cmi" in ([cmi_name], [cmi_name])
        else ([], []) in
      let (cmo_deps, cmx_deps) =
        StringSet.fold find_dependency_cmo_cmx !free_structure_names init_deps in
      print_dependencies (basename ^ ".cmo") cmo_deps;
      print_dependencies (basename ^ ".ppo") cmo_deps;
      print_dependencies (basename ^ ".cmx") cmx_deps
    end else
    if Filename.check_suffix source_file ".mli" then begin
      let basename = Filename.chop_suffix source_file ".mli" in
      let deps =
        StringSet.fold find_dependency_cmi !free_structure_names [] in
      print_dependencies (basename ^ ".cmi") deps
    end else
      ();
    close_in ic
  with Sys_error msg ->
    ()

(* Entry point *)

let usage = "Usage: ocamldep [-I <dir>] <files>"

let _ =
  Arg.parse [
     "-I", Arg.String(fun dir -> load_path := !load_path @ [dir]),
           "<dir>  Add <dir> to the list of include directories";
     "-prl_init", Arg.Set prl_init_flag, "add dependencies for PRL files, no Tactic_type";
     "-noprl_init", Arg.Set prl_init_flag, "add dependencies for PRL files, no Tactic_type";
     "-prl", Arg.Set prl_flag, "add dependencies for PRL files";
     "-noprl", Arg.Clear prl_flag, "add dependencies for PRL files"
    ] file_dependencies usage;
  exit 0

}
