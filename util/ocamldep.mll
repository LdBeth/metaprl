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

{
(* Remember the possibly free structure identifiers *)

module StringSet =
  Set.Make(struct type t = string let compare = compare end)

let free_structures = ref StringSet.empty
let prl_structures = ref StringSet.empty

let add_structure name =
  free_structures := StringSet.add name !free_structures

let add_prl_structure name =
   prl_structures := StringSet.add name !prl_structures

(* For nested comments *)

let comment_depth = ref 0

(* For nested terms *)

let term_depth = ref 0

(* Source quotations *)

let contains_source_quot = ref false

(* MetaPRL topval declarations *)

let contains_topval = ref false

(*
 * These are the implicit names.
 *)
let prl_names = [
   "Obj";
   "Lm_symbol";
   "Lm_debug";
   "Refiner";
   "Refine_exn";
   "Theory";
   "Dform";
   "Mp_resource";
   "Precedence";
   "Tactic_type";
]

(*
 * These are the names used by "topval" declarations
 *)
let topval_names = [
   "Mptop";
   "Shell_sig";
]

}

let white = [' ' '\010' '\013' '\009' '\012']
let modname = ['A'-'Z' '\192'-'\214' '\216'-'\222' ]
              (['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255'
                '\'' '0'-'9' ]) *
let source_quote = "<:ext<"
let quotestart = '<' ':' (['a'-'z'])+ '<'

rule main = parse
    "open" white+ | "include" white+
  | "module" white+ modname white+ '=' white+
      { struct_name lexbuf; main lexbuf }
  | "derive" white+ | "extends" white+
      { prl_struct_name lexbuf; main lexbuf }
  | "topval" white+
      { contains_topval := true; main lexbuf }
  (* Interactive rules and rewrites are justified via Shell *)
  | white "interactive" white+ | white "interactive_rw" white+ | white "derived" white+
      { add_structure "Shell"; main lexbuf }
  | modname '.'
      { let s = Lexing.lexeme lexbuf in
        add_structure(String.sub s 0 (String.length s - 1));
        main lexbuf }
  | "\""
      { string lexbuf; main lexbuf }
  | source_quote
      { source_quot lexbuf; contains_source_quot := true; main lexbuf }
  | "(*" | "<<" | quotestart
      { comment_depth := 1; comment lexbuf; main lexbuf }
  | "'" [^ '\\'] "'"
  | "'" '\\' ['\\' '\'' 'n' 't' 'b' 'r'] "'"
  | "'" '\\' ['0'-'9'] ['0'-'9'] ['0'-'9'] "'"
    { main lexbuf }
  | eof
      { () }
  | _
      { main lexbuf }

and struct_name = parse
    modname
      { add_structure(Lexing.lexeme lexbuf) }
  | ""
      { () }

and prl_struct_name = parse
    modname
      { add_prl_structure(Lexing.lexeme lexbuf) }
  | ""
      { () }

and comment = parse
    "(*" | "<<" | quotestart
      { comment_depth := succ !comment_depth; comment lexbuf }
  | "*)" | ">>"
      { comment_depth := pred !comment_depth;
        if !comment_depth > 0 then comment lexbuf }
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

(* There are no nested source quotations *)
(* Inside source quotations we do not obey strings or escaped characters,
   since they may not exist according to the source syntax rules *)
and source_quot = parse
  | ">>" | eof
      { () }
  | _
      { source_quot lexbuf }

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

(* Print the dependencies *)

let load_path = ref [""]

let prl_flag = ref false
let omake_flag = ref false
let ml_topval_flag = ref true
let modules_flag = ref false

let syntaxdef_prereq = "syntax.pho"

let find_in_path path name name' =
   let rec try_dir = function
      [] -> raise Not_found
    | dir::rem ->
        let fullname = Filename.concat dir name in
        if Sys.file_exists fullname then fullname else
        let fullname = Filename.concat dir name' in
        if Sys.file_exists fullname then fullname else try_dir rem
    in try_dir path

let rec find_file name = function
   [] ->
      raise Not_found
 | ext::exts ->
      try Filename.chop_suffix (find_in_path !load_path ((String.uncapitalize name) ^ ext) (name ^ ext)) ext
      with Not_found -> find_file name exts

let find_dependency_cmi modname deps =
   try
      ((find_file modname [".mli"; ".cmi"; ".mlz"; ".ml"; ".cmo"; ".cmx"])^".cmi")::deps
   with Not_found ->
      deps

let find_dependency_cmiz modname deps =
   try
      ((find_file modname [".mli"; ".cmiz"; ".mlz"; ".ml"; ".cmi"; ".cmo"; ".cmx"])^".cmiz")::deps
   with Not_found ->
      deps

let find_dependency_cmo_cmx modname (cmo_deps,cmx_deps) =
   let cmi_file =
      try
         Some ((find_file modname [".mli"; ".cmi"; ".mlz"; ".ml"; ".cmo"; ".cmx"])^".cmi")
      with Not_found ->
         None
   in
   let cmx_file =
      try
         Some ((find_file modname [".ml"; ".cmx"; ".mlz"; ".cmo"])^".cmx")
      with Not_found ->
         None
   in
   match cmi_file, cmx_file with
      Some cmi, Some cmx ->
         cmi :: cmo_deps, cmx :: cmx_deps
    | Some cmi, None ->
         cmi :: cmo_deps, cmi :: cmx_deps
    | None, Some cmx ->
         raise(Invalid_argument "Ocamldep.find_dependency_cmo_cmx: bug!")
    | None, None ->
         cmo_deps, cmx_deps

let (depends_on, escaped_eol) =
  match Sys.os_type with
  | "Unix" | "Win32" | "Cygwin" -> (": ", "\\\n    ")
  | "MacOS" -> ("\196 ", "\182\n    ")
  | x -> invalid_arg ("Ocamldep: unknown system type: " ^ x)
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
    print_items (String.length target_file + 2) deps;
    flush stdout

let file_dependencies source_file =
  try
    contains_topval := false;
    contains_source_quot := false;
    free_structures := StringSet.empty;
    prl_structures := StringSet.empty;
    let ic =
       if source_file != "-" then
          open_in source_file
       else begin
          print_endline "Reading from stdin...";
          stdin
       end
    in
    let lb = Lexing.from_channel ic in
    main lb;
    free_structures := StringSet.union !free_structures !prl_structures;
    if !modules_flag then begin
       StringSet.iter (fun s -> print_string s; print_char ' ') !prl_structures;
       print_newline ()
    end else begin
    if !prl_flag then
      List.iter add_structure prl_names;
    if !prl_flag && !contains_topval then
      List.iter add_structure topval_names;
    if Filename.check_suffix source_file ".ml" then begin
      let basename = Filename.chop_suffix source_file ".ml" in
      let init_deps,init_ppo_dep =
        if Sys.file_exists (basename ^ ".mli")
        then let cmi_name = basename ^ ".cmi" in (([cmi_name], [cmi_name]), [basename ^ ".cmiz"])
        else (([], []), []) in
      let (cmo_deps, cmx_deps) =
        StringSet.fold find_dependency_cmo_cmx !free_structures init_deps in
      let ppo_deps =
         if !omake_flag then
            StringSet.fold find_dependency_cmiz !prl_structures (fst init_deps)
         else cmo_deps
      in
      let ppo_deps =
         if !contains_source_quot then syntaxdef_prereq :: ppo_deps else ppo_deps
      in
      (* XXX HACK: since we can not check whether the corresponding .mli contains "topval",
         in prl mode we always assume dependencies - at least on the .cmi

         JYH: unfortunately this is wrong for files in the support/shell directory.  *)
      let cmo_deps, cmx_deps =
         if !prl_flag && !ml_topval_flag then
            let extra_deps = List.fold_right find_dependency_cmi topval_names [] in
               (extra_deps @ cmo_deps), (extra_deps @ cmx_deps)
         else cmo_deps, cmx_deps
      in
      print_dependencies (basename ^ ".cmo") cmo_deps;
      if !prl_flag then print_dependencies (basename ^ ".ppo") ppo_deps;
      print_dependencies (basename ^ ".cmx") cmx_deps
    end else
    if Filename.check_suffix source_file ".mli" then begin
      let basename = Filename.chop_suffix source_file ".mli" in
      print_dependencies (basename ^ ".cmi") (StringSet.fold find_dependency_cmi !free_structures []);
      if !omake_flag && !prl_flag then
         print_dependencies (basename ^ ".cmiz") (StringSet.fold find_dependency_cmiz !prl_structures [])
    end else
      raise(Invalid_argument("Unknown suffix in a file " ^ source_file))
    end;
    close_in ic
  with Sys_error msg ->
    ()

(* Entry point *)

let usage = "Usage: ocamldep [-I <dir>] <files>"

let _ =
  Arg.parse [
     "-I", Arg.String(fun dir -> load_path := !load_path @ [dir]),
           "<dir>  Add <dir> to the list of include directories";
     "-prl", Arg.Set prl_flag, "add dependencies for PRL files";
     "-omake", Arg.Set omake_flag, "add dependencies on PRL files";
     "-noprl", Arg.Clear prl_flag, "do not add dependencies for PRL files";
     "-notopval", Arg.Clear ml_topval_flag, "do not add topval dependencies for .ml files";
     "-modules", Arg.Set modules_flag, "print modules";
     "-native", Arg.Unit (fun () -> ()), "compatibility with OCaml distribtion (ignored)"
    ] file_dependencies usage;
  exit 0
