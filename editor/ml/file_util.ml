(*
 * Some utilities for working with filenames.
 *
 *)

(*
 * Split the name into a list of names.
 * If there is an initial /, then this is an
 * absolute name.  Otherwise it is relative to the current module.
 *)
let parse_path dir name =
   let len = String.length name in
   let rec aux path i j =
      if i < len then
         if j = len or name.[j] = '/' then
            let word = String.sub name i (j - i) in
            let path' =
               match word with
                  "" -> path
                | "." -> path
                | ".." ->
                     if path = [] then
                        []
                     else
                        List.tl path
                | _ -> word::path
            in
               aux path' (j + 1) (j + 1)
         else
            aux path i (j + 1)
      else
         path
   in
   let path =
      if len <> 0 & name.[0] = '/' then
         []
      else
         dir
   in
      List.rev (aux path 0 0)

(*
 * Build the path from the list.
 *)
let rec build_path = function
   [h] -> h
 | h::t ->
      h ^ "/" ^ (build_path t)
 | [] -> "."

(*
 * Get dir name.
 *)
let path_dir path =
   match parse_path [] path with
      [] -> "."
    | x ->
         let h, _ = List_util.split_last x in
            build_path h

(*
 * Get filename.
 *)
let path_file path =
   match parse_path [] path with
      [] -> "."
    | x ->
         List_util.last x

(*
 * $Log$
 * Revision 1.1  1997/08/06 16:17:11  jyh
 * This is an ocaml version with subtyping, type inference,
 * d and eqcd tactics.  It is a basic system, but not debugged.
 *
 * Revision 1.1  1996/09/02 19:32:57  jyh
 * Semi-working package management.
 *
 * -*-
 * Local Variables:
 * Caml-master: "prlcomp.run"
 * End:
 * -*-
 *)
