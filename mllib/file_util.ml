(*
 * Operations on files.
 *)

open Printf
open Nl_debug
open Nl_pervasives

(*
 * Show the file loading.
 *)
let _ =
   if !debug_load then
      eprintf "Loading File_util%t" eflush

(* Can't open and can't find a file *)
exception CantOpen of string
exception CantFind of string

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
            let word = String_util.sub "File_util.parse_path" name i (j - i) in
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
 * Open a file somewhere in the path.
 *)
let open_in_path path name =
   let rec aux = function
      [] -> raise (CantFind name)
    | dir::rest ->
         let fullname = Filename.concat dir name in
            try let ifile = open_in fullname in (ifile, fullname)
               with Sys_error _ -> aux rest
   in
      aux path

(*
 * Safe file handler.
 *)
let with_input_file name f =
   let iport = open_in name in
   let a = try f iport with x -> close_in iport; raise x in
       close_in iport;
       a

let with_output_file name f =
   let oport = open_out name in
   let a = try f oport with x -> close_out oport; raise x in
      close_out oport;
      a

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
