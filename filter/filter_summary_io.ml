(*
 * This module defines an interface for saving information about
 * modules.  We record information about each module interface,
 * to be used in the definition of the module and in submodules.
 *
 *)

open Opname
open File_util
open Term
open Term_util
open Filter_type
open Filter_summary

(************************************************************************
 * TYPES                                                                *
 ************************************************************************)

type module_base_io =
   { io_base : module_base;
     mutable io_path : string list
   }

let new_module_base_io path =
   { io_base = new_module_base ();
     io_path = path
   }

let set_module_base_path base path =
   base.io_path <- path

let module_name base = Filter_summary.module_name base.io_base

let module_fullname base = Filter_summary.module_fullname base.io_base

let push_module base = Filter_summary.push_module base.io_base

(************************************************************************
 * BASE IO                                                              *
 ************************************************************************)

(*
 * Suffix of info files.
 *)
let info_suffix = ".cmiz"

(*
 * This is a the magic number for this version of the
 * filter.
 *)
let magic_number = 0x73095472

exception BadMagicNumber of int

(*
 * Read a summary from a file.
 *)
let read_info ifile =
   let i = input_binary_int ifile in
      if i = magic_number then
         (input_value ifile : module_info)
      else
         raise (BadMagicNumber i)

(*
 * Print it to a file.
 *)
let write_info info ofile =
   output_binary_int ofile magic_number;
   output_value ofile info

(************************************************************************
 * INPUT                                                                *
 ************************************************************************)

(*
 * Load a summary from a file given by name.
 *)
let load_summary base name ifile =
   match name with
      [] ->
         (* This will never happen *)
         failwith "load_summary"
    | top::rest ->
         let info = read_info ifile in
            push_module base top [top] info;
            find_sub_module info rest

(*
 * Load a specific module.
 *)
let load_module base path mpath id =
   match Filter_summary.find_module base.io_base mpath id with
      None ->
         (* Load it from the filesystem *)
         let ifile = open_in path in
         let sum = load_summary base mpath ifile in
            close_in ifile;
            if id = ignore_id or find_id sum = id then
               sum
            else
               raise Not_found
    | Some sum ->
         sum

(*
 * Load a module by name.
 *)
let load_in_path base path name mpath id =
   let name = String.uncapitalize name in
   let rec aux = function
      dir::rest ->
         let fullname = Filename.concat dir name in
            begin
               try
                  let ifile = open_in fullname in
                  let sum = load_summary base mpath ifile in
                     close_in ifile;
                     if id = ignore_id or find_id sum = id then
                        sum
                     else
                        aux rest
                     
               with
                  Sys_error _ -> aux rest
            end
    | [] -> raise (CantFind name)
   in
      aux path

let find_module base mpath id =
   match Filter_summary.find_module base.io_base mpath id with
      None ->
         begin
            (* Load it from the filesystem *)
            match mpath with
               [] -> raise (EmptyModulePath "find_module")
             | top::rest ->
                  let sum = load_in_path base base.io_path (top ^ info_suffix) mpath id in
                     if rest <> [] then
                        find_sub_module sum rest
                     else
                        sum
         end
    | Some sum ->
         sum

(************************************************************************
 * OUTPUT                                                               *
 ************************************************************************)

(*
 * Save a module.
 *)
let save_module info path =
   let ofile = open_out path in
      (try write_info info ofile with
         x -> close_out ofile;
              raise x);
      close_out ofile

(*
 * $Log$
 * Revision 1.1  1997/04/28 15:50:59  jyh
 * This is the initial checkin of Nuprl-Light.
 * I am porting the editor, so it is not included
 * in this checkin.
 *
 * Directories:
 *     refiner: logic engine
 *     filter: front end to the Ocaml compiler
 *     editor: Emacs proof editor
 *     util: utilities
 *     mk: Makefile templates
 *
 * Revision 1.1  1996/09/02 19:43:16  jyh
 * Semi working package management.
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
