(*
 * Print exceptions.
 *)

open Printf

open Debug

open Rformat
open Dform
open Dform_print
open Simple_print

open Filter_ocaml
open Filter_type

(*
 * Print an argument list.
 *)
let rec format_arg_list db buf = function
   (sl, t) :: tl ->
      let rec format = function
         h::t ->
            format_string buf h;
            format_space buf;
            format t
       | [] ->
            format_term db buf t
      in
         format sl;
         format_space buf;
         format_arg_list db buf tl
 | [] ->
      ()

(*
 * Convert an exception to a string.
 *)
let format_exn db buf exn =
   let format = function
      FormatError (name, t) ->
         format_string buf "FormatError:";
         format_space buf;
         format_string buf name;
         format_space buf;
         format_term db buf t
    | NotANumber name ->
         format_string buf "Not a number:";
         format_space buf;
         format_string buf name
    | BadParam name ->
         format_string buf "Bad parameter:";
         format_space buf;
         format_string buf name
    | BadLevelExp l ->
         format_string buf "Bad level expression:";
         format_space buf;
         format_simple_level_exp buf l
    | BadParamCast (p, s) ->
         format_string buf "Bad param cast:";
         format_space buf;
         format_simple_param buf p;
         format_space buf;
         format_string buf "to";
         format_space buf;
         format_string buf s
    | BadArgList l ->
         format_string buf "Bad argument list:";
         format_space buf;
         format_arg_list db buf l
    | BadBinder t ->
         format_string buf "Bad binder:";
         format_space buf;
         format_term db buf t
    | ParseError s ->
         format_string buf "Parse error:";
         format_space buf;
         format_string buf s
    | BadCommand s ->
         format_string buf "Bad command:";
         format_space buf;
         format_string buf s
    | EmptyModulePath s ->
         format_string buf "Empty module path:";
         format_space buf;
         format_string buf s
    | exn ->
         Refine_exn.format_exn db buf exn
   in
      format exn

(*
 * Print an exception if it occurs, then reraise it.
 *)
let print_exn db out exn =
   let buf = new_buffer () in
      format_exn db buf exn;
      print_to_channel 80 buf stderr;
      flush stderr;
      raise exn

let print db f x =
   try f x with
      exn ->
         print_exn db stderr exn

(*
 * $Log$
 * Revision 1.2  1998/06/01 13:52:55  jyh
 * Proving twice one is two.
 *
 * Revision 1.1  1998/04/28 18:30:07  jyh
 * ls() works, adding display.
 *
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
