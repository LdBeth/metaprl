(*
 * Print terms as strings.
 *)

open Ml_print_sig
open Ml_print

(*
 * String printer.
 *)
module StringFile =
struct
   type t = { mutable buf : string list }
   type name = unit
   type out = string
   
   (*
    * Creation.
    *)
   let create () = { buf = [] }
   let close _ = ()
   
   (*
    * Printing.
    *)
   let puti _ _ = ()
   let put file s =
      file.buf <- s :: file.buf

   let get { buf = buf } =
      let rec count i = function
         h::t ->
            count (i + String.length h) t
       | [] ->
            i
      in
      let length = count 0 buf in
      let out = String.create length in
      let rec squash i = function
         h::t ->
            let len = String.length h in
            let off = i - len in
               String.blit h 0 out off len;
               squash off t
       | [] ->
            ()
      in
         squash length buf;
         out
end

(*
 * Printer.
 *)
module StringPrint = MakePrinter (StringFile)

(*
 * $Log$
 * Revision 1.1  1997/04/28 15:51:26  jyh
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
 *
 * -*-
 * Local Variables:
 * Caml-master: "prlcomp.run"
 * End:
 * -*-
 *)
