(*
 * Print terms as strings.
 *)

open Printf
open Debug

open Ml_print_sig
open Ml_print

(*
 * Show the file loading.
 *)
let _ =
   if !debug_load then
      eprintf "Loading Ml_string%t" eflush

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
      let out = String_util.create "Ml_string.get" length in
      let rec squash i = function
         h::t ->
            let len = String.length h in
            let off = i - len in
               String_util.blit "Ml_string.get" h 0 out off len;
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
 * -*-
 * Local Variables:
 * Caml-master: "prlcomp.run"
 * End:
 * -*-
 *)
