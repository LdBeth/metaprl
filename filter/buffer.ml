(*
 * An "infinite" buffer.  The buffer grows as the space requirements
 * increase.
 *)

open Printf
open Debug

(*
 * Show the file loading.
 *)
let _ =
   if !debug_load then
      eprintf "Loading Buffer%t" eflush

type t =
   { mutable buf_str : string;
     mutable buf_index : int
   }

(*
 * Create a new empty buffer.
 *)
let create () =
   { buf_str = String.create 32;
     buf_index = 0
   }

(*
 * Clear the buffer.
 *)
let clear buf =
   buf.buf_index <- 0

(*
 * Place something in the buffer,
 * and increase the buffer size if necessary.
 *)
let putc buf c =
   let { buf_str = str; buf_index = i } = buf in
      if i = String.length str then
         (* Grow the buffer *)
         buf.buf_str <- str ^ (String.create i);

      (* Insert the char *)
      str.[i] <- c;
      buf.buf_index <- i + 1

(*
 * Place a string in the buffer.
 *)
let puts buf s =
   let { buf_str = str; buf_index = i } = buf in
   let len = String.length s in
      if i + len > String.length str then
         buf.buf_str <- str ^ (String.create (i + len));

      (* Add the string *)
      String.blit s 0 str i len;
      buf.buf_index <- i + len

(*
 * Get the contents of the buffer.
 *)
let gets { buf_str = str; buf_index = i } =
   String.sub str 0 i

(*
 * $Log$
 * Revision 1.4  1998/06/01 13:52:43  jyh
 * Proving twice one is two.
 *
 * Revision 1.3  1998/04/24 19:38:11  jyh
 * Updated debugging.
 *
 * Revision 1.2  1998/04/24 02:41:41  jyh
 * Added more extensive debugging capabilities.
 *
 * Revision 1.1  1998/02/19 17:13:54  jyh
 * Splitting filter_parse.
 *
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
