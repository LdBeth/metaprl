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
         buf.buf_str <- str ^ (String_util.create "Buffer.putc" i);

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
         buf.buf_str <- str ^ (String_util.create "Buffer.puts" (i + len));

      (* Add the string *)
      String.blit s 0 str i len;
      buf.buf_index <- i + len

(*
 * Get the contents of the buffer.
 *)
let gets { buf_str = str; buf_index = i } =
   String.sub str 0 i

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
