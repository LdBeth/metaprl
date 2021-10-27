(*
 * Utilities for the bounded buffers.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2004 Mojave Group, Caltech
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * Author: Jason Hickey
 * @email{jyh@cs.caltech.edu}
 * @end[license]
 *)
open Lm_string_set

(*
 * A simple bounded buffer.
 *)
module type LineBufferSig =
sig
   type 'a t

   val create      : unit -> 'a t
   val clone       : ('a -> 'a) -> 'a t -> 'a t
   val add         : 'a t -> 'a -> unit
   val iter        : ('a -> unit) -> 'a t -> unit
   val fold        : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
   val last        : 'a t -> 'a option
   val remove_last : 'a t -> unit
   val length      : 'a t -> int

   val version     : string
end

module LineBuffer : LineBufferSig =
struct
   type 'a t =
      { mutable lines  : 'a array;
        alength : int;
        mutable first  : int;
        mutable length : int
      }

   let max_queue_length = 100

   let create () =
      { lines = [||]; (* initially empty *)
        alength = max_queue_length;
        first = 0;
        length = 0
      }

   let clone f ({ lines = lines;
                  alength = alength;
                  first = first;
                  length = length } as queue) =
      if length = 0 then queue else begin
         let new_lines = Array.make alength (f (Array.get lines first)) in
            for i = first + 1 to first + length - 1 do
               let j = i mod alength in
                  Array.set new_lines j (f (Array.get lines j))
            done;
            { queue with lines = new_lines }
      end

   let length queue =
      queue.length

   let add queue buf =
      let { lines = lines;
            alength = alength;
            first = first;
            length = length
          } = queue
      in
         if length = 0 then (* initialize *)
         begin
            queue.lines <- Array.make alength buf;
            queue.first <- 1;
            queue.length <- 1
         end
         else begin
            let last = (first + length) mod alength in
               lines.(last) <- buf;
               if length = alength then
                  queue.first <- (succ first) mod alength
               else
                  queue.length <- succ length
         end

   let last queue =
      let { lines = lines;
            alength = alength;
            first = first;
            length = length
          } = queue
      in
         if length = 0 then
            None
         else
            let last = (first + length - 1) mod alength in
               Some lines.(last)

   let remove_last queue =
      let { length = length; _ } = queue in
         if length <> 0 then
            queue.length <- pred length

   let iter f queue =
      let { lines = lines;
            alength = alength;
            first = first;
            length = length
          } = queue
      in
         for i = 0 to pred length do
            let j = (first + i) mod alength in
               f lines.(j)
         done

   let fold f x queue =
      let { lines  = lines;
            alength = alength;
            first  = first;
            length = length
          } = queue
      in
      let rec collect x i =
         if i = length then
            x
         else
            let j = (first + i) mod alength in
            let x =
               f x lines.(j)
            in
               collect x (succ i)
      in
         collect x 0

   (* Change this string when the representaion changes *)
   let version = "2.0"

end

(************************************************************************
 * Directories are maintained as a StringTable.t
 *)
module type LineTableSig =
sig
   type 'a t

   val empty  : 'a t
   val mem    : 'a t -> string -> bool
   val add    : 'a t -> string -> 'a -> 'a t
   val find   : 'a t -> string -> 'a
   val iter   : (string -> 'a -> unit) -> 'a t -> unit
   val fold   : ('a -> string -> 'b -> 'a) -> 'a -> 'b t -> 'a
end

module LineTable : LineTableSig =
struct

   type 'a t = 'a StringTable.t * string Lm_fqueue.t

   (*
    * Length is hardcoded for now.
    *)
   let max_queue_length = 100

   (*
    * Empty queue.
    *)
   let empty = StringTable.empty, Lm_fqueue.empty

   (*
    * Remove the oldest element.
    *)
   let remove_oldest table queue =
      let key, queue = Lm_fqueue.take queue
      in
         StringTable.remove table key, queue

   (*
    * Test for membership.
    *)
   let mem (table, _) key = StringTable.mem table key

   (*
    * Add an element to the table.
    *)
   let add (table, queue) key x =
      let table = StringTable.add table key x in
      let queue = Lm_fqueue.add key queue in
         if StringTable.cardinal table > max_queue_length then
            remove_oldest table queue
         else
            table, queue

   (*
    * Get an element from the table.
    *)
   let find (table, _) key =
      StringTable.find table key

   (*
    * Iterate over the queue.
    *)
   let iter f (table, _) =
      StringTable.iter f table

   (*
    * Fold over the queue.
    *)
   let fold f x (table, _) =
      StringTable.fold f x table
end

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
