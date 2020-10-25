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
   val clone       : 'a t -> ('a -> 'b) -> 'b t
   val add         : 'a t -> 'a -> unit
   val iter        : ('a -> unit) -> 'a t -> unit
   val fold        : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
   val last        : 'a t -> 'a option
   val remove_last : 'a t -> unit
   val length      : 'a t -> int
end

module LineBuffer : LineBufferSig =
struct
   type 'a t =
      { mutable lines  : 'a option array;
        mutable first  : int;
        mutable length : int
      }

   let max_queue_length = 100

   let create () =
      { lines = Array.make max_queue_length None;
        first = 0;
        length = 0
      }

   let clone queue f =
      { queue with lines = Array.map (fun x ->
                                 match x with
                                    Some x ->
                                       Some (f x)
                                  | None ->
                                       None) queue.lines
      }

   let length queue =
      queue.length

   let add queue buf =
      let { lines = lines;
            first = first;
            length = length
          } = queue
      in
      let alength = Array.length lines in
      let last = (first + length) mod alength in
         lines.(last) <- Some buf;
         if length = alength then
            queue.first <- (succ first) mod alength
         else
            queue.length <- succ length

   let last queue =
      let { lines = lines;
            first = first;
            length = length
          } = queue
      in
         if length = 0 then
            None
         else
            let alength = Array.length lines in
            let last = (first + length - 1) mod alength in
               lines.(last)

   let remove_last queue =
      let { length = length; _ } = queue in
         if length <> 0 then
            queue.length <- pred length

   let iter f queue =
      let { lines = lines;
            first = first;
            length = length
          } = queue
      in
      let alength = Array.length lines in
         for i = 0 to pred length do
            let j = (first + i) mod alength in
               match lines.(j) with
                  Some x -> f x
                | None -> ()
         done

   let fold f x queue =
      let { lines  = lines;
            first  = first;
            length = length
          } = queue
      in
      let alength = Array.length lines in
      let rec collect x i =
         if i = length then
            x
         else
            let j = (first + i) mod alength in
            let x =
               match lines.(j) with
                  Some y ->
                     f x y
                | None ->
                     x
            in
               collect x (succ i)
      in
         collect x 0
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
   (*
    * The float is the time the entry was added.
    *)
   type 'a t = (float * 'a) StringTable.t

   (*
    * Length is hardcoded for now.
    *)
   let max_queue_length = 100

   (*
    * Empty queue.
    *)
   let empty = StringTable.empty

   (*
    * Remove the oldest element.
    *)
   let remove_oldest table =
      let key =
         StringTable.fold (fun oldest key (time, _) ->
               match oldest with
                  Some (time', _) ->
                     if time < time' then
                        Some (time, key)
                     else
                        oldest
                | None ->
                     Some (time, key)) None table
      in
         match key with
            Some (_, key) ->
               StringTable.remove table key
          | None ->
               table

   (*
    * Test for membership.
    *)
   let mem = StringTable.mem

   (*
    * Add an element to the table.
    *)
   let add table key x =
      let time = Unix.gettimeofday () in
      let table = StringTable.add table key (time, x) in
         if StringTable.cardinal table > max_queue_length then
            remove_oldest table
         else
            table

   (*
    * Get an element from the table.
    *)
   let find table key =
      snd (StringTable.find table key)

   (*
    * Iterate over the queue.
    *)
   let iter f table =
      StringTable.iter (fun key (_, x) ->
            f key x) table

   (*
    * Fold over the queue.
    *)
   let fold f x table =
      StringTable.fold (fun x key (_, y) ->
            f x key y) x table
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
