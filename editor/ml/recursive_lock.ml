(*
 * Recursive lock.  This allws the calling thread to perform
 * recursive locks.  The unlock function returns true iff
 * this was the last recursive call.
 *
 * ----------------------------------------------------------------
 *
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/index.html for information on Nuprl,
 * OCaml, and more information about this system.
 *
 * Copyright (C) 1999 Jason Hickey, Cornell University
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
 * jyh@cs.cornell.edu
 *)

(*
 * The data we need for the lock.
 * The count is the number of recursive calls.
 *)
type t =
   { lock : Mutex.t;
     cond : Condition.t;
     mutable count : int;
     mutable locked_id : int option;
     mutable pending_count : int
   }

(*
 * Create the lock.
 *)
let create () =
   { lock = Mutex.create ();
     cond = Condition.create ();
     count = 0;
     locked_id = None;
     pending_count = 0
   }

(*
 * Lock it.  Check for recursive locks.
 *)
let lock lock =
   let self = Thread.id (Thread.self ()) in
      Mutex.lock lock.lock;
      let rec wait () =
         match lock.locked_id with
            Some id ->
               if id = self then
                  begin
                     lock.count <- succ lock.count;
                     Mutex.unlock lock.lock
                  end
               else
                  begin
                     lock.pending_count <- succ lock.pending_count;
                     Condition.wait lock.cond lock.lock;
                     lock.pending_count <- pred lock.pending_count;
                     wait ()
                  end
          | None ->
               lock.locked_id <- Some self;
               lock.count <- 1;
               Mutex.unlock lock.lock
      in
         wait ()

(*
 * Unlock it only if the last lock was not by us.
 *)
let unlock lock =
   Mutex.lock lock.lock;
   lock.count <- pred lock.count;
   if lock.count = 0 then
      begin
         lock.locked_id <- None;
         if lock.pending_count <> 0 then
            Condition.signal lock.cond;
         Mutex.unlock lock.lock;
         true
      end
   else
      begin
         Mutex.unlock lock.lock;
         false
      end

(*
 * -*-
 * Local Variables:
 * Caml-master: "nl"
 * End:
 * -*-
 *)
