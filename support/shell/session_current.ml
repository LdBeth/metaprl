(*
 * This takes care of creating shells, and load/save of sessions.
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
open Lm_rformat
open Lm_string_set
open Lm_thread

open Line_buffer
open Session_sig
open Shell_util

(*
 * Current session.
 *)
let default_shared =
   { shared_directories    = LineTable.empty;
     shared_files          = LineTable.empty
   }

let shared_entry = State.shared_val "Session_current.shared_entry" default_shared

(*
 * Current session.
 *)
let default_session =
   { session_history        = LineBuffer.create ();
     session_messages       = LineBuffer.create ();
     session_content_buffer = new_buffer ();
     session_content_table  = StringTable.empty;
     session_options        = ls_options_default
   }

let fork_session session =
   let { session_history = history;
         session_messages = messages;
         session_content_buffer = content_buffer
       } = session
   in
      { session with session_history = LineBuffer.clone history (fun s -> s);
                     session_messages = LineBuffer.clone messages clone_buffer;
                     session_content_buffer = clone_buffer content_buffer
      }

let session_entry = State.private_val "Session_current.session_entry" default_session fork_session

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
