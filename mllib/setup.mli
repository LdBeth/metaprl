(*
 * Values for MetaPRL environment variables.
 *
 * ----------------------------------------------------------------
 *
 * Copyright (C) 2004 MetaPRL Group, Caltech
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
 * Authors: Jason Hickey <jyh@cs.caltech.edu>
 *          Aleksey Nogin <nogin@cs.caltech.edu>
 *)

val environ_prefix : string  (* MP *)
val root : unit -> string    (* $(MP_ROOT); must be defined *)
val lib : unit -> string     (* $(MPLIB) - when defined, or $(MP_ROOT)/lib - when directory exists *)

(*
 * $(HOME)/.metaprl directory (uses C:\metaprl or /tmp/metaprl-<uid>, when
 * $(HOME) is not defined)
 *
 * The first time this function is called, it will make sure that
 * the directory exists and is readable and writable
 *)
val home : unit -> string    (* $(HOME)/.metaprl *)

(*
 * Read the editor from the $HOME/.metaprl/editor file.
 *)
val editor : unit -> string

(*
 * Hostname - $(MP_BROWSER_HOSTNAME), or Unix.gethostname ()
 *)
val hostname_var : string (* "browser_hostname" *)
val gethostname : unit -> string
val sethostname : string -> unit

(*
 * SSL certivicates. Will use different ones for different hostnames
 * and will try to create if they do not exist.
 *)
val server_pem : unit -> string
val dh_pem : unit -> string
val client_pem : unit -> string

(*
 * Browser command to use by default.
 * /usr/bin/htmlview, if exists, otherwise /usr/bin/mozilla, if exists, otherwise None
 *)
val default_browser_string : unit -> string option

(*
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
