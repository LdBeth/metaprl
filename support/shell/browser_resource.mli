(*
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
open Refiner.Refiner.TermType
open Mp_resource

(************************************************************************
 * Different kinds of elements to display.
 *
 * These terms are not passed through display forms.
 *)

(*
 * A button with the given label.
 * The command is text that should be executed
 * when the button is pushed.
 *)
declare button[label:s, command:s]

(*
 * A menu is like a button, but it has collections of
 * elements that act like buttons.
 *)
declare menu[menuname:s, label:s]
declare menuitem[menuname:s, label:s, command:s]

(************************************************************************
 * Resource
 *)

(*
 * The resulting info.
 *)
type browser_info =
   { browser_buf     : Buffer.t;
     browser_styles  : Buffer.t;
     browser_macros  : Buffer.t
   }

(*
 * The current state of the browser.
 *)
type pid = Lm_thread_shell.pid

type browser_state =
   { browser_directories : string list;
     browser_files       : string list;
     browser_history     : string list;
     browser_options     : string;
     browser_id          : pid;
     browser_sessions    : (pid * string) list
   }

(*
 * Menubar has a default.
 *)
val default_menubar_info : browser_state -> browser_info
val default_commandbar_info : browser_state -> browser_info

(*
 * There are two menus:
 *   The menubar goes at the top of the page.
 *   The commandbar goes by the input area.
 *)
type menu_item = term * (unit -> bool)

val always_enabled : unit -> bool
val refine_is_enabled : unit -> bool

resource (menu_item, browser_state -> browser_info) menubar
resource (menu_item, browser_state -> browser_info) commandbar

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
