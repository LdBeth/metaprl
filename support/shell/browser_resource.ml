(*
 * The resource for the Browser display.
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
open Printf

open Refiner.Refiner.Term
open Refiner.Refiner.TermMan
open Refiner.Refiner.RefineError

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
declare menubar[menuname:s, label:s]
declare menuitem[menuname:s, label:s, command:s]

(*
 * The user is also allowed to specify additional style pages.
 *)
declare style[text:s]

(************************************************************************
 * Resource.
 *)

(*
 * The resource info.
 *)
type menu_kind =
   MenuBar
 | MenuButton

type menu =
   { menu_kind     : menu_kind;
     menu_label    : string;
     menu_items    : (string * string) list
   }

type info =
   { info_buttons : (string * string) list;
     info_menus   : menu StringTable.t;
     info_styles  : string list
   }

(*
 * The resulting info.
 *)
type browser_info =
   { browser_styles  : Buffer.t;
     browser_menubar : Buffer.t;
     browser_buttons : Buffer.t;
     browser_menu_macros : string StringTable.t;
     browser_buttons_macros : string StringTable.t
   }

(*
 * Empty info.
 *)
let info_empty =
   { info_buttons = [];
     info_menus = StringTable.empty;
     info_styles = []
   }

(*
 * Figure out what kind of item to add to the resource.
 *)
let improve info t =
   match explode_term t with
      << button[label:s, command:s] >> ->
         { info with info_buttons = (label, command) :: info.info_buttons }
    | << menu[menuname:s, label:s] >> ->
         let menu = { menu_kind = MenuButton; menu_label = label; menu_items = [] } in
            { info with info_menus = StringTable.add info.info_menus menuname menu }
    | << menubar[menuname:s, label:s] >> ->
         let menu = { menu_kind = MenuBar; menu_label = label; menu_items = [] } in
            { info with info_menus = StringTable.add info.info_menus menuname menu }
    | << menuitem[menuname:s, label:s, command:s] >> ->
         let menu =
            try StringTable.find info.info_menus menuname with
               Not_found ->
                  raise (RefineError ("Browser_resource.improve", StringTermError ("menu not declared: " ^ menuname, t)))
         in
         let menu = { menu with menu_items = (label, command) :: menu.menu_items } in
            { info with info_menus = StringTable.add info.info_menus menuname menu }
    | << style[text:s] >> ->
         { info with info_styles = text :: info.info_styles }
    | _ ->
         raise (RefineError ("Browser_resource.improve", StringTermError ("unrecognized term", t)))

(*
 * Extract the map.
 *)
let extract info =
   let { info_buttons = buttons;
         info_menus   = menus;
         info_styles  = styles
       } = info
   in

   (* Collect the button text *)
   let buttons_buf    = Buffer.create 32 in
   let buttons_macros = StringTable.empty in
   let buttons_macros =
      List.fold_left (fun macros (label, command) ->
            let sym = sprintf "id%d" (StringTable.cardinal macros) in
               bprintf buttons_buf "<input type=button name=\"%s\" value=\"%s\" onclick=\"parent.ButtonCommand(macros, this);\">\n" sym label;
               StringTable.add macros sym command) buttons_macros (List.rev buttons)
   in

   (* Collect the menu text *)
   let buttons_macros =
      StringTable.fold (fun macros _ menu ->
            let { menu_kind = kind; menu_label = label; menu_items = items } = menu in
               if items = [] || kind <> MenuButton then
                  macros
               else
                  let macros =
                     bprintf buttons_buf "<select name=\"%s\" onChange=\"parent.MenuCommand(macros, this, true);\">\n" label;
                     List.fold_left (fun macros (label, command) ->
                           let sym = sprintf "id%d" (StringTable.cardinal macros) in
                              bprintf buttons_buf "<option value=\"%s\">%s</option>\n" sym label;
                              StringTable.add macros sym command) macros (List.rev items)
                  in
                     bprintf buttons_buf "</select>\n";
                     macros) buttons_macros menus
   in

   (* Collect the menubar *)
   let menu_buf    = Buffer.create 32 in
   let menu_macros = StringTable.empty in
   let menu_macros =
      StringTable.fold (fun macros _ menu ->
            let { menu_kind = kind; menu_label = label; menu_items = items } = menu in
               if kind <> MenuBar then
                  macros
               else
                  let macros =
                     bprintf menu_buf "<select name=\"%s\" onChange=\"parent.MenuCommand(macros, this, true);\">\n" label;
                     bprintf menu_buf "<option value=\"\"><b>%s</b></option>\n" label;
                     List.fold_left (fun macros (label, command) ->
                           let sym = sprintf "id%d" (StringTable.cardinal macros) in
                              bprintf menu_buf "<option value=\"%s\">%s</option>\n" sym label;
                              StringTable.add macros sym command) macros (List.rev items)
                  in
                     bprintf menu_buf "</select>\n";
                     macros) menu_macros menus
   in

   (* Collect the style sheets *)
   let style_buf = Buffer.create 32 in
   let () =
      List.iter (fun s ->
            Buffer.add_string style_buf s;
            Buffer.add_char style_buf '\n') (List.rev styles)
   in
      { browser_menubar = menu_buf;
        browser_buttons = buttons_buf;
        browser_styles  = style_buf;
        browser_menu_macros = menu_macros;
        browser_buttons_macros = buttons_macros
      }

(*
 * Now, finally, the resource.
 *)
let term_collection =
   Functional (**)
      { fp_empty = info_empty;
        fp_add = improve;
        fp_retr = extract
      }

let resource (term, browser_info) browser =
   term_collection

let resource browser +=
    [<< menubar["file", "--File--"] >>;
     << menuitem["file", "Save", "save ()"] >>]

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
