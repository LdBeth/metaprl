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
declare menuitem[menuname:s, label:s, command:s]

(*
 * The user is also allowed to specify additional style pages.
 *)
declare style[text:s]

(************************************************************************
 * Types.
 *)

(*
 * The resource info.
 *)
type command =
   { command_label : string;
     command_value : string
   }

type menu =
   { menu_name     : string;
     menu_label    : string;
     menu_items    : command list
   }

type info =
   { info_buttons : command list;
     info_menus   : menu list;
     info_styles  : string list
   }

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
type browser_state =
   { browser_directories : string list;
     browser_files       : string list;
     browser_history     : string list;
     browser_options     : string;
     browser_id          : int;
     browser_sessions    : (int * string) list
   }

(************************************************************************
 * Proxyedit names.
 *)

let proxyedit_of_filename name =
   name ^ "/" ^ "info.prl"

let filename_of_proxyedit name =
   if Filename.basename name = "info.prl" then
      Filename.dirname name
   else
      name

(************************************************************************
 * Implementation.
 *)

(*
 * Empty info.
 *)
let info_empty =
   { info_buttons = [];
     info_menus   = [];
     info_styles  = []
   }

(*
 * Replace a menu item.
 *)
let menu_replace info name f =
   let rec replace menus =
      match menus with
         menu :: menus ->
            if menu.menu_name = name then
               f menu :: menus
            else
               menu :: replace menus
       | [] ->
            raise Not_found
   in
      { info with info_menus = replace info.info_menus }

(*
 * Figure out what kind of item to add to the resource.
 *)
let improve info t =
   match explode_term t with
      << button[label:s, arg:s] >> ->
         let command = { command_label = label; command_value = arg } in
            { info with info_buttons = command :: info.info_buttons }
    | << menu[menuname:s, label:s] >> ->
         let menu = { menu_name = menuname; menu_label = label; menu_items = [] } in
            { info with info_menus = info.info_menus @ [menu] }
    | << menuitem[menuname:s, label:s, arg:s] >> ->
         (try
             menu_replace info menuname (fun menu ->
                   let command = { command_label = label; command_value = arg } in
                      { menu with menu_items = command :: menu.menu_items })
          with
             Not_found ->
                raise (RefineError ("Browser_resource.improve", StringTermError ("menu not declared: " ^ menuname, t))))
    | << style[text:s] >> ->
         { info with info_styles = text :: info.info_styles }
    | _ ->
         raise (RefineError ("Browser_resource.improve", StringTermError ("unrecognized term", t)))

(*
 * Extract the map.
 *)
let rec pp_print_id_list buf ids =
   match ids with
      [id] ->
         bprintf buf "'%s'" id
    | id :: ids ->
         bprintf buf "'%s', " id;
         pp_print_id_list buf ids
    | [] ->
         ()

let extract info =
   let { info_buttons = buttons;
         info_menus   = menus;
         info_styles  = styles
       } = info
   in

   (* Collect the button text *)
   let buf  = Buffer.create 32 in
   let buttoncommands = StringTable.empty in
   let buttoncommands =
      List.fold_left (fun buttoncommands { command_label = label; command_value = command } ->
            let sym = sprintf "id%d" (StringTable.cardinal buttoncommands) in
            let buttoncommands = StringTable.add buttoncommands sym command in
               bprintf buf "\t\t<td class=\"menulabel\" id=\"%s\">%s</td>\n" sym label;
               buttoncommands) buttoncommands (List.rev buttons)
   in

   (* Collect the menu text *)
   let menus, menulabels, menucommands =
      List.fold_left (fun (menus, menulabels, menucommands) menu ->
            let { menu_name = name; menu_label = label; menu_items = items } = menu in
            let items, menulabels, menucommands =
               bprintf buf "\t\t<td class=\"menulabel\" id=\"%s\">&#8227;%s</td>\n" name label;
               List.fold_left (fun (items, menulabels, menucommands)
                                   { command_label = label; command_value = command } ->
                     let sym = sprintf "id%d" (StringTable.cardinal menulabels) in
                     let items = sym :: items in
                     let menulabels = StringTable.add menulabels sym label in
                     let menucommands = StringTable.add menucommands sym command in
                        items, menulabels, menucommands) ([], menulabels, menucommands) (List.rev items)
            in
            let menus = StringTable.add menus name (List.rev items) in
               menus, menulabels, menucommands) (StringTable.empty, StringTable.empty, StringTable.empty) menus
   in

   (* Format the macros *)
   let macros_buf = Buffer.create 32 in
   let () =
      bprintf macros_buf "\tvar buttoncommands = new Array();\n";
      StringTable.iter (fun id s ->
            bprintf macros_buf "\tbuttoncommands['%s'] = \"%s\";\n" id (String.escaped s)) buttoncommands;

      bprintf macros_buf "\tvar menus = new Array();\n";
      StringTable.iter (fun name items ->
            bprintf macros_buf "\tmenus['%s'] = new Array(%a);\n" name pp_print_id_list items) menus;

      bprintf macros_buf "\tvar menulabels = new Array();\n";
      StringTable.iter (fun id s ->
            bprintf macros_buf "\tmenulabels['%s'] = \"%s\";\n" id (String.escaped s)) menulabels;

      bprintf macros_buf "\tvar menucommands = new Array();\n";
      StringTable.iter (fun id s ->
            bprintf macros_buf "\tmenucommands['%s'] = \"%s\";\n" id (String.escaped s)) menucommands
   in

   (* Collect the style sheets *)
   let style_buf = Buffer.create 32 in
   let () =
      List.iter (fun s ->
            Buffer.add_string style_buf s;
            Buffer.add_char style_buf '\n') (List.rev styles)
   in
      { browser_buf    = buf;
        browser_styles = style_buf;
        browser_macros = macros_buf
      }

(*
 * Add the directories.
 *)
let add_directories info dirs =
   try
      menu_replace info "dir" (fun menu ->
            let items =
               List.fold_left (fun items s ->
                     let item =
                        { command_label = s;
                          command_value = sprintf "Command('cd \"%s\"')" s
                        }
                     in
                        item :: items) menu.menu_items dirs
            in
               { menu with menu_items = items })
   with
      Not_found ->
         info

(*
 * Add the history.
 *)
let add_history info lines =
   try
      menu_replace info "history" (fun menu ->
            let items =
               List.fold_left (fun items s ->
                     let item =
                        { command_label = s;
                          command_value = sprintf "Prompt('%s')" s
                        }
                     in
                        item :: items) menu.menu_items lines
            in
               { menu with menu_items = items })
   with
      Not_found ->
         info

(*
 * Add the sessions.
 *)
let add_sessions state info ids =
   let id = state.browser_id in
      try
         menu_replace info "file" (fun menu ->
               let items =
                  List.fold_left (fun items (pid, cwd) ->
                        let label = sprintf "Session %d (%s)" pid cwd in
                        let label =
                           if pid = id then
                              "&#8227;" ^ label
                           else
                              label
                        in
                        let item =
                           { command_label = label;
                             command_value = sprintf "Session(%d)" pid
                           }
                        in
                           item :: items) menu.menu_items ids
               in
                  { menu with menu_items = items })
      with
         Not_found ->
            info

(*
 * Add files for editing.
 *)
let add_edit state info =
   let { browser_id = id;
         browser_files = files;
         browser_options = options
       } = state
   in
      try
         menu_replace info "edit" (fun menu ->
               let flag = String.contains options 'E' in
               let item =
                  if flag then
                     { command_label = "Use Browser editor";
                       command_value = "Command('clear_view_options \"E\"')"
                     }
                  else
                     { command_label = "Use External editor";
                       command_value = "Command('set_view_options \"E\"')"
                     }
               in
               let items = item :: menu.menu_items in
               let items =
                  List.fold_left (fun items file ->
                        let item =
                           { command_label = "Open " ^ Filename.basename file;
                             command_value = sprintf "Edit(%b, '/session/%d/edit/%s')" flag id (proxyedit_of_filename file)
                           }
                        in
                           item :: items) items files
               in
                  { menu with menu_items = items })
      with
         Not_found ->
            info

(*
 * Add the options.
 *)
let view_table =
   ['H', "",  "Show Term Handles",              "Hide Term Handles";
    'u', "",  "Show Only Unjustified Content",  "Show Default Set of Items";
    'f', "",  "Show All Formal Content",        "Hide All Formal Content";
    'R', "-", "Show Rules",                     "Hide Rules";
    'r', "-", "Show Rewrites",                  "Hide Rewrites";
    'i', "",  "Show All Informal Content",      "Hide All Informal Content";
    'D', "-", "Show Documentation",             "Hide Documentation";
    'p', "-", "Show Parents",                   "Hide Parents";
    'd', "-", "Show Display Forms",             "Hide Display Forms"]

let add_view info view =
   try
      menu_replace info "view" (fun menu ->
            let items =
               List.fold_left (fun items (flag, pre, show, hide) ->
                     let item =
                        if String.contains view flag then
                           { command_label = sprintf "%s %s" pre hide;
                             command_value = sprintf "Command('clear_view_options \"%c\"')" flag
                           }
                        else
                           { command_label = sprintf "%s %s" pre show;
                             command_value = sprintf "Command('set_view_options \"%c\"')" flag
                           }
                     in
                        item :: items) (List.rev menu.menu_items) view_table
            in
               { menu with menu_items = items })
   with
      Not_found ->
         info

let extract info state =
   let { browser_directories = directories;
         browser_history = history;
         browser_sessions = sessions;
         browser_options = options
       } = state
   in
   let info = add_directories info directories in
   let info = add_history info history in
   let info = add_sessions state info sessions in
   let info = add_edit state info in
   let info = add_view info options in
      extract info

(************************************************************************
 * Commandbar.
 *)
let commandbar_init =
   [<< menu["history", "History"] >>]

let default_commandbar = List.fold_left improve info_empty commandbar_init

let default_commandbar_info state =
   extract default_commandbar state

let commandbar_collection =
   Functional (**)
      { fp_empty = default_commandbar;
        fp_add   = improve;
        fp_retr  = extract
      }

let resource (term, browser_state -> browser_info) commandbar =
   commandbar_collection

(*
 * Default menubar resource.
 *)
let menubar_init =
   [<< menu["file", "File"] >>;
    << menuitem["file", "New Window", "NewWindow()"] >>;
    << menuitem["file", "New Session", "NewSession()"] >>;
    << menu["edit", "Edit"] >>;
    << menu["view", "View"] >>;
    << menu["dir", "Directory"] >>;
    << menuitem["dir", "Refresh", "Command('ls \"\"')"] >>;
    << menu["help", "Help"] >>;
    << menuitem["help", "MetaPRL Home", "URL('http://www.metaprl.org/')"] >>]

let default_menubar = List.fold_left improve info_empty menubar_init

let default_menubar_info state =
   extract default_menubar state

let menubar_collection =
   Functional (**)
      { fp_empty = default_menubar;
        fp_add   = improve;
        fp_retr  = extract
      }

let resource (term, browser_state -> browser_info) menubar =
   menubar_collection

let resource menubar +=
    << menuitem["file", "Save", "Command('save ()')"] >>

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
