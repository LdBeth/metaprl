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

open Browser_state
open Browser_edit

open Shell_sig

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
 * Menu element description.
 *)
type menu_item = term * (unit -> bool)

(*
 * The resource info.
 *)
type command =
   { command_label   : string;
     command_value   : string;
     command_enabled : unit -> bool
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
type pid = Lm_thread_shell.pid

type browser_state =
   { browser_directories : string list;
     browser_files       : string list;
     browser_history     : string list;
     browser_options     : string;
     browser_id          : pid;
     browser_sessions    : (pid * string) list
   }

(************************************************************************
 * Utilities.
 *)

let eflush out =
   output_char out '\n';
   flush out

let int_of_pid debug pid =
   let id, i = Lm_thread_shell.dest_pid pid in
      if id <> browser_id then
         eprintf "Browser_resource.int_of_pid: %s: non-browser id: %s.%d%t" debug id i eflush;
      i

let always_enabled () =
   true

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
let improve info (t, test) =
   match explode_term t with
      << button[label:s, arg:s] >> ->
         let command =
            { command_label   = label;
              command_value   = arg;
              command_enabled = test
            }
         in
            { info with info_buttons = command :: info.info_buttons }
    | << menu[menuname:s, label:s] >> ->
         let menu = { menu_name = menuname; menu_label = label; menu_items = [] } in
            { info with info_menus = info.info_menus @ [menu] }
    | << menuitem[menuname:s, label:s, arg:s] >> ->
         (try
             menu_replace info menuname (fun menu ->
                   let command =
                      { command_label   = label;
                        command_value   = arg;
                        command_enabled = test
                      }
                   in
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
      List.fold_left (fun buttoncommands command ->
            let { command_label = label;
                  command_value = command;
                  command_enabled = enabled
                } = command
            in
            if enabled () then
               let sym = sprintf "id%d" (StringTable.cardinal buttoncommands) in
               let buttoncommands = StringTable.add buttoncommands sym command in
                  bprintf buf "\t\t<td class=\"menulabel\" id=\"%s\">%s</td>\n" sym label;
                  buttoncommands
            else
               buttoncommands) buttoncommands (List.rev buttons)
   in

   (* Collect the menu text *)
   let menus, menulabels, menucommands =
      List.fold_left (fun (menus, menulabels, menucommands) menu ->
            let { menu_name = name; menu_label = label; menu_items = items } = menu in
            let items, menulabels, menucommands =
               bprintf buf "\t\t<td class=\"menulabel\" id=\"%s\">&#8227;%s</td>\n" name label;
               List.fold_left (fun (items, menulabels, menucommands) command ->
                     let { command_label = label;
                           command_value = command;
                           command_enabled = enabled
                         } = command
                     in
                     let sym = sprintf "id%d" (StringTable.cardinal menulabels) in
                     let items = sym :: items in
                     let menulabels = StringTable.add menulabels sym (label, enabled ()) in
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
               bprintf macros_buf "\tbuttoncommands['%s'] = '%s';\n" id (Lm_string_util.js_escaped s)) buttoncommands;

      bprintf macros_buf "\tvar menus = new Array();\n";
      StringTable.iter (fun name items ->
            bprintf macros_buf "\tmenus['%s'] = new Array(%a);\n" name pp_print_id_list items) menus;

      bprintf macros_buf "\tvar menuenabled = new Array();\n";
      bprintf macros_buf "\tvar menulabels = new Array();\n";
      StringTable.iter (fun id (s, enabled) ->
            bprintf macros_buf "\tmenuenabled['%s'] = %b;\n" id enabled;
            bprintf macros_buf "\tmenulabels['%s'] = '%s';\n" id (Lm_string_util.js_escaped s)) menulabels;

      bprintf macros_buf "\tvar menucommands = new Array();\n";
      StringTable.iter (fun id s ->
            bprintf macros_buf "\tmenucommands['%s'] = '%s';\n" id (Lm_string_util.js_escaped s)) menucommands
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
                        { command_label   = s;
                          command_value   = sprintf "Command('cd \"%s\"')" s;
                          command_enabled = always_enabled
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
                        { command_label   = s;
                          command_value   = sprintf "Prompt('%s')" s;
                          command_enabled = always_enabled
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
                        let pid_id, i = Lm_thread_shell.dest_pid pid in
                           if pid_id = browser_id then
                              let key =
                                 if pid = id then
                                    "&#8227;"
                                 else
                                    "-"
                              in
                              let label = sprintf "%s Session %d (%s)" key i cwd in
                              let item =
                                 { command_label   = label;
                                   command_value   = sprintf "Session(%d)" i;
                                   command_enabled = always_enabled
                                 }
                              in
                                 item :: items
                           else
                              items) menu.menu_items ids
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
                     { command_label   = "Use Browser editor";
                       command_value   = "Command('clear_view_options \"E\"')";
                       command_enabled = always_enabled
                     }
                  else
                     { command_label   = "Use External editor";
                       command_value   = "Command('set_view_options \"E\"')";
                       command_enabled = always_enabled
                     }
               in
               let divider =
                  { command_label      = "-";
                    command_value      = "";
                    command_enabled    = always_enabled
                  }
               in
               let items = divider :: item :: menu.menu_items in
               let items =
                  List.fold_left (fun items file ->
                        let item =
                           { command_label = "Open " ^ file;
                             command_value =
                                sprintf "Edit(%b, '/session/%d/edit/%s')"
                                   flag (int_of_pid "add_edit" id) (proxyedit_of_filename file);
                             command_enabled = always_enabled
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
    'u', "",  "Show Only Unjustified Content",  "Show Default Items";
    'f', "",  "Show All Formal Content",        "Hide All Formal Content";
    'R', "-", "Show Rules",                     "Hide Rules";
    'r', "-", "Show Rewrites",                  "Hide Rewrites";
    'i', "",  "Show All Informal Content",      "Hide All Informal Content";
    'D', "-", "Show Documentation",             "Hide Documentation";
    'p', "-", "Show Parents",                   "Hide Parents";
    'd', "-", "Show Display Forms",             "Hide Display Forms";
    'A', "",  "Show All Files",                 "Show Standard Files";
    'F', "-", "Show File Attributes",           "Hide File Attributes";
    'L', "-", "Show Line Numbers",              "Hide Line Numbers"]

let add_view info view =
   try
      menu_replace info "view" (fun menu ->
            let items =
               List.fold_left (fun items (flag, pre, show, hide) ->
                     let item =
                        if String.contains view flag then
                           { command_label   = sprintf "%s %s" pre hide;
                             command_value   = sprintf "Command('clear_view_options \"%c\"')" flag;
                             command_enabled = always_enabled
                           }
                        else
                           { command_label   = sprintf "%s %s" pre show;
                             command_value   = sprintf "Command('set_view_options \"%c\"')" flag;
                             command_enabled = always_enabled
                           }
                     in
                        item :: items) menu.menu_items view_table
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
   [<< menu["history", "History"] >>, always_enabled]

let default_commandbar = List.fold_left improve info_empty commandbar_init

let default_commandbar_info state =
   extract default_commandbar state

let commandbar_collection =
   Functional (**)
      { fp_empty = default_commandbar;
        fp_add   = improve;
        fp_retr  = extract
      }

let resource (menu_item, browser_state -> browser_info) commandbar =
   commandbar_collection

(*
 * Default menubar resource.
 *)
let refine_is_enabled () =
   Shell_command.is_enabled MethodRefine

let paste_is_enabled () =
   Shell_command.is_enabled (MethodPaste "clipboard")

let undo_is_enabled () =
   Shell_command.is_enabled MethodUndo

let redo_is_enabled () =
   Shell_command.is_enabled MethodRedo

let menubar_init =
   [<< menu["file", "File"] >>,                                            always_enabled;
    << menuitem["file", "Rebuild",    "Command('!omake')"] >>,             always_enabled;
    << menuitem["file", "Restart",    "Command('!restart')"] >>,           always_enabled;
    << menuitem["file", "CVS Update", "Command('!cvs \"update\"')"] >>,    always_enabled;
    << menuitem["file", "Save",       "Command('save ()')"] >>,            always_enabled;
    << menuitem["file", "Quit",       "Quit()"] >>,                        always_enabled;
    << menuitem["file", "-",          ""] >>,                              always_enabled;
    << menuitem["file", "New Window", "NewWindow()"] >>,                   always_enabled;
    << menuitem["file", "New Session", "NewSession()"] >>,                 always_enabled;
    << menu["edit", "Edit"] >>,                                            always_enabled;
    << menuitem["edit", "Copy", "Command('copy \"clipboard\"')"] >>,       refine_is_enabled;
    << menuitem["edit", "Paste", "Command('paste \"clipboard\"')"] >>,     paste_is_enabled;
    << menuitem["edit", "-", ""] >>,                                       always_enabled;
    << menuitem["edit", "Undo", "Command('undo ()')"] >>,                  undo_is_enabled;
    << menuitem["edit", "Redo", "Command('redo ()')"] >>,                  redo_is_enabled;
    << menuitem["edit", "-", ""] >>,                                       always_enabled;
    << menu["view", "View"] >>,                                            always_enabled;
    << menuitem["view", "View Session", "ShowContent()"] >>,               always_enabled;
    << menuitem["view", "View Command Output", "ShowSystem()"] >>,         always_enabled;
    << menuitem["view", "View Debug Messages", "ShowDebug()"] >>,          always_enabled;
    << menuitem["view", "-", ""] >>,                                       always_enabled;
    << menu["dir", "Directory"] >>,                                        always_enabled;
    << menuitem["dir", "Refresh", "Command('ls \"\"')"] >>,                always_enabled;
    << menu["help", "Help"] >>,                                            always_enabled;
    << menuitem["help", "MetaPRL Home", "URL('http://www.metaprl.org/')"] >>, always_enabled]

let default_menubar = List.fold_left improve info_empty menubar_init

let default_menubar_info state =
   extract default_menubar state

let menubar_collection =
   Functional (**)
      { fp_empty = default_menubar;
        fp_add   = improve;
        fp_retr  = extract
      }

let resource (menu_item, browser_state -> browser_info) menubar =
   menubar_collection

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
