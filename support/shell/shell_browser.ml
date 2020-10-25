(*
 * This is the WWW service.  The purpose of this module is
 * mainly to reverse the client/server roles.  MetaPRL waits
 * for a connection, then handles the input request.
 *
 * ----------------------------------------------------------------
 *
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/htmlman/default.html or visit http://metaprl.org/
 * for more information.
 *
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
 * jyh@cs.cornell.edu
 *)
open Lm_debug
open Lm_thread

open Format

open Http_simple
open Http_server_type

open Refiner.Refiner.RefineError

open Browser_copy
open Browser_edit
open Browser_state
open Browser_resource
open Shell_sig
open Shell_util
open Shell_syscall_sig

let _ =
   show_loading "Loading Shell Browser%t"

let debug_http =
   create_debug (**)
      { debug_name = "http";
        debug_description = "HTTP server operations";
        debug_value = false
      }

let _debug_lock =
   create_debug (**)
      { debug_name = "lock";
        debug_description = "Show locking operations";
        debug_value = false
      }

(*
 * We may start this as a web service.
 *)
let browser_port_name = Shell_state.browser_port_name
let browser_port      = Shell_state.browser_port

let browser_name      = Shell_state.browser_name
let browser_string    = Shell_state.browser_string

let challenge_name    = Shell_state.challenge_name
let challenge_string  = Shell_state.challenge_string

module ShellBrowser (Top : ShellTopSig) =
struct
   (*
    * Menu info contains a string to for the text, and
    * macros definitions.
    *)
   type menu =
      { menu_buffer : Buffer.t;
        menu_macros : Buffer.t
      }

   (*
    * Each session has its own info,
    * and it is assigned a unique process id.
    *)
   type pid = Lm_thread_shell.pid

   type session =
      { session_id                      : pid;

        (*
         * This is the current browsewr directory.
         * This is for updating the windows when
         * the directory changes.
         *)
        mutable session_cwd             : string;

        (*
         * Version numbers should be incremented whenever data is
         * invalidated.
         *)
        mutable session_menu_version    : int;
        mutable session_content_version : int;
        mutable session_message_version : int;
        mutable session_buttons_version : int;
        mutable session_rule_version    : int;

        (*
         * Directory and history info.
         *)
        mutable session_state           : browser_state option;

        (*
         * Browser info include menus and styles.
         *)
        mutable session_menubar_info    : browser_info option;
        mutable session_commandbar_info : browser_info option;

        (*
         * Menubar info is computed from browser_info.
         *)
        mutable session_menu            : menu option;

        (*
         * Buttons info is computed from browser info.
         *)
        mutable session_buttons         : menu option;

        (*
         * Styles are computed from browser_info.
         *)
        mutable session_styles          : Buffer.t option;

        (*
         * File editing.
         *)
        mutable session_edit            : string;
        mutable session_edit_flag       : bool;
        mutable session_edit_external   : bool;
        mutable session_edit_version    : int
      }

   (*
    * Keep track of the state.
    *)
   type state_shared =
      { mutable shared_challenge : string;
        mutable shared_response  : string;
        mutable shared_children  : int list
      }

   type state =
      { state_table     : BrowserTable.t;
        state_mp_dir    : string;
        state_password  : string;
        state_shared    : state_shared State.entry;
        state_challenge : string;
        state_response  : string;
        state_children  : int list;
        state_io        : Browser_syscall.t
      }

   (*
    * Types of URIs.
    *)
   type uri =
      InputURI    of string
    | ManualURI   of string
    | LoginURI    of string
    | UnknownURI  of string
    | FileURI     of string
    | ContentURI  of pid * string * bool  (* bool states whether the URL ends with a / *)
    | FrameURI    of pid * string
    | SessionURI  of pid
    | EditURI     of pid * string
    | CloneURI    of pid
    | OutputURI
    | WelcomeURI

   (*
    * Find the root job.
    *)
   let main_pid = Lm_thread_shell.create_or_find browser_id 1 Lm_thread_shell.VisibleJob

   let make_pid id =
      Lm_thread_shell.make_pid browser_id (int_of_string id)

   let dest_pid pid =
      let s, i = Lm_thread_shell.dest_pid pid in
         if s <> browser_id then
            eprintf "Shell_browser.dest_pid: bad process identifier %s@." s;
         i

   (*
    * Decode the URI.
    * This first part should be a session #.
    *)
   let rec decode_path state path uri =
      match path with
         [] ->
            WelcomeURI
       | "nocache" :: _ :: path ->
            decode_path state path uri
       | ["session"; id; "frameset"] ->
            (try FrameURI (make_pid id, "frameset") with
                Failure _
              | Not_found ->
                   eprintf "Bad session: %s@." id;
                   UnknownURI "/")
       | ["session"; id; "frameset"; "clone"] ->
            (try CloneURI (make_pid id) with
                Failure _
              | Not_found ->
                   eprintf "Bad session: %s@." id;
                   UnknownURI "/")
       | ["session"; id1; "frameset"; "session"; id2] ->
            (try
                (* Check id1, even if we don't use it *)
                let _ = make_pid id1 in
                   SessionURI (make_pid id2)
             with
                Failure _
              | Not_found ->
                   eprintf "Bad sessions: %s -> %s@." id1 id2;
                   UnknownURI "/")
       | ["session"; id; "frame"; frame] ->
            (try FrameURI (make_pid id, frame) with
                Failure _
              | Not_found ->
                   eprintf "Bad session: %s@." id;
                   UnknownURI "/")
       | "session" :: id :: "content" :: rest ->
            (try
                let pid = make_pid id in
                let dirname = Lm_string_util.prepend "/" rest in
                let is_dir = uri.[String.length uri - 1] = '/' in
                   ContentURI (pid, dirname, is_dir)
             with
                Failure _
              | Not_found ->
                   eprintf "Bad session: %s@." id;
                   UnknownURI (Lm_string_util.prepend "/" rest))
       | "session" :: id :: "edit" :: rest ->
            (try
                let pid = make_pid id in
                let name = String.concat "/" rest in
                   EditURI (pid, name)
             with
                Failure _
              | Not_found ->
                   eprintf "Bad session: %s@." id;
                   UnknownURI (Lm_string_util.prepend "/" rest))
       | ["output"] ->
            OutputURI
       | "inputs" :: rest ->
            InputURI (Lm_string_util.prepend "/" rest)
       | ["login"; key] ->
            LoginURI key
       | "file" :: name ->
            FileURI (String.concat "/" name)
       | "manual" :: rest ->
            ManualURI (Lm_string_util.prepend "/" rest)
       | ["favicon.ico"] ->
            ManualURI "/images/metaprl.ico"
       | uri ->
            UnknownURI (Lm_string_util.prepend "/" uri)

   let decode_uri state uri =
      decode_path state (decode_uri uri) uri

   (*
    * Make up a challenge.
    *)
   let new_challenge () =
      Lm_string_util.hexify (Digest.string (sprintf "%12.4f" (Unix.gettimeofday ())))

   (*
    * Copy the local field.
    *)
   let update_from_shared state shared =
      let { shared_challenge = challenge;
            shared_response  = response;
            shared_children  = children
          } = shared
      in
         { state with state_challenge = challenge;
                      state_response  = response;
                      state_children  = children
         }

   let update_state state =
      State.read state.state_shared (fun shared ->
            update_from_shared state shared)

   (*
    * Update the challenge and renew the response.
    * The environment may specify a challenge.
    *)
   let update_challenge state =
      let { state_password = password;
            state_shared = shared_entry;
            _
          } = state
      in
      let challenge =
         match !challenge_string with
            Some challenge ->
               challenge
          | None ->
               new_challenge ()
      in
      let response = Digest.string (password ^ challenge) in
      let response = Lm_string_util.hexify response in
      let () =
         if !debug_http then
            eprintf "@[<v 3>Password is: %s@ Challenge is: %s@ Response is: %s@]@." password challenge response
      in
         challenge_string := None;
         State.write shared_entry (fun shared ->
               shared.shared_challenge <- challenge;
               shared.shared_response <- response;
               update_from_shared state shared)

   (*
    * Load the browser on startup.
    *)
   let load_browser state =
      let state = update_challenge state in
      let io = state.state_io in
         Session_io.read_output (Browser_syscall.set_command io) (Browser_syscall.add_char io);
         state

   (*
    * Save the state in the environment so that we can re-use it
    * during a restart.
    *)
   let save_browser server state =
      let io = state.state_io in
      let command = Browser_syscall.command io in
      let contents = Browser_syscall.contents io in
         Session_io.write_output command contents;
         State.read state.state_shared (fun shared ->
               let port = Http_simple.save_http server in
                  Env_arg.putenv challenge_name shared.shared_challenge;
                  Env_arg.putenv browser_name "";
                  Env_arg.putenv browser_port_name (string_of_int port))

   (*
    * Get the content-type.
    *)
   let rec get_content_type header =
      match header with
         RequestContentType content_type :: _ ->
            content_type
       | _ :: rest ->
            get_content_type rest
       | [] ->
            raise Not_found

   let get_post_body header body =
      try parse_post_body (get_content_type header) body with
         Not_found ->
            []

   (*
    * Check the response from the header.
    *)
   let is_valid_response state header =
      let rec search header =
         match header with
            RequestCookies cookies :: rest ->
               if !debug_http then
                  List.iter (fun (name, v) ->
                        eprintf "Cookie[1]: %s=%s@." name v) cookies;
               (try List.assoc "MetaPRL.response" cookies with
                   Not_found ->
                      search rest)
          | _ :: rest ->
               search rest
          | [] ->
               raise Not_found
      in
      let flag =
         if !debug_http then
            eprintf "is_valid_response: begin@.";
         try
            let response = search header in
               if !debug_http then
                  eprintf "Response: %s, Valid response: %s, ok=%b@." (**)
                     response state.state_response
                     (response = state.state_response);
               response = state.state_response
         with
            Not_found ->
               false
      in
         if !debug_http then
            eprintf "is_valid_response: %b@." flag;
         flag

   (*
    * Get the window width (approximately in characters) from the header.
    * We assume some kind of character width.  Of course, this is quite wrong,
    * but the formatter needs a right margin in terms of characters.  Be
    * conservative, and choose to make the right margin smaller than it
    * might be.  We assume, on average, each char takes 10 pixels (bogus).
    *)
   let char_width = 10
   let default_width = 140

   let get_window_width header =
      let rec search header =
         match header with
            RequestCookies cookies :: rest ->
               if !debug_http then
                  List.iter (fun (name, v) ->
                        eprintf "Cookie[2]: %s=%s@." name v) cookies;
               (try int_of_string (List.assoc "MetaPRL.width" cookies) / char_width with
                   Not_found ->
                      search rest
                 | Failure s when s = "int_of_string" ->
                      search rest)
          | _ :: rest ->
               search rest
          | [] ->
               raise Not_found
      in
      let width =
         if !debug_http then
            eprintf "get_window_width: begin@.";
         try max default_width (search header) with
            Not_found ->
               (* Some default width *)
               default_width
      in
         if !debug_http then
            eprintf "get_window_width: %d@." width;
         width

   (*
    * Close an exit the process.
    *)
   let quit state =
      exit 0

   (************************************************************************
    * Session management.
    *)

   (*
    * Keep a handle to the current session.
    *)
   let session_entry =
      let default =
         let id = Lm_thread_shell.get_pid () in
            { session_id              = id;
              session_cwd             = "/";
              session_menu_version    = 0;
              session_content_version = 0;
              session_message_version = 0;
              session_buttons_version = 0;
              session_rule_version    = 0;
              session_state           = None;
              session_menubar_info    = None;
              session_commandbar_info = None;
              session_menu            = None;
              session_buttons         = None;
              session_styles          = None;
              session_edit            = "No File";
              session_edit_flag       = false;
              session_edit_external   = false;
              session_edit_version    = 0
            }
      in
      let fork session =
         let id = Lm_thread_shell.get_pid () in
         let cwd = Top.pwd () in
         let clone =
            { session_id              = id;
              session_cwd             = cwd;
              session_menu_version    = 0;
              session_content_version = 0;
              session_message_version = 0;
              session_buttons_version = 0;
              session_rule_version    = 0;
              session_state           = None;
              session_menubar_info    = None;
              session_commandbar_info = None;
              session_menu            = None;
              session_buttons         = None;
              session_styles          = None;
              session_edit            = "No File";
              session_edit_flag       = false;
              session_edit_external   = false;
              session_edit_version    = 0
            }
         in
            Top.set_dfmode "html";
            Top.set_dftype DisplayBrowser;
            Top.refresh ();
            session.session_menu_version <- succ session.session_menu_version;
            session.session_state        <- None;
            session.session_menubar_info <- None;
            session.session_menu         <- None;
            clone
      in
         State.private_val "Shell_browser.session" default fork

   (*
    * Lock the current session.
    *)
   let synchronize_pid pid f =
      Lm_thread_shell.with_pid pid (fun () ->
            State.write session_entry (fun session ->
                  f session)) ()

   (*
    * The session isn't actually needed,
    * but we require it for safety.
    *)
   let unsynchronize_session (session : session) f =
      State.unlock session_entry f

   (************************************************************************
    * Lazy state operations.
    *)

   (*
    * Invalidations need to keep track of dependencies:
    *    browser_info -> menu, buttons, styles
    *    buttons -> history
    *
    * Be sure to increment version numbers.
    *)
   let invalidate_session_directories () =
      let pids = Lm_thread_shell.get_pids () in
         List.iter (fun pid ->
               Lm_thread_shell.with_pid pid (fun () ->
                     State.write session_entry (fun session ->
                           let { session_menu_version = menu_version;
                                 session_buttons_version = buttons_version;
                                 session_content_version = content_version;
                                 _
                               } = session
                           in
                              session.session_menu_version    <- succ menu_version;
                              session.session_buttons_version <- succ buttons_version;
                              session.session_content_version <- succ content_version;

                              session.session_state           <- None;
                              session.session_menubar_info    <- None;
                              session.session_commandbar_info <- None;
                              session.session_menu            <- None;
                              session.session_buttons         <- None;
                              session.session_styles          <- None)) ()) pids

   let invalidate_directory session cwd =
      Session.add_directory cwd;
      Session.add_file (Top.filename ());
      session.session_cwd <- cwd;
      invalidate_session_directories ()

   let maybe_invalidate_directory session =
      let cwd = Top.pwd () in
         if cwd <> session.session_cwd then
            invalidate_directory session cwd

   (*
    * Invalidate the session.  This basically invalidates everything.
    *)
   let invalidate_eval session =
      let { session_buttons_version = buttons_version;
            session_message_version = message_version;
            session_content_version = content_version;
            session_menu_version    = menu_version;
            _
          } = session
      in
         session.session_message_version <- succ message_version;
         session.session_buttons_version <- succ buttons_version;
         session.session_content_version <- succ content_version;
         session.session_menu_version    <- succ menu_version;
         session.session_state           <- None;
         session.session_menubar_info    <- None;
         session.session_commandbar_info <- None;
         session.session_buttons         <- None;
         session.session_styles          <- None;
         session.session_menu            <- None;
         maybe_invalidate_directory session

   (*
    * Invalidate the directory if it changed.
    * If there was an error, then invalidate the message too.
    *)
   let invalidate_chdir session success =
      if success then
         maybe_invalidate_directory session
      else
         invalidate_eval session

   (*
    * Get the current state if possible.
    *)
   let get_session_state state session =
      let { session_state = info;
            session_id = id;
            _
          } = session
      in
         match info with
            Some state ->
               state
          | None ->
               let sessions =
                  List.fold_left (fun sessions pid ->
                        let session =
                           Lm_thread_shell.with_pid pid (fun () ->
                                 pid, Top.pwd ()) ()
                        in
                           session :: sessions) [] (Lm_thread_shell.get_pids ())
               in
               let info =
                  { browser_directories = Session.get_directories ();
                    browser_files       = Session.get_files ();
                    browser_history     = Session.get_history ();
                    browser_options     = string_of_ls_options (Session.get_view_options ());
                    browser_id          = id;
                    browser_sessions    = List.rev sessions
                  }
               in
                  session.session_state <- Some info;
                  info

   (*
    * Get the browser info, if possible.
    *)
   let get_menubar_info state session =
      let info = session.session_menubar_info in
         match info with
            Some info ->
               info
          | None ->
               let state = get_session_state state session in
               let f =
                  try get_menubar_resource (Top.get_resource ()) with
                     Not_found ->
                        default_menubar_info
               in
               let info = f state in
                  session.session_menubar_info <- Some info;
                  info

   let get_commandbar_info state session =
      let info = session.session_commandbar_info in
         match info with
            Some info ->
               info
          | None ->
               let state = get_session_state state session in
               let f =
                  try get_commandbar_resource (Top.get_resource ()) with
                     Not_found ->
                        default_commandbar_info
               in
               let info = f state in
                  session.session_commandbar_info <- Some info;
                  info

   (*
    * Get the menubar info.
    *)
   let get_menu state session =
      let info = session.session_menu in
         match info with
            Some info ->
               info
          | None ->
               let { browser_buf = buffer;
                     browser_macros = macros;
                     _
                   } = get_menubar_info state session
               in
               let info =
                  { menu_buffer = buffer;
                    menu_macros = macros
                  }
               in
                  session.session_menu <- Some info;
                  info

   (*
    * Get the buttons info.
    *)
   let get_buttons state session =
      let info = session.session_buttons in
         match info with
            Some info ->
               info
          | None ->
               let { browser_buf = buffer;
                     browser_macros = macros;
                     _
                   } = get_commandbar_info state session
               in
               let info =
                  { menu_buffer = buffer;
                    menu_macros = macros
                  }
               in
                  session.session_buttons <- Some info;
                  info

   (*
    * Get the current CSS.
    *)
   let get_styles state session =
      match session.session_styles with
         Some info ->
            info
       | None ->
            let { browser_styles = buffer; _ } = get_commandbar_info state session in
               session.session_styles <- Some buffer;
               buffer

   (************************************************************************
    * Page production.
    *)

   (*
    * Add the info to the table.
    *)
   let table_of_state state =
      let { state_table     = table;
            state_challenge = challenge;
            state_response  = response;
            _
          } = state
      in
      let table = BrowserTable.add_string table challenge_sym challenge in
      let table = BrowserTable.add_string table response_sym (String.sub response 8 8) in
         table

   (*
    * Print the edit info.
    *)
   let print_edit_info info buf =
      let { edit_new = isnew;
            edit_modified = ismodified;
            edit_point = point;
            _
          } = info
      in
         Printf.bprintf buf "\tvar editinfo = new Object();\n";
         Printf.bprintf buf "\teditinfo.isnew = %b;\n" isnew;
         Printf.bprintf buf "\teditinfo.ismodified = %b;\n" ismodified;
         Printf.bprintf buf "\teditinfo.point = %d;\n" point

   (*
    * Print the session state.
    *)
   let print_session server state session buf =
      let { session_id              = id;
            session_cwd             = cwd;
            session_menu_version    = menu_version;
            session_content_version = content_version;
            session_message_version = message_version;
            session_buttons_version = buttons_version;
            session_rule_version    = rule_version;
            session_edit            = edit;
            session_edit_flag       = edit_flag;
            session_edit_external   = edit_external;
            session_edit_version    = edit_version;
            _
          } = session
      in
      let id = dest_pid id in
      let io_version = Browser_syscall.version state.state_io in
      let js_cwd = Lm_string_util.js_escaped cwd in
      let js_edit = Lm_string_util.js_escaped edit in
         Printf.bprintf buf "\tvar session = new Array();\n";
         Printf.bprintf buf "\tsession['cwd']          = '%s';\n" js_cwd;
         Printf.bprintf buf "\tsession['location']     = '/session/%d/content%s/';\n" id js_cwd;
         Printf.bprintf buf "\tsession['menu']         = %d;\n" menu_version;
         Printf.bprintf buf "\tsession['content']      = %d;\n" content_version;
         Printf.bprintf buf "\tsession['message']      = %d;\n" message_version;
         Printf.bprintf buf "\tsession['buttons']      = %d;\n" buttons_version;
         Printf.bprintf buf "\tsession['rule']         = %d;\n" rule_version;
         Printf.bprintf buf "\tsession['io']           = %d;\n" io_version;
         Printf.bprintf buf "\tsession['file']         = '%s';\n" js_edit;
         Printf.bprintf buf "\tsession['editflag']     = %b;\n" edit_flag;
         Printf.bprintf buf "\tsession['edit']         = %d;\n" edit_version;
         Printf.bprintf buf "\tsession['external']     = %b;\n" edit_external;
         Printf.bprintf buf "\tsession['id']           = %d;\n" id

   (*
    * Print the history.
    *)
   let print_history state session buf =
      let info = get_session_state state session in
         Printf.bprintf buf "\tvar command_history = new Array();\n";
         ignore (List.fold_left (fun i s ->
                       Printf.bprintf buf "\tcommand_history[%d] = '%s';\n" i (Lm_string_util.js_escaped s);
                       succ i) 0 info.browser_history)


   (*
    * This is the default printer for each non-content pane.
    *)
   let print_page server state session out width frame =
      let { session_cwd             = cwd;
            session_menu            = menu;
            session_buttons         = buttons;
            session_styles          = styles;
            _
          } = session
      in
      let table = table_of_state state in

      (* Some helper functions *)
      let print_buffer flush buf =
         Buffer.add_buffer buf (flush state session)
      in
      let print_menu_buffer flush buf =
         let { menu_buffer = buf'; _ } = flush state session in
            Buffer.add_buffer buf buf'
      in
      let print_menu_macros flush buf =
         let { menu_macros = macros; _ } = flush state session in
            Buffer.add_buffer buf macros
      in
      let print_cwd buf =
         Buffer.add_string buf cwd
      in

      (* Current command *)
      let table = BrowserTable.add_string table command_sym        (Browser_syscall.command state.state_io) in

      (* General info *)
      let table = BrowserTable.add_string table title_sym          "MetaPRL" in
      let table = BrowserTable.add_fun    table location_sym       print_cwd in
      let table = BrowserTable.add_fun    table session_sym        (print_session server state session) in

      (* Menubar *)
      let table = BrowserTable.add_fun    table menu_sym           (print_menu_buffer get_menu) in
      let table = BrowserTable.add_fun    table menu_macros_sym    (print_menu_macros get_menu) in

      (* Content *)
      let table = BrowserTable.add_fun    table body_sym           (Session.format_main width) in

      (* Messages *)
      let table = BrowserTable.add_fun    table message_sym        (Session.format_message width) in

      (* Buttons *)
      let table = BrowserTable.add_fun    table buttons_sym        (print_menu_buffer get_buttons) in
      let table = BrowserTable.add_fun    table buttons_macros_sym (print_menu_macros get_buttons) in

      (* History *)
      let table = BrowserTable.add_fun    table history_sym        (print_history state session) in

      (* Styles *)
      let table = BrowserTable.add_fun    table style_sym          (print_buffer get_styles) in
         unsynchronize_session session (fun () ->
               print_translated_file_to_http out table (frame ^ ".html"))

   (*
    * Print the login page.
    *)
   let print_login_page out state session =
      let table = table_of_state state in
      let table = BrowserTable.add_string table title_sym "MetaPRL Login Page" in
      let table =
         let id =
            match session with
               Some { session_id = id; _ } ->
                  id
             | None ->
                  main_pid
         in
            BrowserTable.add_string table session_sym (string_of_int (dest_pid id))
      in
         match session with
            Some session ->
               unsynchronize_session session (fun () ->
                     print_translated_file_to_http out table "login.html")
          | None ->
               print_translated_file_to_http out table "login.html"

   (*
    * Print the startup page.  The start page gets the full response.
    *)
   let print_start_page out state =
      let table = table_of_state state in
      let table = BrowserTable.add_string table title_sym "MetaPRL Start Page" in
      let table = BrowserTable.add_string table response_sym state.state_response in
         print_translated_file_to_channel out table "start.html"

   (*
    * Print the login page.
    *)
   let print_access_granted_page out state =
      if !debug_http then
         eprintf "Access granted@.";
      let table = table_of_state state in
      let table = BrowserTable.add_string table title_sym "MetaPRL Access Granted" in
      let table = BrowserTable.add_string table response_sym state.state_response in
         print_translated_file_to_http out table "access.html"

   (*
    * Something failed.  Ask the browser to start over.
    *)
   let print_redisplay_page which_uri server state session outx =
      let { session_id = id;
            session_cwd = cwd;
            _
          } = session
      in
      let uri =
         sprintf "/session/%d/%s" (dest_pid id) (which_uri cwd)
      in
         if !debug_http then
            eprintf "Redirecting to %s@." uri;
         unsynchronize_session session (fun () ->
               print_redirect_page outx SeeOtherCode uri)

   (*
    * Ship out a local file.
    *)
   let print_internal_edit_page server state session filename outx =
      let filename = filename_of_proxyedit filename in
      let info = get_edit_info filename in
      let line_numbers = LsOptionSet.mem (Top.get_ls_options ()) LsLineNumbers in
      let table = table_of_state state in
      let table = BrowserTable.add_string table title_sym "Edit File" in
      let table = BrowserTable.add_string table file_sym filename in
      let table = BrowserTable.add_string table basename_sym (Filename.basename filename) in
      let table = BrowserTable.add_file   table content_sym info.edit_rootname line_numbers in
      let table = BrowserTable.add_string table response_sym state.state_response in
      let table = BrowserTable.add_fun    table session_sym (print_session server state session) in
      let table = BrowserTable.add_fun    table editinfo_sym (print_edit_info info) in
         print_translated_file_to_http outx table "edit.html"

   let print_external_edit_page server state session filename outx =
      let { http_host     = host;
            http_port     = port
          } = http_info server
      in
      let response = state.state_response in
      let buf = Buffer.create 100 in

      (* Strip the .proxyedit *)
      let filename = filename_of_proxyedit filename in
         try
            bprintf buf "host = \"%s\"\n" (String.escaped host);
            bprintf buf "port = %d\n" port;
            bprintf buf "name = \"file/%s\"\n" (String.escaped filename);
            bprintf buf "passwd = \"%s\"\n" (String.escaped response);
            bprintf buf "keyfile = \"%s\"\n" (String.escaped (string_of_abspath_file (Setup.client_pem())));
            bprintf buf "content = \"%s\"\n" (String.escaped (string_of_root_file filename));
            print_content_page outx OkCode "application/x-metaprl" buf
         with
            Not_found ->
               print_error_page outx NotFoundCode

   let print_edit_page server state session filename outx =
      let edit =
         if LsOptionSet.mem (Top.get_ls_options ()) LsExternalEditor then
            print_external_edit_page
         else
            print_internal_edit_page
      in
         session.session_edit_flag <- false;
         unsynchronize_session session (fun () ->
               edit server state session filename outx)

   (*
    * Print the edit done page.
    *)
   let print_edit_done_page out state =
      let table = table_of_state state in
      let table = BrowserTable.add_string table title_sym "MetaPRL Editing Done Page" in
      let table = BrowserTable.add_string table response_sym state.state_response in
         print_translated_file_to_http out table "edit-done.html"

   let print_file_page server state filename outx =
      print_metaprl_file_to_http outx filename

   (*
    * Welcome page.
    *)
   let print_welcome_page out state =
      let table = table_of_state state in
      let table = BrowserTable.add_string table title_sym "Welcome to MetaPRL" in
      let table = BrowserTable.add_string table version_sym Top.version in
         print_translated_file_to_http out table "welcome.html"

   (*
    * URL names.
    *)
   let content_uri cwd =
      "content" ^ cwd ^ "/"

   let rule_uri cwd =
      "frame/rule/"

   let frameset_uri cwd =
      "frameset"

   (*
    * Flush the session (ask the shell to display the current
    * directory).
    *)
   let flush state session =
      Session.synchronize Top.flush ()

   (************************************************************************
    * System calls.
    *)

   (*
    * Print the output page.
    *)
   let print_output_page state outx =
      let table = table_of_state state in
      let table = BrowserTable.add_string table title_sym "MetaPRL output window" in
      let table = BrowserTable.add_string table command_sym (Browser_syscall.command state.state_io) in
         print_translated_io_buffer_to_http outx table "output.html" state.state_io

   (*
    * Perform a command.
    *)
   let start_command session state command =
      if !debug_http then
         eprintf "Executing windowed command %s@." command;
      try Browser_syscall.start state.state_io command with
         Unix.Unix_error _ ->
            Lm_format.eprintf "%s: failed\n" command

   let start_inline_command session state command =
      if !debug_http then
         eprintf "Executing inline command %s@." command;
      start_command session state command;
      Session.add_command state.state_io

   (*
    * For the editor, just set the info in the session.
    *)
   let start_edit_command session state target =
      let { session_edit_version = edit_version; _ } = session in
      let flag = LsOptionSet.mem (Top.get_ls_options ()) LsExternalEditor in
         if Sys.file_exists (editname target) then
            Session.add_edit target;
         session.session_edit <- target;
         session.session_edit_flag <- true;
         session.session_edit_external <- flag;
         session.session_edit_version <- succ edit_version

   (*
    * Generic handler.
    * Some of the commands can be executed immediately.
    *)
   let handle_syscall server state outx command =
      State.write session_entry (fun session ->
            Top.backup_all ();
            match command with
               SyscallRestart ->
                  Shell_current.flush ();
                  save_browser server state;
                  (match outx with
                      Some outx ->
                         print_page server state session outx 100 "reload";
                         Http_simple.Output.flush outx
                    | None ->
                         ());
                  let () =
                     try Unix.execv Sys.argv.(0) Sys.argv with
                        Unix.Unix_error _ ->
                           ()
                  in
                     Lm_format.eprintf "System restart failed@."
             | SyscallOMake target ->
                  start_command session state (sprintf "omake %s" target)
             | SyscallSVN (cwd, command) ->
                  start_command session state (sprintf "cd %s && svn %s" cwd command)
             | SyscallEdit (_, target) ->
                  start_edit_command session state target
             | SyscallShell s ->
                  start_inline_command session state s);
      0

   (************************************************************************
    * Commands.
    *)

   (*
    * Evaluate a command.
    * Send it to the shell, and get the result.
    *)
   let eval server state session outx command =
      Shell_syscall.set_syscall_handler (handle_syscall server state (Some outx));
      Session.add_prompt command;
      unsynchronize_session session (fun () ->
            Session.synchronize Top.eval (command ^ ";;"));
      Shell_syscall.set_syscall_handler (handle_syscall server state None);
      invalidate_eval session

   (*
    * Set the current directory.
    *)
   let chdir state session dir =
      if !debug_http then
         eprintf "Changing directory to %s@." dir;
      let success =
         unsynchronize_session session (fun () ->
               Session.synchronize Top.chdir dir)
      in
         invalidate_chdir session success;
         success

   (*
    * Check if a command is pasted.  Just check for the regular expression %%[A-z]*%%
    *)
   let pasted_regexp = Str.regexp "%%[A-Za-z0-9]*%%"

   let get_pasted_command server state session command =
      try
         let index = Str.search_forward pasted_regexp command 0 in
         let name = Str.matched_string command in
         let command = String.sub command 0 index in
         let len = String.length name in
            if len < 4 then
               None
            else
               let id = String.sub name 2 (len - 4) in
                  Some (command, Session.get_term id)
      with
         Not_found ->
            None

   (*
    * Paste the term.
    *)
   let paste server state session outx command term =
      let db = Dform.change_mode (Top.get_dforms ()).df_base "src" in
      let s = Dform.string_of_term db term in
      let command = Printf.sprintf "%s << %s >>" command s in
      let state =
         { state with state_table = BrowserTable.add_string state.state_table rulebox_sym (String.escaped command) }
      in
         print_page server state session outx 140 "rule"

   (************************************************************************
    * Server operations.
    *)

   (*
    * Handle a get command by changing directories as specified.
    *
    * This is untrusted, so each case should check if access is
    * granted.
    *)
   let get server state outx uri header =
      if !debug_http then
         eprintf "Get: %s@." uri;

      (*
       * Catch references to direct files.
       *)
      match decode_uri state uri with
         InputURI filename ->
            print_raw_file_to_http outx filename
       | ManualURI filename ->
            begin match Setup.doc_dir () with
               Some dir ->
                  print_metaprl_file_to_http outx ( dir ^ filename )
             | None ->
                  print_redirect_page outx MovedPermCode ( "http://metaprl.org/" ^ filename )
            end
       | WelcomeURI ->
            print_welcome_page outx state
       | LoginURI key ->
            if key = state.state_response then
               print_access_granted_page outx state
            else
               print_login_page outx state None
       | FrameURI (pid, frame) ->
            synchronize_pid pid (fun session ->
                  if is_valid_response state header then
                     let width = get_window_width header in
                        print_page server state session outx width frame
                  else
                     print_login_page outx state (Some session))
       | CloneURI pid ->
            synchronize_pid pid (fun session ->
                  if is_valid_response state header then
                     let pid = Lm_thread_shell.create browser_id Lm_thread_shell.VisibleJob in
                        synchronize_pid pid (fun session ->
                              print_redisplay_page frameset_uri server state session outx)
                  else
                     print_login_page outx state (Some session))
       | SessionURI pid ->
            synchronize_pid pid (fun session ->
                  if is_valid_response state header then
                     print_redisplay_page frameset_uri server state session outx
                  else
                     print_login_page outx state (Some session))
       | ContentURI (pid, dirname, is_dir) ->
            synchronize_pid pid (fun session ->
                  if is_valid_response state header then
                     let width = get_window_width header in
                        (* To ensure relative links are correct, all URLs must end with a slash *)
                        if chdir state session dirname && is_dir && (Top.pwd () = dirname) then
                           begin
                              flush state session;
                              print_page server state session outx width "content"
                           end
                        else
                           begin
                              (* Invalid directory or directory change failed *)
                              print_redisplay_page content_uri server state session outx
                           end
                  else
                     print_login_page outx state (Some session))
       | EditURI (pid, filename) ->
            synchronize_pid pid (fun session ->
                  if is_valid_response state header then
                     print_edit_page server state session filename outx
                  else
                     print_login_page outx state (Some session))
       | FileURI filename ->
            if is_valid_response state header then
               print_file_page server state filename outx
            else
               print_error_page outx ForbiddenCode
       | OutputURI ->
            if is_valid_response state header then
               print_output_page state outx
            else
               print_login_page outx state None
       | UnknownURI dirname ->
            print_error_page outx NotFoundCode

   (*
    * Handle a put command by updating the file as specified.
    *)
   let put server state outx uri header body =
      if !debug_http then
         eprintf "Put: %s@." uri;
      match decode_uri state uri with
         FileURI filename ->
            if save_file filename false 0 body then
               print_edit_done_page outx state
            else
               print_error_page outx NotFoundCode
       | InputURI _
       | ManualURI _
       | LoginURI _
       | UnknownURI _
       | ContentURI _
       | FrameURI _
       | CloneURI _
       | OutputURI
       | SessionURI _
       | EditURI _
       | WelcomeURI ->
            print_error_page outx BadRequestCode;
            eprintf "Shell_simple_http: bad PUT command@."

   (*
    * Handle a post command.  This means to submit the text to MetaPRL.
    *)
   let post server state outx inx uri header body =
      let body = get_post_body header body in
      let () =
         if !debug_http then
            List.iter (fun (name, text) ->
                  eprintf "Post: %s, \"%s\"@." name (String.escaped text)) body
      in

      (* Precedence *)
      let command =
         try Some (List.assoc "command" body) with
            Not_found ->
               None
      in
      let content =
         try Some (List.assoc "content" body) with
            Not_found ->
               None
      in
         match decode_uri state uri with
            FrameURI (pid, "rule") ->
               synchronize_pid pid (fun session ->
                     match command with
                        Some command ->
                           if !debug_http then
                              eprintf "Command: \"%s\"@." (String.escaped command);

                           (* If the command contains a pasted element, then paste it *)
                           (match get_pasted_command server state session command with
                               Some (command, term) ->
                                  paste server state session outx command term
                             | None ->
                                  try
                                     eval server state session outx command;
                                     print_redisplay_page rule_uri server state session outx
                                  with
                                     End_of_file ->
                                        quit state)
                      | None ->
                           print_redisplay_page rule_uri server state session outx)
          | EditURI (pid, filename) ->
               let edit_uri _ =
                  sprintf "edit/%s" filename
               in
                  synchronize_pid pid (fun session ->
                        match content with
                           Some content ->
                              let point =
                                 try int_of_string (List.assoc "point" body) with
                                    Failure _
                                  | Not_found ->
                                    0
                              in
                              let action =
                                 let code =
                                    try List.assoc "type" body with
                                       Not_found ->
                                          "save"
                                 in
                                    match code with
                                       "save" ->
                                          save_file
                                     | "cancel" ->
                                          cancel_file
                                     | _ ->
                                          backup_file
                              in
                              let line_flag = LsOptionSet.mem (Top.get_ls_options ()) LsLineNumbers in
                              let filename = filename_of_proxyedit filename in
                                 if action filename line_flag point content then
                                    print_redisplay_page edit_uri server state session outx
                                 else
                                    print_error_page outx NotFoundCode
                         | None ->
                              print_error_page outx BadRequestCode)
          | InputURI _
          | ManualURI _
          | LoginURI _
          | UnknownURI _
          | ContentURI _
          | FrameURI _
          | CloneURI _
          | OutputURI
          | SessionURI _
          | FileURI _
          | WelcomeURI ->
               print_error_page outx BadRequestCode;
               eprintf "Shell_simple_http: bad POST command@."

   (*
    * Poll-wait for child processes.
    *
    * BUG JYH: this is kind of ugly.  When using threads,
    * it is probably best to fork a thread to do an explicit wait.
    * We don't want to catch SIGCHLD since we'll have
    * trouble on Windows.  See the comment on start_browser
    * below.
    *)
   let http_wait server state =
      if state.state_children <> [] then
         State.write state.state_shared (fun shared ->
               let children =
                  List.fold_left (fun children pid ->
                        try
                           (* Wait for the child *)
                           let pid', _ = Unix.waitpid [Unix.WNOHANG] pid in
                              if pid' = pid then
                                 children
                              else
                                 pid :: children
                        with
                           Unix.Unix_error _ ->
                              (* Assume the child is dead *)
                              children) [] shared.shared_children
               in
                  shared.shared_children <- children)

   (*
    * Handle a connection.
    * We handle only get and post for now.
    *)
   let http_connect server state outx inx args header body =
      try
         let state =
            http_wait server state;
            update_state state
         in
         let () =
            match args with
               "get" :: uri :: _ ->
                  get server state outx uri header
             | "put" :: uri :: _ ->
                  if is_valid_response state header then
                     put server state outx uri header body
                  else
                     print_login_page outx state None
             | "post" :: uri :: _ ->
                  if is_valid_response state header then
                     post server state outx inx uri header body
                  else
                     print_login_page outx state None
             | command :: _ ->
                  print_error_page outx NotImplementedCode;
                  eprintf "Shell_simple_http: unknown command: %s@." command
             | [] ->
                  print_error_page outx BadRequestCode;
                  eprintf "Shell_simple_http: null command@."
         in
            if !debug_http then
               eprintf "Shell_browser.http_connect: connection finished@.";
            state
      with
         RefineError(_, ToploopIgnoreError)
       | Refine_exn.ToploopIgnoreExn _ ->
            state

   (*
    * Start a browser if possible.
    *
    * BUG JYH: this works with Mozilla/Linux, but I'm not at all sure
    * how well it will work on other systems.
    *
    * BUG n8: It doesn't work so well on Safari/OS X, where you want
    * to use "open -a safari" as your browser command.
    *
    * BUG JYH: took out the waitpid.  This will allow the browser
    * not to exit, but it usually leaves around a zombie process.
    * For now, poll for exit in the connect function.
    *)
   let start_browser server state browser url =
      let argv = Lm_string_util.split " \t" browser @ [url] in
      let argv = Array.of_list argv in
         try
            let pid = Unix.create_process browser argv Unix.stdin Unix.stdout Unix.stderr in
               State.write state.state_shared (fun shared ->
                     shared.shared_children <- pid :: shared.shared_children)
         with
            Unix.Unix_error _ ->
               eprintf "Shell_browser: could not start browser %s@." browser

   (*
    * Start the server.
    *)
   let http_start server state =
      let { http_host     = host;
            http_port     = port
          } = http_info server
      in
      let table = state.state_table in
      let table = BrowserTable.add_string table host_sym host in
      let table = BrowserTable.add_string table port_sym (string_of_int port) in
      let table = BrowserTable.add_string table protocol_sym Shell_state.protocol_name in
      let state = { state with state_table = table } in
      let metaprl_dir = state.state_mp_dir in

      (* Create the start file *)
      let filename = Filename.concat metaprl_dir "start.html" in
      let fd = Unix.openfile filename [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] 0o600 in
      let () =
         try Unix.fchmod fd 0o600 with
            Unix.Unix_error _
          | Invalid_argument _ ->
               ()
      in
      let out = Unix.out_channel_of_descr fd in
      let file_url = sprintf "file://%s" filename in
      let http_url = sprintf "%s://%s:%d/" Shell_state.protocol_name host port in
         (* Start page *)
         print_start_page out state;
         Stdlib.flush out;
         Unix.close fd;

         (* Start the browser if requested *)
         let message1, message2 =
            match !browser_string with
               Some browser when browser <> "" ->
                  start_browser server state browser file_url;
                  "Browsing service started and your browser \"" ^ browser ^ "\" should start up momentarily.",
                  "If the browser fails to start automatically, manually point"
             | _ ->
                  "Browsing service started.", "Point"
         in

         (* Tell the user *)
         eprintf "@[<v 0>@ %s@ @[<v 3>%s your browser to the following URL:@ %s (will log you in automatically)@]@ \
@[<v 3>Or use the following URL:@ %s (will require you to log in)@]@]@." message1 message2 file_url http_url;

         (* Handle commands *)
         Shell_syscall.set_syscall_handler (handle_syscall server state None);

         (* Return the new translation state *)
         state

   (*
    * Setup the browser.
    *)
   let init_password () =
      (* Get, or create the password *)
      let passwd = Filename.concat (Setup.home ()) "passwd" in
         if Sys.file_exists passwd then
            begin
               let inx =
                  try open_in passwd with
                     Sys_error _ ->
                        raise (Invalid_argument ("The password file \"" ^ passwd ^ "\" exists, but is protected"))
               in
               let password =
                  try input_line inx with
                     End_of_file ->
                        close_in inx;
                        raise (Invalid_argument ("The password file \"" ^ passwd ^ "\" is empty"))
               in
                  close_in inx;
                  Lm_string_util.trim password

            end
         else
            begin
               (* Create a new password *)
               let fd = Unix.openfile passwd [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] 0o600 in
               let () =
                  try Unix.fchmod fd 0o600 with
                     Unix.Unix_error _
                   | Invalid_argument _ ->
                        ()
               in
               let out = Unix.out_channel_of_descr fd in
               let password = new_challenge () in
                  output_string out password;
                  close_out out;

                  eprintf "@[<v 3>\
*** Note ***@ \
*** This is the first time you are using the browser service.@ \
*** A new password has been created for you in the file %s@ \
*** You will need this password to connect to MetaPRL from your browser.@ \
*** You may change this password to something you can remember if you like.@ @]@." passwd;

                  password
            end

   (*
    * Start the web server.
    *)
   let main () =
      if not (Shell_state.cli_flag ()) then
         let password = init_password () in
         let shared =
            { shared_challenge  = "unknown";
              shared_response   = "unknown";
              shared_children   = []
            }
         in
         let shared = State.shared_val "Shell_browser.shared" shared in
         let state =
            { state_table     = BrowserTable.empty;
              state_mp_dir    = Setup.home ();
              state_password  = password;
              state_shared    = shared;
              state_challenge = "unknown";
              state_response  = "unknown";
              state_children  = [];
              state_io        = Browser_syscall.create ()
            }
         in
         let state = load_browser state in
            Lm_thread_shell.with_pid main_pid (fun () ->
                  Top.init ();
                  Top.set_dfmode "html";
                  Top.set_dftype DisplayBrowser;
                  Top.refresh ();
                  State.write session_entry maybe_invalidate_directory) ();
            let serve () =
               let port = !browser_port in
                  if port = 0 then
                     let rec serve port =
                        try
                           serve_http http_start http_connect state port
                        with
                           Unix.Unix_error(Unix.EADDRINUSE, _, _) ->
                              serve (port + 1)
                     in
                        serve 60000
                  else
                     serve_http http_start http_connect state port
            in
            begin try
               Filter_exn.print_exn Dform.null_base (Some "Shell_browser.main: Uncaught exception from serve_http: ") serve ()
            with _ ->
               exit 1
            end;
            eprintf "Browser service finished@."
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "nl"
 * End:
 * -*-
 *)
