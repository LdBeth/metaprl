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
open Lm_debug
open Lm_symbol
open Lm_string_set
open Lm_int_set

open Format

open Http_simple
open Http_server_type

open Browser_copy
open Browser_resource
open Shell_sig
open Package_info

let _ =
   show_loading "Loading Shell HTTP%t"

let debug_http =
   create_debug (**)
      { debug_name = "http";
        debug_description = "HTTP server operations";
        debug_value = false
      }
(*
 * We may start this as a web service.
 *)
let browser_flag = Env_arg.bool "browser" false "start a browser service" Env_arg.set_bool_bool
let browser_port = Env_arg.int "port" None "start browser services on this port" Env_arg.set_int_option_int
let browser_string = Env_arg.string "BROWSER_COMMAND" None "browser to start on startup" Env_arg.set_string_option_string

module ShellBrowser (Shell : ShellSig) =
struct
   (*
    * Menu info contains a string to for the text, and
    * macros definitions.
    *)
   type menu =
      { menu_buffer : Buffer.t;
        menu_macros : string StringTable.t
      }

   (*
    * Each session has its own info.
    *)
   type session =
      { session_id                      : int;
        session_buffer                  : Browser_display_term.t;
        session_shell                   : Shell.t;

        (*
         * If the directory is changed, invalidate:
         *    browser_info, menu, buttons, style
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
         * Browser info include menus and styles.
         *)
        mutable session_browser_info    : browser_info option;

        (*
         * Menubar info is computed from browser_info.
         *)
        mutable session_menu            : menu option;

        (*
         * Buttons info is computed from browser info.
         *)
        mutable session_buttons         : menu option;

        (*
         * History is computed from the message window
         * (in Browser_display_term), and session_buttons.
         *)
        mutable session_history         : menu option;

        (*
         * Styles are computed from browser_info.
         *)
        mutable session_styles          : Buffer.t option
      }

   (*
    * Keep track of the state.
    *)
   type state =
      { state_table     : BrowserTable.t;
        state_mp_dir    : string;
        state_password  : string;

        (* Validation *)
        mutable state_challenge : string;
        mutable state_response  : string;

        (* Active sessions *)
        mutable state_sessions  : session IntTable.t
      }

   (*
    * Types of URIs.
    *)
   type uri =
      ContentURI of session * string * bool  (* bool states whether the URL ends with a / *)
    | FrameURI of session * string
    | InputURI of string
    | LoginURI of string
    | UnknownURI of string

   (*
    * Decode the URI.
    * This first part should be a session #.
    *)
   let decode_uri state uri =
      match decode_uri uri with
         ["session"; id]
       | ["session"; id; "frameset"] ->
            (try
                let session = IntTable.find state.state_sessions (int_of_string id) in
                   FrameURI (session, "frameset")
             with
                Not_found
              | Failure _ ->
                   eprintf "Bad session: %s@." id;
                   UnknownURI "/")
       | ["session"; id; "frame"; frame] ->
            (try
                let session = IntTable.find state.state_sessions (int_of_string id) in
                   FrameURI (session, frame)
             with
                Not_found
              | Failure _ ->
                   eprintf "Bad session: %s@." id;
                   UnknownURI "/")
       | "session" :: id :: "content" :: rest ->
            (try
                let session = IntTable.find state.state_sessions (int_of_string id) in
                let dirname = Lm_string_util.prepend "/" rest in
                let is_dir = uri.[String.length uri - 1] = '/' in
                   ContentURI (session, dirname, is_dir)
             with
                Not_found ->
                   eprintf "Bad session: %s@." id;
                   UnknownURI (Lm_string_util.prepend "/" rest))
       | "inputs" :: rest ->
            InputURI (Lm_string_util.prepend "/" rest)
       | ["login"; key] ->
            LoginURI key
       | uri ->
            UnknownURI (Lm_string_util.prepend "/" uri)

   (*
    * Make up a challenge.
    *)
   let new_challenge () =
      Lm_string_util.hexify (Digest.string (sprintf "%12.4f" (Unix.gettimeofday ())))

   (*
    * Update the challenge and renew the response.
    *)
   let update_challenge state =
      let { state_password = password } = state in
      let challenge = new_challenge () in
      let response = Digest.string (password ^ challenge) in
      let response = Lm_string_util.hexify response in
         if !debug_http then
            eprintf "@[<v 3>Password is: %s@ Challenge is: %s@ Response is: %s@]@." password challenge response;
         { state with state_challenge = challenge;
                      state_response = response
         }

   (*
    * Check the response from the header.
    *)
   let is_valid_response state header =
      let rec search header =
         match header with
            RequestCookies cookies :: rest ->
               if !debug_http then
                  List.iter (fun (name, v) ->
                        eprintf "Cookie: %s=%s@." name v) cookies;
               (try List.assoc "MetaPRL.response" cookies with
                   Not_found ->
                      search rest)
          | _ :: rest ->
               search rest
          | [] ->
               raise Not_found
      in
         try
            let response = search header in
               if !debug_http then
                  eprintf "Response: %s, Valid response: %s, ok=%b@." response state.state_response (response = state.state_response);
               response = state.state_response
         with
            Not_found ->
               false

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
                        eprintf "Cookie: %s=%s@." name v) cookies;
               (try int_of_string (List.assoc "MetaPRL.width" cookies) / char_width with
                   Not_found
                 | Failure "int_of_string" ->
                      search rest)
          | _ :: rest ->
               search rest
          | [] ->
               raise Not_found
      in
         try max default_width (search header) with
            Not_found ->
               (* Some default width *)
               default_width

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
   let invalidate_directory session cwd =
      let { session_menu_version = menu_version;
            session_buttons_version = buttons_version;
            session_content_version = content_version
          } = session
      in
         session.session_cwd <- cwd;

         session.session_menu_version    <- succ menu_version;
         session.session_buttons_version <- succ buttons_version;
         session.session_content_version <- succ content_version;

         session.session_browser_info <- None;
         session.session_menu    <- None;
         session.session_buttons <- None;
         session.session_history <- None;
         session.session_styles  <- None

   let maybe_invalidate_directory session =
      let cwd = Shell.pwd session.session_shell in
         if cwd <> session.session_cwd then
            invalidate_directory session cwd

   (*
    * Invalidate the session.  This means: assume there has been output,
    * and invalidate the directory.
    *)
   let invalidate_eval session =
      let { session_buttons_version = buttons_version;
            session_message_version = message_version;
            session_content_version = content_version
          } = session
      in
         session.session_message_version <- succ message_version;
         session.session_buttons_version <- succ buttons_version;
         session.session_content_version <- succ content_version;
         session.session_history <- None;
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
    * Get the browser info, if possible.
    *)
   let get_browser_info state session =
      let info = session.session_browser_info in
         match info with
            Some _ ->
               info
          | None ->
               (try
                   let info = Some (get_browser_resource (Shell.get_resource session.session_shell)) in
                      session.session_browser_info <- info;
                      info
                with
                   Not_found ->
                      None)

   (*
    * Get the menubar info.
    *)
   let get_menu state session =
      let info = session.session_menu in
         match info with
            Some _ ->
               info
          | None ->
               (match get_browser_info state session with
                   Some { browser_menubar = buffer;
                          browser_menu_macros = macros
                   } ->
                      let info =
                         Some { menu_buffer = buffer;
                                menu_macros = macros
                         }
                      in
                         session.session_menu <- info;
                         info
                 | None ->
                      None)

   (*
    * Get the buttons info.
    *)
   let get_buttons state session =
      let info = session.session_buttons in
         match info with
            Some _ ->
               info
          | None ->
               (match get_browser_info state session with
                   Some { browser_buttons = buffer;
                          browser_buttons_macros = macros
                   } ->
                      let info =
                         Some { menu_buffer = buffer;
                                menu_macros = macros
                         }
                      in
                         session.session_buttons <- info;
                         info
                 | None ->
                      None)

   (*
    * Get the current CSS.
    *)
   let get_styles state session =
      let info = session.session_styles in
         match session.session_styles with
            Some _ ->
               info
          | None ->
               (match get_browser_info state session with
                   Some { browser_styles = buffer } ->
                      let buffer = Some buffer in
                         session.session_styles <- buffer;
                         buffer
                 | None ->
                      None)

   (*
    * Get the current history.
    *)
   let get_history state session =
      let { session_history = history;
            session_buffer = buffer
          } = session
      in
         match history with
            Some _ ->
               history
          | None ->
               let macros =
                  match get_buttons state session with
                     Some { menu_macros = macros } ->
                        macros
                   | None ->
                        StringTable.empty
               in
               let buffer, macros = Browser_display_term.get_history buffer macros in
               let info =
                  Some { menu_buffer = buffer;
                         menu_macros = macros
                  }
               in
                  session.session_history <- info;
                  info

   (************************************************************************
    * Page production.
    *)

   (*
    * Add the info to the table.
    *)
   let table_of_state state =
      let { state_table     = table;
            state_challenge = challenge;
            state_response  = response
          } = state
      in
      let table = BrowserTable.add_string table challenge_sym challenge in
      let table = BrowserTable.add_string table response_sym (String.sub response 8 8) in
         table

   (*
    * Print the session state.
    *)
   let print_session server session buf =
      let { http_host = host;
            http_port = port
          } = http_info server
      in
      let { session_id              = id;
            session_cwd             = cwd;
            session_menu_version    = menu_version;
            session_content_version = content_version;
            session_message_version = message_version;
            session_buttons_version = buttons_version;
            session_rule_version    = rule_version
          } = session
      in
         Printf.bprintf buf "\tsession['location'] = 'http://%s:%d/session/%d/content%s/';\n" host port id cwd;
         Printf.bprintf buf "\tsession['menu']     = %d;\n" menu_version;
         Printf.bprintf buf "\tsession['content']  = %d;\n" content_version;
         Printf.bprintf buf "\tsession['message']  = %d;\n" message_version;
         Printf.bprintf buf "\tsession['buttons']  = %d;\n" buttons_version;
         Printf.bprintf buf "\tsession['rule']     = %d;\n" rule_version

   (*
    * Print the macros set.
    *)
   let print_macros macros buf =
      StringTable.iter (fun s v ->
            Printf.bprintf buf "\tmacros['%s'] = '%s';\n" s v) macros

   (*
    * This is the default printer for each non-content pane.
    *)
   let print_page server state session out width frame =
      let { session_buffer          = info;
            session_cwd             = cwd;
            session_menu            = menu;
            session_buttons         = buttons;
            session_history         = history;
            session_styles          = styles
          } = session
      in
      let table = table_of_state state in

      (* Some helper functions *)
      let print_buffer flush buf =
         match flush state session with
            Some buf' ->
               Buffer.add_buffer buf buf'
          | None ->
               ()
      in
      let print_menu_buffer flush buf =
         match flush state session with
            Some { menu_buffer = buf' } ->
               Buffer.add_buffer buf buf'
          | None ->
               ()
      in
      let print_menu_macros flush buf =
         match flush state session with
            Some { menu_macros = macros } ->
               print_macros macros buf
          | None ->
               ()
      in

      (* General info *)
      let table = BrowserTable.add_string table title_sym          "MetaPRL" in
      let table = BrowserTable.add_string table location_sym       cwd in
      let table = BrowserTable.add_fun    table session_sym        (print_session server session) in

      (* Menubar *)
      let table = BrowserTable.add_fun    table menu_sym           (print_menu_buffer get_menu) in
      let table = BrowserTable.add_fun    table menu_macros_sym    (print_menu_macros get_menu) in

      (* Content *)
      let table = BrowserTable.add_fun    table body_sym           (Browser_display_term.format_main info width) in

      (* Messages *)
      let table = BrowserTable.add_fun    table message_sym        (Browser_display_term.format_message info width) in

      (* Buttons *)
      let table = BrowserTable.add_fun    table buttons_sym        (print_menu_buffer get_buttons) in
      let table = BrowserTable.add_fun    table history_sym        (print_menu_buffer get_history) in
      let table = BrowserTable.add_fun    table buttons_macros_sym (print_menu_macros get_history) in

      (* Styles *)
      let table = BrowserTable.add_fun    table style_sym          (print_buffer get_styles) in

         (* Now print the file *)
         print_translated_file_to_http out table (frame ^ ".html")

   (*
    * Print the login page.
    *)
   let print_login_page out state =
      let table = table_of_state state in
      let table = BrowserTable.add_string table title_sym "MetaPRL Login Page" in
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
   let print_content_redisplay_page server state session outx =
      let { http_host     = host;
            http_port     = port
          } = http_info server
      in
      let { session_id = id;
            session_cwd = cwd
          } = session
      in
      let uri = sprintf "http://%s:%d/session/%d/content%s/" host port id cwd in
         if !debug_http then
            eprintf "Redirecting to %s@." uri;
         print_redirect_page outx SeeOtherCode uri

   (*
    * Redisplay the rulebox.
    *)
   let print_rule_redisplay_page server state session outx =
      let { http_host     = host;
            http_port     = port
          } = http_info server
      in
      let { session_id = id } = session in
      let uri = sprintf "http://%s:%d/session/%d/frame/rule/" host port id in
         if !debug_http then
            eprintf "Redirecting to %s@." uri;
         print_redirect_page outx SeeOtherCode uri

   let flush state session =
      let { session_buffer = buffer;
            session_shell  = shell
          } = session
      in
         Browser_display_term.synchronize buffer Shell.flush session.session_shell

   (*
    * Evaluate a command.
    * Send it to the shell, and get the result.
    *)
   let eval state session outx command =
      let { session_shell = shell;
            session_buffer = buffer
          } = session
      in
         Browser_display_term.add_prompt buffer command;
         Browser_display_term.synchronize buffer (Shell.eval_top shell) (command ^ ";;");
         invalidate_eval session

   let chdir state session dir =
      if !debug_http then
         eprintf "Changing directory to %s@." dir;
      let { session_buffer = buffer;
            session_shell = shell
          } = session
      in
      let success = Browser_display_term.synchronize buffer (Shell.chdir_top shell) dir in
         invalidate_chdir session success;
         success

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
       | LoginURI key ->
            if key = state.state_response then
               print_access_granted_page outx state
            else
               print_login_page outx state
       | FrameURI (session, frame) ->
            if is_valid_response state header then
               let width = get_window_width header in
                  print_page server state session outx width frame
            else
               print_login_page outx state
       | ContentURI (session, dirname, is_dir) ->
            if is_valid_response state header then
               let width = get_window_width header in
                  (* To ensure relative links are correct, all URLs must end with a slash *)
                  if chdir state session dirname && is_dir then
                     begin
                        flush state session;
                        print_page server state session outx width "content"
                     end
                  else
                     (* Invalid directory or directory change failed *)
                     print_content_redisplay_page server state session outx
            else
               print_login_page outx state
       | UnknownURI dirname ->
            print_error_page outx NotFoundCode

   (*
    * Handle a post command.  This means to submit the text to MetaPRL.
    *)
   let post server state outx inx uri header body =
      let body = parse_post_body body in
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
         match decode_uri state uri with
            FrameURI (session, "rule") ->
               let state =
                  match command with
                     Some command ->
                        if !debug_http then
                           eprintf "Command: \"%s\"@." (String.escaped command);
                        eval state session outx command
                   | None ->
                        ()
               in
                  print_rule_redisplay_page server state session outx
          | InputURI _
          | LoginURI _
          | UnknownURI _
          | ContentURI _
          | FrameURI _ ->
               print_error_page outx BadRequestCode;
               eprintf "Shell_simple_http: bad POST command@."

   (*
    * Handle a connection.
    * We handle only get and post for now.
    *)
   let http_connect server state outx inx args header body =
      let () =
         (* Process the command *)
         match args with
            "get" :: uri :: _ ->
               get server state outx uri header
          | "post" :: uri :: _ ->
               if is_valid_response state header then
                  post server state outx inx uri header body
               else
                  print_login_page outx state
          | command :: _ ->
               print_error_page outx NotImplementedCode;
               eprintf "Shell_simple_http: unknown command: %s@." command
          | [] ->
               print_error_page outx BadRequestCode;
               eprintf "Shell_simple_http: null command@."
      in
         state

   (*
    * Start a browser if possible.
    *
    * BUG JYH: this works with Mozilla/Linux, but I'm not at all sure
    * how well it will work on other systems.
    *      n8: It doesn't work so well on Safari/OS X, where you want
    * to use "open -a safari" as your browser command.
    *)
   let start_browser browser url =
      let browser_list = Str.split (Str.regexp "[ \t]+") browser in
      let argv = Array.of_list (browser_list @ [url]) in
         try ignore (Unix.waitpid [] (Unix.create_process browser argv Unix.stdin Unix.stdout Unix.stderr)) with
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
      let state = { state with state_table = table } in
      let metaprl_dir = state.state_mp_dir in

      (* Create the start file *)
      let filename = Filename.concat metaprl_dir "start.html" in
      let fd = Unix.openfile filename [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] 0o600 in
      let () =
         try Unix.fchmod fd 0o600 with
            Unix.Unix_error _ ->
               ()
      in
      let out = Unix.out_channel_of_descr fd in
      let file_url = sprintf "file://%s" filename in
      let http_url = sprintf "http://%s:%d/" host port in
         (* Start page *)
         print_start_page out state;
         Pervasives.flush out;
         Unix.close fd;

         (* Start the browser if requested *)
         (match !browser_string with
             Some browser ->
                start_browser browser file_url
           | None ->
                ());

         (* Tell the user *)
         eprintf "@[<v 0>@[<v 3>Browsing service started.  Point your browser to the following URL:@ %s@]@ \
@[<v 3>Or use the following URL:@ %s@]@]@." file_url http_url;

         (* Return the new translation state *)
         state

   (*
    * Setup the browser.
    *)
   let init_password () =
      (* Create the startup file *)
      let home =
         try Sys.getenv "HOME" with
            Not_found ->
               let home =
                  if Sys.os_type = "Win32" then
                     "C:"
                  else
                     "/tmp"
               in
                  eprintf "@[<v 3>Please set the HOME environment variable to point to your home directory.@ Using %s for now.@]@." home;
                  home
      in
      let metaprl_dir = Filename.concat home ".metaprl" in
      let () =
         try Unix.mkdir metaprl_dir 0o777 with
            Unix.Unix_error _ ->
               ()
      in

      (* Get, or create the password *)
      let passwd = Filename.concat metaprl_dir "passwd" in
      let password =
         if Sys.file_exists passwd then
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

         else
               (* Create a new password *)
            let fd = Unix.openfile passwd [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] 0o600 in
            let () =
               try Unix.fchmod fd 0o600 with
                  Unix.Unix_error _ ->
                     ()
            in
            let out = Unix.out_channel_of_descr fd in
            let password = new_challenge () in
               output_string out password;
               close_out out;

               eprintf "@[<v 3>*** Note ***@ \
*** This is the first time you are using the browser service.@ \
*** A new password has been created for you in the file %s@ \
*** You will need this password to connect to MetaPRL from your browser.@ \
*** You may change this password to something you can remember if you like.@ @]@." passwd;

               password
      in
         metaprl_dir, password

   (*
    * Start the web server.
    *)
   let main () =
      if !browser_flag then
         let mp_dir, password = init_password () in
         let id = 1 in
         let shell = Shell.get_current_shell () in
         let empty_buffer = Buffer.create 1 in
         let session =
            { session_id              = id;
              session_shell           = shell;
              session_buffer          = Browser_display_term.create ();
              session_cwd             = Shell.pwd shell;
              session_menu_version    = 1;
              session_content_version = 1;
              session_message_version = 1;
              session_buttons_version = 1;
              session_rule_version    = 1;
              session_browser_info    = None;
              session_menu            = None;
              session_buttons         = None;
              session_history         = None;
              session_styles          = None
            }
         in
         let state =
            { state_table     = BrowserTable.empty;
              state_mp_dir    = mp_dir;
              state_password  = password;
              state_challenge = "unknown";
              state_response  = "unknown";
              state_sessions  = IntTable.add IntTable.empty id session
            }
         in
         let state = update_challenge state in
            Shell.refresh_packages ();
            Shell.set_dfmode shell "html";
            serve_http http_start http_connect state !browser_port
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "nl"
 * End:
 * -*-
 *)
