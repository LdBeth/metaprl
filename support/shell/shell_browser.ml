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
open Format

open Lm_debug
open Lm_symbol

open Http_simple
open Http_server_type

open Browser_copy
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
    * Keep track of the state.
    *)
   type state =
      { state_shell     : Shell.t;
        state_table     : BrowserTable.t;
        state_long      : bool;
        state_mp_dir    : string;
        state_password  : string;
        state_challenge : string;
        state_response  : string
      }

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
               (try List.assoc "MetaPRL.response" cookies with
                   Not_found ->
                      search rest)
          | _ :: rest ->
               search rest
          | [] ->
               raise Not_found
      in
         try search header = state.state_response with
            Not_found ->
               false

   (*
    * Add the info to the table.
    *)
   let table_of_state state =
      let { state_table = table;
            state_challenge = challenge;
            state_response = response
          } = state
      in
      let table = BrowserTable.add_string table challenge_sym challenge in
      let table = BrowserTable.add_string table response_sym (String.sub response 8 8) in
         table

   (*
    * Add the title and the location.
    *)
   let add_title_location state title location =
      let table = table_of_state state in
      let table = BrowserTable.add_string table title_sym title in
      let table = BrowserTable.add_string table location_sym location in
         table

   (*
    * Add the rulebox to the state table.
    *)
   let add_rulebox state command =
      let table = state.state_table in
      let table = BrowserTable.add_string table rulebox_sym command in
      let table = BrowserTable.add_fun table rulebox_hex_sym (fun buf -> Buffer.add_string buf (encode_hex command)) in
         { state with state_table = table }

   (*
    * This is the default printer that uses tables for display.
    *)
   let print_page out state location =
      let table = add_title_location state "MetaPRL display" location in
      let table = BrowserTable.add_buffer table body_sym Browser_display_term.buffer in
      let table = BrowserTable.add_buffer table message_sym Browser_display_term.message in
      let filename =
         if state.state_long then
            "pagelong.html"
         else
            "pageshort.html"
      in
         print_translated_file_to_http out table filename

   (*
    * Print the login page.
    *)
   let print_login_page out state =
      let state = update_challenge state in
      let table = table_of_state state in
      let table = BrowserTable.add_string table title_sym "MetaPRL Login Page" in
         print_translated_file_to_http out table "login.html";
         state

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
      let state = update_challenge state in
      let table = table_of_state state in
      let table = BrowserTable.add_string table title_sym "MetaPRL Access Granted" in
      let table = BrowserTable.add_string table response_sym state.state_response in
         print_translated_file_to_http out table "access.html";
         state

   (*
    * Evaluate a command.
    * Send it to the shell, and get the result.
    *)
   let eval state outx command =
      Browser_display_term.reset ();
      Shell.eval state.state_shell (command ^ ";;")

   let chdir state dir =
      if !debug_http then
         eprintf "Changing directory to %s@." dir;
      Browser_display_term.reset ();
      ignore (Shell.cd state.state_shell dir)

   let flush state =
      Shell.flush state.state_shell

   (*
    * Handle a get command by changing directories as specified.
    *
    * This is untrusted, so each case should check if access is
    * granted.
    *)
   let get state outx uri header =
      if !debug_http then
         eprintf "Get: %s@." uri;

      (*
       * Catch references to direct files.
       *)
      match decode_uri uri with
         "inputs" :: uri ->
            let filename = Lm_string_util.concat "/" uri in
               print_raw_file_to_http outx filename;
               state
       | ["login"; key] when key = state.state_response ->
            print_access_granted_page outx state
       | _ ->
            if is_valid_response state header then
               let uri = "" :: decode_uri uri in
               let dirname = Lm_string_util.concat "/" uri in
                  chdir state dirname;
                  flush state;
                  print_page outx state dirname;
                  state
            else
               print_login_page outx state

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
      let uri = decode_uri uri in
      let dirname = Lm_string_util.concat "/" uri in

      (* Precedence *)
      let command =
         try Some (List.assoc "command" body) with
            Not_found ->
               None
      in
      let button =
         try Some (List.assoc "button" body) with
            Not_found ->
               None
      in
      let macro =
         try Some (List.assoc "macro" body) with
            Not_found ->
               None
      in
         match button with
            Some ("Long" as s)
          | Some ("Short" as s) ->
               let state = { state with state_long = s = "Long" } in
               let state' =
                  match command with
                     Some command ->
                        add_rulebox state command
                   | None ->
                        state
               in
                  print_page outx state' dirname;
                  state
          | Some "Submit"
          | None ->
               (match macro, command with
                   Some command, _
                 | None, Some command ->
                      if !debug_http then
                         eprintf "Command: \"%s\"@." (String.escaped command);
                      eval state outx command;
                      let { http_host     = host;
                            http_port     = port
                          } = http_info server
                      in
                      let uri = sprintf "http://%s:%d%s" host port (Shell.pwd state.state_shell) in
                         print_redirect_page outx SeeOtherCode uri;
                         state
                 | None, None ->
                      print_error_page outx BadRequestCode;
                      eprintf "Shell_simple_http: null command@.";
                      state)
          | Some button ->
               Browser_display_term.set_message (sprintf "Unknown button %s" button);
               print_page outx state dirname;
               state

   (*
    * Handle a connection.
    * We switch the mode to HTML.
    *)
   let http_connect server state outx inx args header body =
      (* Process the command *)
      match args with
         "get" :: uri :: _ ->
            get state outx uri header
       | "post" :: uri :: _ ->
            if is_valid_response state header then
               post server state outx inx uri header body
            else
               print_login_page outx state
       | command :: _ ->
            print_error_page outx NotImplementedCode;
            eprintf "Shell_simple_http: unknown command: %s@." command;
            state
       | [] ->
            print_error_page outx BadRequestCode;
            eprintf "Shell_simple_http: null command@.";
            state

   (*
    * Start a browser if possible.
    *
    * BUG JYH: this works with Mozilla/Linux, but I'm not at all sure
    * how well it will work on other systems.
    *)
   let start_browser browser url =
      let argv = [|browser; url|] in
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
This is the first time you are using the browser service.@ \
A new password has been created for you in the file %s@ \
You will need this password to connect to MetaPRL from your browser.@ \
You may change this password to something you can remember if you like.@ @]@." passwd;

               password
      in
         metaprl_dir, password

   (*
    * Start the web server.
    *)
   let main () =
      if !browser_flag then
         let mp_dir, password = init_password () in
         let shell = Shell.get_current_shell () in
         let state =
            { state_shell     = shell;
              state_table     = BrowserTable.empty;
              state_long      = false;
              state_mp_dir    = mp_dir;
              state_password  = password;
              state_challenge = "unknown";
              state_response  = "unknown"
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
