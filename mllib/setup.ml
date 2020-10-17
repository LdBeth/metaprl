(*
 * Values for MetaPRL environment variables.
 *
 * ----------------------------------------------------------------
 *
 * Copyright (C) 2004-2006 MetaPRL Group, Caltech
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
open Unix
open Lm_printf
open Lm_thread

(*
 * Environment variables are prefixed with this string.
 *)
let environ_prefix = "MP"

let confname = "metaprl"

let shared_state = State.shared_val "Setup" (Hashtbl.create 5)

let delay name f =
   let writer tbl =
      try Hashtbl.find tbl name with
         Not_found ->
            let res = f () in
               Hashtbl.add tbl name res;
               res
   in
      fun () -> State.write shared_state writer

(* $(MP_ROOT) *)
let root =
    let writer () =
      let name = environ_prefix ^ "_ROOT" in
         try Sys.getenv name with
            Not_found ->
               raise (Invalid_argument ("Setup: the environment variable " ^ name ^ " is not defined"))
   in
      delay "Setup.root" writer

(* $(MPLIB) *)
let lib =
   let writer () =
      let name = environ_prefix ^ "LIB" in
         try Sys.getenv name with
            Not_found ->
               (* Fall back to $(MP_ROOT)/lib, but only if directory exists and is accessible *)
               let lib = Filename.concat (root ()) "lib" in
                  try
                     closedir(opendir lib);
                     lib
                  with
                     _ ->
                        raise (Invalid_argument ("Setup: " ^ name ^ " environment variable must be defined"))
   in
      delay "Setup.lib" writer

(* "/doc/htmlman/" *)
let doc_dir =
   let shared_state = State.shared_val "Setup.doc_dir" (ref None) in
   let writer ref =
      match !ref with
         Some dir -> dir
       | None ->
            let root = root () in
            let dir = Filename.concat root "/doc/htmlman/" in
            let dir =
               if
                  try 
                     (Unix.stat dir).st_kind = S_DIR
                  with _ ->
                     false
               then
                  Some "/doc/htmlman/"
               else
                  None
            in
               ref := Some dir;
               dir
   in
      fun () -> State.write shared_state writer

(*
 * Make sure directory exists and is writable (creating it, if necessary).
 * Returns true if a new directory was created.
 *)
let ensure_dir name mode =
   let new_dir =
      (* Does it exist already? *)
         try access name [F_OK]; false with
            Unix_error _ ->
               begin
                  try mkdir name mode with
                     Unix_error _ -> ()
               end;
               true
      in
         begin (* Make sure home is read/write *)
            try access name [F_OK;R_OK;W_OK]; closedir(opendir name); with
               Unix_error _ ->
                  raise (Invalid_argument ("Setup: Please make sure that " ^ name ^ " is a directory for which MetaPRL has read and write access"))
         end;
         new_dir


(* $(HOME)/.metaprl *)
let home =
   let writer () =
      let home =
         try Filename.concat (Sys.getenv "HOME") ("." ^ confname) with
            Not_found ->
               let home =
                  if Sys.os_type = "Win32" then
                     Filename.concat "C:" confname
                  else
                     sprintf "/tmp/%s-%i" confname (getuid ())
               in
                  eprintf "@[<v 3>@ WARNING!   Please set the HOME environment variable to point@ WARNING!   to your home directory.@ WARNING!   Using %s in place of $HOME/.%s for now.@]@." home confname;
                  home
      in
         (* Does "home" dir exist already? *)
         if ensure_dir home 0o700 then begin
            printf "@[<v 3>@ WARNING!   MetaPRL state directory %s did not exist, created a new one.@ @]@." home;
            (*
             * XXX TODO (nogin): Here we should add code that would ask all kind of questions
             * about user's preferences.
             *)
            ()
         end;
         home
   in
      delay "Setup.home" writer

let editor =
   let writer () =
      let file = Filename.concat (home ()) "editor" in
         try
            let inx = open_in file in
            let editor = input_line inx in
               close_in inx;
               editor
         with
            Sys_error _
          | End_of_file ->
               if Sys.os_type = "Win32" then
                  "notepad.exe"
               else
                  let editor =
                     try Sys.getenv "EDITOR" with
                        Not_found ->
                           "vi"
                  in
                     sprintf "xterm -e %s" editor
   in
      delay "Setup.editor" writer

let hostname_var = "browser_hostname"

let gethostname, sethostname =
   let nm = "Setup.hostname" in
   let hostname_env = environ_prefix ^ "_" ^ (String.uppercase hostname_var) in
   let getter tbl =
      try Hashtbl.find tbl nm with
         Not_found ->
            begin
               let name =
                  try Sys.getenv hostname_env with
                     Not_found ->
                        begin
                           let name = Unix.gethostname () in
                              Unix.putenv hostname_env name;
                              name
                        end
               in
                  Hashtbl.add tbl nm name;
                  name
            end
   in
   let setter name tbl =
      Unix.putenv hostname_env name;
      (Hashtbl.replace tbl nm name)
   in
      (fun () -> State.write shared_state getter),
      (fun name -> State.write shared_state (setter name))

let wrap_umask077 f () =
   (* Unix.umask will fail on Windows, but we do not care *)
   let old_umask =
      try Unix.umask 0o077
      with _ -> 0
   in
   let res = f () in
      ignore(try Unix.umask old_umask with _ -> 0);
      res

let certs_dir =
   let writer () =
      let ssl_dir = Filename.concat (home ()) "ssl" in
         ignore(ensure_dir ssl_dir 0o700);
         let certs_dir = Filename.concat ssl_dir (gethostname ()) in
            ignore(ensure_dir certs_dir 0o700);
            certs_dir
   in
      delay "Setup.certs_dir" writer

let execute_openssl args =
   eprintf "+ openssl";
   for i = 1 to pred (Array.length args) do
      eprintf " %s" args.(i)
   done;
   eflush stderr;
   let pid = Unix.create_process "openssl" args Unix.stdin Unix.stdout Unix.stderr in
   let _, status = Unix.waitpid [] pid in
      if status <> Unix.WEXITED 0 then
         raise (Failure "Setup.create_cert: executing openssl failed")

let create_cert name =
   if not (Sys.file_exists name) then begin
      let conf = Filename.concat (lib ()) "metaprl-ssl.config" in
      let () =
         try access conf [F_OK;R_OK] with
            Unix.Unix_error (err, _, _) ->
               raise (Failure ("Setup.create_cert: config file " ^ conf ^ " is not accessible: " ^ (Unix.error_message err)))
      in
         (* Make sure the MP_BROWSER_HOSTNAME environ variable is set - we refer to it in the config file *)
         sethostname (gethostname ());
         eprintf "Creating certificate file %s%t" name eflush;
         execute_openssl [|"openssl"; "req"; "-x509"; "-newkey"; "rsa:1024"; "-keyout"; name; "-out"; name; "-days"; "360"; "-config"; conf|]
   end;
   try access name [F_OK;R_OK] with
      Unix.Unix_error (err, _, _) ->
         raise (Failure ("Setup.create_cert: certificate file " ^ name ^ " is not accessible: " ^ (Unix.error_message err)))

(*
 * BUG JYH: this is a temporary hack.
 * Need to find a cleaner solution to no SSL.
 *)
let server_pem =
   let writer () =
      if Lm_ssl.enabled then
         let file = Filename.concat (certs_dir ()) "server.pem" in
            create_cert file;
            file
      else
         "/setup-error/server.pem"
   in
      delay "Setup.server_pem" (wrap_umask077 writer)

let client_pem =
   let writer () =
      if Lm_ssl.enabled then
         let file = Filename.concat (certs_dir ()) "client.pem" in
            create_cert file;
            file
      else
         "/setup-error/client.pem"
   in
      delay "Setup.client_pem" (wrap_umask077 writer)

let dh_pem =
   let writer () =
      if Lm_ssl.enabled then
         let name = Filename.concat (certs_dir ()) "dh.pem" in
            if not (Sys.file_exists name) then
               begin
                  eprintf "Creating certificate file %s%t" name eflush;
                  execute_openssl [|"openssl"; "dhparam"; "-2"; "-out"; name|];
               end;
            try access name [F_OK;R_OK]; name with
               Unix.Unix_error (err, _, _) ->
                  raise (Failure ("Setup.create_cert: certificate file " ^ name ^ " is not accessible: " ^ (Unix.error_message err)))
      else
         "/setup-error/dh.pem"
   in
      delay "Setup.dh_pem" (wrap_umask077 writer)

(*
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
