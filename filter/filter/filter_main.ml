(*
 * Main function collects arguments, and starts parsing.
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
 * Copyright (C) 1998 Jason Hickey, Cornell University
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

open Arg
open Printf
open Mp_debug

open Filter_parse

(*
 * Show the file loading.
 *)
let _ =
   show_loading "Loading Filter_main%t"

(*
 * string -> path commands
 *)
let set_path doc var path =
   let path' = String_util.split ':' path in
      var := path'

let set_path_arg doc var =
   Arg.String (set_path doc var)

(*
 * This is a list of hosts to use for database lookup.
 *)
let include_path = Env_arg.general "include" ["."] "Include directories" set_path set_path_arg

let add_include path =
   include_path := !include_path @ [path]

let set_output_file name =
   Pcaml.output_file := Some name

let remove_output_file () =
   match !Pcaml.output_file with
      Some name ->
         Sys.remove name
    | None ->
         ()

let set_lib s =
   let var = sprintf "MPLIB=%s" s in
      Punix.putenv var

let set_export () =
   Punix.putenv "MP_EXPORT=true"

let add_anon_arg arg =
   ()

let spec =
   ["-I", String add_include, "add an directory to the path for include files";
    "-o", String set_output_file, "specify output file";
    "-lib", String set_lib, "specify MPLIB directory";
    "-export", Unit set_export, "export an ASCII file"]

(*
 * Add the include path.
 *)
let main () =
   Arg.current := 1;
   Env_arg.parse spec add_anon_arg "MetaPRL compiler";
   set_include_path !include_path;
   Filter_parse.init ();
   try
      Filter_exn.print Dform.null_base !Odyl_main.go ();
      Filter_parse.close ()
   with
      exn ->
         remove_output_file ();
         raise exn

external exit : int -> unit = "caml_exit"

let _ =
   try main () with
      exn ->
         exit 1

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
