(*
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/index.html for information on Nuprl,
 * OCaml, and more information about this system.
 *
 * Copyright (C) 1998 Lori Lorigo, Richard Eaton, Cornell University
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
 * Authors: Lori Lorigo, Richard Eaton
 *
 * In this simple implementation, a registry is a collection of hash
 * tables, one or two for each registry type.  There are two
 * registries, one for the global information, and one for the local
 * information.
 *)
open Printf
open Mp_debug
open Mp_pervasives

open Int32

let _ =
   if !debug_load then
      eprintf "Loading Registry%t" eflush

(*
 * jyh: we need documentation for these types.
 * Unidirectional or Bidirectional (pair of) hash tables.
 * String is a symbolic label and int32 is its numeric label.
 *)
type tb =
   Uni of (string, int32) Hashtbl.t
 | Bi of (string, int32) Hashtbl.t * (int32, string) Hashtbl.t

(*
 * Registry hash table.
 * String is a registry type, tb it's corresponding cable or pair of tables.
 *)
type regtb = (string, tb) Hashtbl.t

let global_registry = (Hashtbl.create 3 : regtb)
let local_registry = (Hashtbl.create 3 : regtb)

let registry_types = ref []

(*
 * The registry file should be stored in the lib directory.
 *)
let registry_file =
   try Filename.concat (Sys.getenv "NLLIB") "registry.txt" with
      Not_found ->
         raise (Failure "environment variable NLLIB is not defined")

(*
 * Define a particular type of registry.
 *)
let define_registry_type label bidirectional =
   if not (List.mem label !registry_types) then
      begin
         registry_types := label::!registry_types;
         Hashtbl.add local_registry label
         (if bidirectional then
             Bi ((Hashtbl.create 10), (Hashtbl.create 10))
          else Uni (Hashtbl.create 10));

         Hashtbl.add global_registry label
         (if bidirectional then
             Bi ((Hashtbl.create 10), (Hashtbl.create 10))
          else Uni (Hashtbl.create 10));

      end

(*
 * These are the two types that we use.
 *)
let _ = define_registry_type "StringId" true
let _ = define_registry_type "SubTypes" false

(*
 * Remove all entries from the global and local registries.
 *)
let clear_registry globalp localp =
   begin
      if globalp then
         Hashtbl.clear global_registry;
      if localp then
         Hashtbl.clear local_registry
   end

(*
 * Get a value from the registry.
 *)
let registry_lookup_value id regtype =
   match Hashtbl.find local_registry regtype with
      Uni h ->
         (try Hashtbl.find h id with
             Not_found ->
                (match (Hashtbl.find global_registry regtype) with
                    Uni h -> Hashtbl.find h id
                  | Bi (h1, h2) -> Hashtbl.find h1 id))
    | Bi (h1, h2) ->
         (try Hashtbl.find h1 id with
             Not_found ->
                (match (Hashtbl.find global_registry regtype) with
                    Uni h -> Hashtbl.find h id
                  | Bi (h1, h2) -> Hashtbl.find h1 id))

(*
 * Lookup indentifiers.  jyh: what are identifiers?
 * An identifier is a string label for a mathbus node.
 *)
let registry_lookup_identifier regtype v =
   match (Hashtbl.find local_registry regtype) with
      Uni h ->
         failwith "Not a bidirectional registry property"
    | Bi (h1, h2) ->
         (try Hashtbl.find h2 v with
             Not_found ->
                (match (Hashtbl.find global_registry regtype) with
                    Uni h -> failwith "Not a bidirectional registry property"
                  | Bi (h1, h2) -> Hashtbl.find h2 v))

(*
 * Save a value in the local registry.
 *)
let registry_store_local id regtype v =
   match Hashtbl.find local_registry regtype with
      Uni h ->
         Hashtbl.add h id v
 | Bi (h1, h2) ->
      (Hashtbl.add h1 id v;
       Hashtbl.add h2 v id)

(*
 * Save a value in the global registry.
 *)
let registry_store_global id regtype v =
   match Hashtbl.find global_registry regtype with
      Uni h ->
         Hashtbl.add h id v
 | Bi (h1, h2) ->
      (Hashtbl.add h1 id v;
       Hashtbl.add h2 v id)

(*
 * Read the next string from the registry.
 *)
let read_string stream =
   let rec read (buf : string) len skip =
      try
         match input_char stream with
            ' ' | '\r' | '\n' | '\t' ->
               if skip then
                  read buf len skip
               else
                  String_util.sub "MathBus.read_string" buf 0 len
          | c ->
               let buf =
                  if String.length buf = len then
                     let buf' = String_util.create "MathBus.read_string" (len * 2) in
                        String_util.blit "MathBus.read_string" buf 0 buf' 0 len;
                        buf'
                  else
                     buf
               in
                  String_util.set "MathBus.read_string" buf len c;
                  read buf (len + 1) false
      with
         End_of_file ->
            String_util.sub "MathBus.read_string" buf 0 len
   in
      read (String.create 100) 0 true

(*
 * Read a 32bit integer from the stream.
 *)
let read_int32 stream =
   let s = read_string stream in
   let v = int_of_string s in
      if v < -9 then (*LAL hack-subtypes can be neg...number too big to convert from string*)
         (print_int v; failwith "fg" )  (*"not yet ready to read in int32s"*)
      else
         int32_of_int v

(*
 * Load the entire registry file.
 *)
let read_registry =
   let stream = open_in registry_file in
   let p = read_string stream in
   let rec loop ident =
      if ident = "" then
         close_in stream
      else
         let a = read_string stream in
         let b = read_int32 stream in
            registry_store_local ident a b;
            loop (read_string stream)
   in
      loop p

(*
 read_registry registry_file

 Generated the LISP declaration file for label names.
let generate_registry_declarations ofile file =
  let stream = open_in file and ostream = open_out ofile in
  when or None ofile None file
    multiple_value_bind in out default_registry_files
      when None ofile
	setq ofile out
      when None file
	setq file in	
  with_open_file stream file :direction :input
    with_open_file ostream ofile :direction :output
    format ostream registry_header_text

    loop for ident = read_item stream
	  with regtype and regval
	  do when None ident
	       return t
	  setq regtype read_item stream
	  setq regval read_item stream
	  when string_equal regtype "StringId"
	     Don't bother with MBS_ variables for single characters or Temps'
	    unless or = length ident 1
			and = length ident 5
			     char= aref ident 0 #\T
			     char= aref ident 1 #\e
			     char= aref ident 2 #\m
			     char= aref ident 3 #\p
	      format ostream "~%defvar mBS_~A #x~16R~%export 'mathbus::MBS_~A 'mathbus~%"
		      ident regval string_upcase ident
	  format ostream "registry_store_local \"~A\" \"~A\" ~D~%"
		  ident regtype regval
    format ostream "~%~%; End of automatically generated file.~%~%"

*)

