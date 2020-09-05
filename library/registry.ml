(*
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/htmlman/default.html or visit http://metaprl.org/
 * for more information.
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
open Lm_debug

open Lint32

let _ =
   show_loading "Loading Registry%t"

(*
 * jyh: we need documentation for these types.
 * Unidirectional or Bidirectional (pair of) hash tables.
 * String is a symbolic label and lint32 is its numeric label.
 *)
type tb =
   Uni of (string, lint32) Hashtbl.t
 | Bi of (string, lint32) Hashtbl.t * (lint32, string) Hashtbl.t

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

let token_table = (Hashtbl.create 3)
let index_table = (Hashtbl.create 3)

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
  try
    (match Hashtbl.find local_registry regtype with
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
            | Bi (h1, h2) -> Hashtbl.find h1 id))) with
    Not_found ->
      raise (Failure ("undefined registry value for id: " ^ id))

(*
 * Lookup indentifiers.  jyh: what are identifiers?
 * An identifier is a string label for a mathbus node.
 *)
let registry_lookup_identifier regtype v =
  try
    (match (Hashtbl.find local_registry regtype) with
      Uni h ->
        failwith "Not a bidirectional registry property"
    | Bi (h1, h2) ->
        (try Hashtbl.find h2 v with
          Not_found ->
            (match (Hashtbl.find global_registry regtype) with
              Uni h -> failwith "Not a bidirectional registry property"
            | Bi (h1, h2) -> Hashtbl.find h2 v)))
  with
    Not_found ->
      raise (Failure ("undefined identifier in registry table: " ^ regtype))

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
(* unused
let registry_store_global id regtype v =
   match Hashtbl.find global_registry regtype with
      Uni h ->
         Hashtbl.add h id v
 | Bi (h1, h2) ->
      (Hashtbl.add h1 id v;
       Hashtbl.add h2 v id)
*)

(*
 * Read the next string from the registry.
 *)
let read_string stream =
   let rec read (buf :bytes) len skip =
      try
         match input_char stream with
            ' ' | '\r' | '\n' | '\t' ->
               if skip then
                  read buf len skip
               else
                  Lm_string_util.sub "MathBus.read_string" (Bytes.to_string buf) 0 len
          | c ->
               let buf =
                  if Bytes.length buf = len then
                     let buf' = Lm_string_util.create "MathBus.read_string" (len * 2) in
                        Lm_string_util.blit "MathBus.read_string" buf 0 buf' 0 len;
                        buf'
                  else
                     buf
               in
                  Lm_string_util.set "MathBus.read_string" buf len c;
                  read buf (len + 1) false
      with
         End_of_file ->
            Lm_string_util.sub "MathBus.read_string" (Bytes.to_string buf) 0 len
   in
      read (String.create 100) 0 true

(*
 * Read a 32bit integer from the stream.
 *)
let read_int32 stream =
   let s = read_string stream in
   let v = int_of_string s in
      if v < -9 then (*LAL hack-subtypes can be neg...number too big to convert from string*)
         (print_int v; failwith "fg" )  (*"not yet ready to read in lint32s"*)
      else
         lint32_of_int v

(*
 * Load the entire registry file.
 *)
let read_registry () =
   let stream = open_in (Filename.concat (Setup.lib()) "registry.txt") in
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

let read_tokens () =
   let stream = open_in (Filename.concat (Setup.lib()) "mbs-mpl.txt") in
   let p = read_string stream in
   let rec loop ident =
      if ident = "" then
         close_in stream
      else
         let a = int_of_string (read_string stream) in
            (Hashtbl.add token_table ident a; Hashtbl.add index_table a ident; loop (read_string stream))
   in
      loop p
