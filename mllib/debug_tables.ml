(*
 * Combine two table implementations to check that they have the same
 * behavior.
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
 * Copyright (C) 1998 Alexey Nogin, Cornell University
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
 * Author: Alexey Nogin
 *)

open Format 
open Set_sig

module MakeTable
   (Base : TableBaseSig)
   (Table1: TableSig
               with type elt = Base.elt
               with type set = Base.set
               with type data = Base.data)
   (Table2: TableSig
               with type elt = Base.elt
               with type set = Base.set
               with type data = Base.data) =
struct

   type elt = Table1.elt
   type set = Table1.set
   type data = Table1.data
   type t = set * Table1.t * Table2.t

   let print (_,t1,t2) =
      force_newline ();
      open_box 3;
      print_string "Table1:";
      Table1.print t1;
      close_box ();
      force_newline ();
      open_box 3;
      print_string "Table2:";
      Table2.print t2;
      close_box ();
      force_newline ()

   let error t =
      print t;
      print_flush ();
      raise (Invalid_argument "DebugTables")

   let mem (_, t1,t2 as t) key =
      match Table1.mem t1 key, Table2.mem t2 key with
         true, true -> true
       | false, false -> false
       | b1,b2 ->
            printf "DebugTables.mem mismatch:\nTable1: %b Table2: %b" b1 b2;
            error t

   let find (s,t1,t2 as t) key =
      let d1 =
         try Table1.find t1 key
         with Not_found ->
            ignore (Table2.find t2 key);
            printf "DebugTables.find mismatch: found in Table2, but not in Table1\n";
            error t
      in let d2 =
         try Table2.find t2 key
         with Not_found ->
            printf "DebugTables.find mismatch: found in Table1, but not in Table2\n";
            error t
      in
         if (d1=d2) then d1 else begin
            open_box 0;
            print_string "DebugTables.find mismatch: found different data:";
            print_newline ();
            open_box 3;
            Base.print s key [d1;d2];
            close_box ();
            close_box ();
            error t
         end

   let find_all (s,t1,t2 as t) key =
      let d1 = Table1.find_all t1 key
      and d2 = Table2.find_all t2 key in
      if List.length d1 = List.length d2 then
         if d1 = d2 then d1 else begin
            open_hvbox 3;
            print_string "DebugTables.find_all mismatch: found different data:";
            print_space ();
            open_hvbox 3;
            print_string "In table1:";
            print_space ();
            Base.print s key d1;
            close_box ();
            print_space ();
            open_hvbox 3;
            print_string "In table2:";
            print_space ();
            Base.print s key d2;
            close_box ();
            close_box ();
            error t
         end
      else begin
         printf "DebugTables.find_all mismatch:\n\t%d entries in Table1, but %d entries in Table2\n" (List.length d1) (List.length d2);
         error t
      end

   exception Mismatch of t * elt

   let check_it (_,t1,t2 as t) key _ =
      if Table1.find_all t1 key <> Table2.find_all t2 key
      then raise (Mismatch (t,key))

   let check (_,t1,t2 as t) =
      Table1.iter (check_it t) t1;
      Table2.iter (check_it t) t2;
      t

   let create s =
      try
         check (s, Table1.create s, Table2.create s)
      with
         Mismatch (t,key) ->
            open_vbox 3;
            print_string "Create made non-empty tables:";
            print_space ();
            ignore(find_all t key);
            raise(Invalid_argument "DebugTables.create - something funny")

   let add (s, t1, t2 as t) key data =
      try 
         check (s, Table1.add t1 key data, Table2.add t2 key data)
      with
         Mismatch (t',key') ->
            open_vbox 3;
            print_string "Add made different tables:";
            print_space ();
            open_vbox 3;
            print_string "Old table:";
            print t;
            close_box ();
            open_hvbox 3;
            print_string "Added data:";
            print_space ();
            Base.print s key [data];
            close_box ();
            print_cut();
            ignore(find_all t' key');
            raise(Invalid_argument "DebugTables.add - something funny")

   let union (s, t1, t2 as t) (s', t1', t2' as t') =
      try
         check(Base.union s s', Table1.union t1 t1', Table2.union t2 t2')
      with
         Mismatch (t'',key) ->
            open_vbox 3;
            print_string "Union made different tables:";
            print_space ();
            ignore(find_all t'' key);
            raise(Invalid_argument "DebugTables.union - something funny")

   let remove (s,t1,t2) key =
      try
         check(s, Table1.remove t1 key, Table2.remove t2 key)
      with
         Mismatch (t',key') ->
            open_vbox 3;
            print_string "Remove made different tables:";
            print_space ();
            ignore(find_all t' key');
            raise(Invalid_argument "DebugTables.remove - something funny")

   let iter f (_, t1, t2) =
      Table1.iter f t1

   let map f (s, t1, t2) =
      try
         check(s, Table1.map f t1, Table2.map f t2)
      with
         Mismatch (t',key) ->
            open_vbox 3;
            print_string "Map made different tables:";
            print_space ();
            ignore(find_all t' key);
            raise(Invalid_argument "DebugTables.map - something funny")


end
