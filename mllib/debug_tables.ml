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
      open_vbox 0;
      print_string "Table1:";
      print_break 0 3;
      open_hvbox 0;
      Table1.print t1;
      close_box ();
      force_newline ();
      print_string "Table2:";
      print_break 0 3;
      open_hvbox 0;
      Table2.print t2;
      close_box ();
      close_box ();
      force_newline ()

   let error t =
      print t;
      print_flush ();
      raise (Invalid_argument "DebugTables")

   let create s =
      s, Table1.create s, Table2.create s

   let add (s, t1, t2) key data =
      s, Table1.add t1 key data, Table2.add t2 key data

   let union (s, t1, t2) (s', t1', t2') =
      Base.union s s', Table1.union t1 t1', Table2.union t2 t2'

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
            open_vbox 0;
            print_string "DebugTables.find_all mismatch: found different data";
            print_break 0 3;
            open_hvbox 0;
            print_string "In table1:";
            print_break 1 3;
            open_hvbox 0;
            print_space ();
            Base.print s key d1;
            close_box ();
            force_newline ();
            print_string "In table2:";
            print_break 0 3;
            open_hvbox 0;
            print_space ();
            Base.print s key d2;
            close_box ();
            close_box ();
            close_box ();
            error t
         end
      else begin
         printf "DebugTables.find_all mismatch:\n\t%d entries in Table1, but %d entries in Table2\n" (List.length d1) (List.length d2);
         error t
      end

   let remove (s,t1,t2) key =
      s, Table1.remove t1 key, Table2.remove t2 key

   let iter f (_, t1, t2) =
      Table1.iter f t1

   let map f (s, t1, t2) =
      s, Table1.map f t1, Table2.map f t2

end
