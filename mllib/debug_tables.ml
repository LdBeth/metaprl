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
open Lm_printf
open Lm_map_sig

module MakeTable
(Base : TableBaseSig)
(Table1: TableSig
         with type elt = Base.elt
         with type data = Base.data)
(Table2: TableSig
         with type elt = Base.elt
         with type data = Base.data) =
struct

   type elt = Table1.elt
   type data = Table1.data
   type t = Table1.t * Table2.t

   let print out (t1, t2) =
      fprintf out "@[<v 0>@[<hv 3>Table1:%a@]@ @[<hv 3>Table2:%a@]@]@." (**)
         Table1.print t1
         Table2.print t2

   let error t =
      print stderr t;
      raise (Invalid_argument "DebugTables")

   let mem (t1,t2 as t) key =
      match Table1.mem t1 key, Table2.mem t2 key with
         true, true -> true
       | false, false -> false
       | b1,b2 ->
            printf "DebugTables.mem mismatch:\nTable1: %b Table2: %b" b1 b2;
            error t

   let find (t1,t2 as t) key =
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
         if (d1=d2) then d1 else
            begin
               eprintf "@[<hv 0>DebugTables.find mismatch: found different data:@ @[<hv 3>%t@]@]" (**)
                  (fun out -> Base.print out key [d1; d2]);
               error t
            end

   let find_all (t1,t2 as t) key =
      let d1 = Table1.find_all t1 key
      and d2 = Table2.find_all t2 key in
         if List.length d1 = List.length d2 then
            if d1 = d2 then d1 else
               begin
                  eprintf "@[<hv 3>DebugTables.find_all mismatch: found different data:@ @[<hv 3>In table1:@ %t@]@ @[<hv 3>In table2:@ %t@]@]" (**)
                     (fun out -> Base.print out key d1)
                     (fun out -> Base.print out key d2);
                  error t
               end
         else begin
                 eprintf "DebugTables.find_all mismatch:\n\t%d entries in Table1, but %d entries in Table2\n" (List.length d1) (List.length d2);
                 error t
              end

   exception Mismatch of t * elt

   let check_it (t1,t2 as t) key _ =
      if Table1.find_all t1 key <> Table2.find_all t2 key
      then raise (Mismatch (t,key))

   let check (t1,t2 as t) =
      Table1.iter (check_it t) t1;
      Table2.iter (check_it t) t2;
      t

   let empty =
      try
         check (Table1.empty, Table2.empty)
      with
         Mismatch (t,key) ->
            eprintf "@[<v 3>Create made non-empty tables:@ ";
            ignore(find_all t key);
            raise(Invalid_argument "DebugTables.create - something funny")

   let add (t1, t2 as t) key data =
      try
         check (Table1.add t1 key data, Table2.add t2 key data)
      with
         Mismatch (t',key') ->
            eprintf "@[<v 3>Add made different tables:@ @[<v 3>Old table: %a@]@ @[<hv 3>Added data:@ %t@]" (**)
               print t
               (fun out -> Base.print out key [data]);
            ignore(find_all t' key');
            raise(Invalid_argument "DebugTables.add - something funny")

   let union (t1, t2 as t) (t1', t2' as t') =
      try
         check(Table1.union t1 t1', Table2.union t2 t2')
      with
         Mismatch (t'',key) ->
            eprintf "@[<v 3>Union made different tables:@ ";
            ignore(find_all t'' key);
            raise(Invalid_argument "DebugTables.union - something funny")

   let remove (t1,t2) key =
      try
         check(Table1.remove t1 key, Table2.remove t2 key)
      with
         Mismatch (t',key') ->
            eprintf "@[< v 3>Remove made different tables:@ ";
            ignore(find_all t' key');
            raise(Invalid_argument "DebugTables.remove - something funny")

   let iter f (t1, t2) =
      Table1.iter f t1

   let list_of (t1, t2) =
      let l1=Table1.list_of t1 in
      let l2=Table2.list_of t2 in
         if l1=l2 then
            l1
         else
            begin
               eprintf "@[<v 3>List_of made different lists:@ %a" (**)
                  print (t1, t2);
               raise(Invalid_argument "DebugTables.list_of - something funny")
            end

   let map f (t1, t2) =
      try
         check(Table1.map f t1, Table2.map f t2)
      with
         Mismatch (t',key) ->
            eprintf "@[<v 3>Map made different tables:@ ";
            ignore(find_all t' key);
            raise(Invalid_argument "DebugTables.map - something funny")

   let deletemax (t1,t2) =
      let (k1,m1,t1')=Table1.deletemax t1 in
      let (k2,m2,t2')=Table2.deletemax t2 in
         if (compare k1 k2 =0) then
            if Lm_list_util.compare_eq m1 m2 then
               try
                  let t=check (t1',t2') in
                     (k1,m1,t)
               with
                  Mismatch (t',key') ->
                     eprintf "@[<v 3>Deletemax made different tables:@ ";
                     ignore(find_all t' key');
                     raise(Invalid_argument "DebugTables.deletemax - something funny")
            else
               begin
                  eprintf "@[<v 3>Deletemax returned different content of same maximum:@ ";
                  ignore(find_all (t1',t2') k1);
                  raise(Invalid_argument "DebugTables.deletemax - something funny")
               end
         else
            begin
               eprintf "@[<v 3>Deletemax returned different maximums:@ ";
               ignore(find_all (t1',t2') k1);
               eprintf "@ ";
               ignore(find_all (t1',t2') k2);
               raise(Invalid_argument "DebugTables.deletemax - something funny")
            end

end
