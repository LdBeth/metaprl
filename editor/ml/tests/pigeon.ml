(*
 * Generate pigeonhole examples.
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
 *
 *)

open Lm_printf

let generate size =
   (* Exclude other pigeons in this hole *)
   let one_per_hole pigeon hole =
      for i = 0 to size do
         if i != pigeon then
            begin
               if i > (if pigeon = 0 then 1 else 0) then
                  printf " or";
               printf " 'x%d_%d" i hole
            end
      done
   in

   (* Exclude this pigeon from the other holes *)
   let one_hole_only pigeon hole =
      for i = 1 to size do
         if i != hole then
            printf " or 'x%d_%d" pigeon i
      done
   in

      (* Header *)
      printf "interactive pigeon%d 'H : :\n   sequent ['ext] { <H" size>;

      (* Pigeon positions *)
      for i = 0 to size do
         for j = 1 to size do
            printf ";\n\tx%d_%d: univ[1:l]" i j
         done;
      done;

      (* Exclusion *)
      for i = 0 to size do
         for j = 1 to size do
            printf ";\n\th%d_%d: 'x%d_%d => \"not\"{." i j i j;
            one_per_hole i j;
            one_hole_only i j;
            printf "}"
         done;
      done;

      (* At least one hole *)
      for i = 0 to size do
         printf ";\n\th%d: 'x%d_1" i i;
         for j = 2 to size do
            printf " or 'x%d_%d" i j
         done;
      done;

      (* Finish sequent *)
      printf "\n\t>- void\n}\n\n";
      flush stdout

let _ =
   for i = 2 to 9 do
      generate i
   done

(*
 * -*-
 * Local Variables:
 * Caml-master: "mp.run"
 * End:
 * -*-
 *)
