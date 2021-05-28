(*
 * The register keeps a list of strings.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2021 LdBeth
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
 * Author: LdBeth
 * @email{ldbeth@sdf.org}
 * @end[license]
 *)
open Lm_thread
open Lm_rprintf

let path_stack =
   State.private_val "Shell_register.path_stack"
   (Stack.create ()) Stack.copy

let push path =
   State.write path_stack (fun stack ->
         Stack.push path stack)

let pop () =
   State.write path_stack (fun stack ->
         try Stack.pop stack
         with Stack.Empty ->
               eprintf "Path stack underflow%t" eflush;
               ".")

let swap () =
    State.write path_stack (fun stack ->
          try let a = Stack.pop stack in
              let b = Stack.pop stack in
                 Stack.push a stack;
                 Stack.push b stack;
                 b
         with Stack.Empty ->
               eprintf "Path stack underflow, performing clear%t" eflush;
               Stack.clear stack;
               ".")

let top () =
   State.read path_stack (fun stack ->
         try Stack.top stack
         with Stack.Empty ->
               eprintf "Path stack has no element%t" eflush;
               ".")

let clear () =
   State.write path_stack (fun stack ->
         Stack.clear stack)

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
