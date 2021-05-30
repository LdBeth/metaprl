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
open Lm_link_list


let path_stack =
   State.private_val "Shell_register.path_stack"
   (ref empty) (fun stack -> ref (copy !stack))

let push path =
   State.write path_stack (fun stack ->
         stack := insert path !stack)

let pop () =
   State.write path_stack (fun stack ->
         try let n, a = delete !stack
             in stack := n;
                a
         with Not_found ->
               eprintf "Path stack underflow%t" eflush;
               ".")

let swap () =
    State.write path_stack (fun stack ->
          try let n, a = delete !stack in
              let n, b = delete n in
                 stack := insert a n |> insert b;
                 b
          with Not_found ->
                eprintf "Path stack underflow, performing clear%t" eflush;
                stack := empty;
                ".")

let prev () =
   State.write path_stack (fun stack ->
         let ns = prev !stack
         in stack := ns;
            top ns)
let next () =
   State.write path_stack (fun stack ->
         let ns = next !stack
         in stack := ns;
            top ns)

let top () =
   State.read path_stack (fun stack ->
         try top !stack
         with Not_found ->
               eprintf "Path stack has no element%t" eflush;
               ".")

let clear () =
   State.write path_stack (fun stack ->
         stack := empty)

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
