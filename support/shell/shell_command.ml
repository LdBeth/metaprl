(*
 * We handle shell commands imperatively.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2004 Mojave Group, Caltech
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
 * @email{jyh@cs.caltech.edu}
 * @end[license]
 *)
extends Mptop

open Lm_debug
open Lm_rprintf

open Opname
open Refiner.Refiner.Term

open Shell_sig
open Shell_util

(*
 * Default command set.
 *)
let uninitialized _ =
   raise (Invalid_argument "The Shell module has not been instantiated")

let commands =
   { initialized = false;
     init = uninitialized;
     cd = uninitialized;
     root = uninitialized;
     refresh = uninitialized;
     pwd = uninitialized;
     relative_pwd = uninitialized;
     fs_pwd = uninitialized;
     set_dfmode = uninitialized;
     set_dftype = uninitialized;
     create_pkg = uninitialized;
     backup = uninitialized;
     backup_all = uninitialized;
     save = uninitialized;
     save_all = uninitialized;
     export = uninitialized;
     export_all = uninitialized;
     revert = uninitialized;
     revert_all = uninitialized;
     view = uninitialized;
     check = uninitialized;
     apply_all = uninitialized;
     expand = uninitialized;
     expand_all = uninitialized;
     interpret = uninitialized;
     undo = uninitialized;
     redo = uninitialized;
     create_ax_statement = uninitialized;
     refine = uninitialized;
     print_theory = uninitialized;
     extract = (fun _ -> uninitialized);
     term_of_extract = uninitialized;
     get_view_options = uninitialized;
     set_view_options = uninitialized;
     clear_view_options = uninitialized;
     find_subgoal = uninitialized;
     is_enabled = uninitialized;
     edit = uninitialized
   }

(*
 * Set the commands.
 *)
let synchronize f =
   if commands.initialized then
      raise (Invalid_argument "The Shell_commands are initialized twice");
   f commands

(*
 * Control profiling.
 *)
external restart_gmon : unit -> unit = "restart_gmon"
external stop_gmon : unit -> unit = "stop_gmon"

let mk_dep_name opname =
   Lm_string_util.prepend "/" (List.rev (dest_opname opname))

(*
 * Call the commands.
 *)
let extract path () = commands.extract path ()
let term_of_extract ts = commands.term_of_extract ts

let init () = commands.init ()
let cd s = commands.cd s
let refresh () = commands.refresh ()
let pwd () = commands.pwd ()
let relative_pwd () = commands.relative_pwd ()
let fs_pwd () = commands.fs_pwd ()
let set_dfmode s = commands.set_dfmode s
let create_pkg s = commands.create_pkg s
let backup _ = commands.backup ()
let backup_all _ = commands.backup_all ()
let save _ = commands.save ()
let save_all _ = commands.save_all ()
let export _ = commands.export ()
let export_all _ = commands.export_all ()
let revert _ = commands.revert ()
let revert_all _ = commands.revert_all ()
let check _ = commands.check ()
let expand _ = commands.expand ()
let expand_all _ = commands.expand_all ()
let apply_all f t c = commands.apply_all f t c
let undo _ = commands.undo ()
let redo _ = commands.redo ()
let create_ax_statement t s = commands.create_ax_statement t s
let refine t = commands.refine t
let print_theory s = commands.print_theory s
let get_view_options _ = commands.get_view_options ()
let set_view_options s = commands.set_view_options s
let clear_view_options s = commands.clear_view_options s
let find_subgoal i = commands.find_subgoal i
let is_enabled name = commands.is_enabled name

let kreitz _ = commands.interpret ProofKreitz
let clean _ = commands.interpret ProofClean
let squash _ = commands.interpret ProofSquash
let copy s = commands.interpret (ProofCopy s)
let paste s = commands.interpret (ProofPaste s)
let make_assum _ = commands.interpret ProofMakeAssum

let interpret_all command modifies =
   let f item db =
      item.edit_interpret [] command
   in
      apply_all f true false

let clean_all _ = interpret_all ProofClean false
let squash_all _ = interpret_all ProofSquash false

let root () = commands.root ()

(*
 * Toploop functions
 *)
let exit () = raise End_of_file

let abort () =
   Pervasives.exit 126

let set_debug = set_debug

let print_gc_stats () =
   Lm_rprintf.flush stdout;
   Gc.print_stat Pervasives.stdout;
   Pervasives.flush Pervasives.stdout

let set_tex_file = Shell_tex.set_file

let ls s =
   let options = ls_options_of_string s in
   let options =
      if LsOptionSet.is_empty options then
         ls_options_default
      else
         options
   in
      commands.view options

let status item =
   let name, status, _, _ = item.edit_get_contents [] in
   let str_status = match status with
      ObjPrimitive ->
         "is a primitive axiom"
    | ObjDerived ->
         "is an internally derived object"
    | ObjComplete (c1, c2) ->
         sprintf "is a derived object with a complete proof (%i rule boxes, %i primitive steps)" c1 c2
    | ObjIncomplete (c1, c2) ->
         sprintf "is a derived object with an incomplete proof (%i rule boxes, %i primitive steps)" c1 c2
    | ObjBad ->
         "is a derived object with a broken proof"
    | ObjUnknown ->
         "is an object with unknown status"
   in
      eprintf "Status: `%s' %s%t" name str_status eflush

let status_all () =
   let f item db =
      eprintf "Expanding `%s':%t" (let name, _, _, _ = item.edit_get_contents [] in name) eflush;
      begin try item.edit_interpret [] ProofExpand with Invalid_argument _ | _ -> () end;
      status item;
   in
      apply_all f true true

let check_all () =
   (* Make a few things bols, or highlight with `...' and *...* *)
   let bfs, bfe, bfs2, bfe2 =
      match Lm_terminfo.tgetstr Lm_terminfo.enter_bold_mode, Lm_terminfo.tgetstr Lm_terminfo.exit_attribute_mode with
         Some b, Some e -> b, e, b, e
       | _ -> "*", "*", "`", "'"
   in
   let check item =
      let name, _, _, _ = item.edit_get_contents [] in
      let status =
         match item.edit_check () with
            RefPrimitive ->
               "is a primitive axiom"
          | RefIncomplete (c1, c2) ->
               sprintf "is a derived object with an %sincomplete%s proof (%i rule boxes, %i primitive steps)" bfs bfe c1 c2
          | RefComplete (c1, c2, l) ->
               sprintf "is a derived object with a complete grounded proof (%i rule boxes, %i primitive steps, %i dependencies)" c1 c2 (List.length l)
          | RefUngrounded (c1, c2, op) ->
               sprintf "is a derived object with a complete %sungrounded%s proof (%i rule boxes, %i primitive steps) that depends on an incomplete %s%s%s" bfs bfe c1 c2 bfs2 (mk_dep_name op) bfe2
      in
         eprintf "Refiner status: %s%s%s %s%t" bfs2 name bfe2 status eflush
   in
   let f item db =
      check item
   in
      apply_all f true true

let up i =
   ignore (cd (String.make (i + 1) '.'));
   ls ""

let down i =
   ignore (cd (string_of_int i));
   ls ""

(*
 * Edit a file.
 *)
let edit name = ignore (commands.edit name)

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
