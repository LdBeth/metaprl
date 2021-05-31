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
extends Summary
extends Mptop

open Lm_debug
open Lm_rformat
open Lm_rprintf

open Shell_sig
open Shell_util
open Shell_internal_sig
open Shell_core

(*
 * Default command set.
 *)
let uninitialized _ =
   raise (Invalid_argument "The Shell module has not been instantiated")

let commands =
   { initialized = false;
     sync = uninitialized;
     parse_arg = uninitialized, uninitialized;
     init = uninitialized;
     refresh = uninitialized;
     pwd = uninitialized;
     set_dfmode = uninitialized;
     create_pkg = uninitialized;
     backup = uninitialized;
     backup_all = uninitialized;
     extract = (fun _ -> uninitialized);
     get_view_options = uninitialized;
     set_view_options = uninitialized;
     clear_view_options = uninitialized;
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

(*
 * Call the commands.
 *)
let extract path () = commands.extract path ()
let term_of_extract ts = commands.sync (term_of_extract ts)

let wrap_arg cmd arg = commands.sync (cmd commands.parse_arg arg)
let wrap_arg_unit cmd () = commands.sync (cmd commands.parse_arg)

let init () = commands.init ()
let cd = wrap_arg cd
let refresh () = commands.refresh ()
let pwd () = commands.pwd ()
let relative_pwd () = commands.sync relative_pwd
let fs_pwd () = commands.sync fs_pwd
(* let items _ = commands.items () XXX: Temporary *)
let set_dfmode s = commands.set_dfmode s
let create_pkg s = commands.create_pkg s
let backup _ = commands.backup ()
let backup_all _ = commands.backup_all ()
let save = wrap_arg_unit save
let save_all = wrap_arg_unit save_all
let export = wrap_arg_unit export
let export_all = wrap_arg_unit export_all
let revert = wrap_arg_unit revert
let revert_all = wrap_arg_unit revert_all
let abandon = wrap_arg_unit abandon
let abandon_all = wrap_arg_unit abandon_all
let check _ = commands.sync check
let expand _ = commands.sync expand
let apply_all f b g h = commands.sync (apply_all commands.parse_arg f b g h)
let undo _ = commands.sync undo
let redo _ = commands.sync redo
let create_ax_statement t s = commands.sync (create_ax_statement commands.parse_arg t s)
let refine t = commands.sync (refine t)
let print_theory = wrap_arg print_theory
let get_view_options _ = commands.get_view_options ()
let set_view_options s = commands.set_view_options s
let clear_view_options s = commands.clear_view_options s
let find_subgoal i = commands.sync (edit_find i)
let is_enabled name = commands.sync (edit_is_enabled name)

let interpret cmd = commands.sync (interpret cmd)
let kreitz _ = interpret ProofKreitz
let clean _ = interpret ProofClean
let squash _ = interpret ProofSquash
let copy s = interpret (ProofCopy s)
let paste s = interpret (ProofPaste s)
let make_assum _ = interpret ProofMakeAssum

let interpret_all command modifies =
   let f item db =
      item#edit_interpret [] command
   in
      apply_all f true dont_clean_item dont_clean_module

let expand_all _ = interpret_all ProofClean false
let clean_all _ = interpret_all ProofClean false
let squash_all _ = interpret_all ProofSquash false

let root = wrap_arg_unit root

(*
 * Toploop functions
 *)
let exit () = raise End_of_file

let abort () =
   Stdlib.exit 126

let set_debug = set_debug

let println s =
   printf "%s%t" s eflush

let eprintln s =
   eprintf "%s%t" s eflush

let print_gc_stats () =
   Lm_rprintf.flush stdout;
   Gc.print_stat Stdlib.stdout;
   Stdlib.flush Stdlib.stdout

let gc_compact () =
   Gc.compact ();
   Gc.print_stat Stdlib.stdout;
   Stdlib.flush Stdlib.stdout

let set_tex_file = Shell_tex.set_file

let ls s =
   let options = ls_options_of_string s in
   let options =
      if LsOptionSet.is_empty options then
         ls_options_default
      else
         options
   in
      commands.sync (view options)

let status item =
   let name, status, _, _ = item#edit_get_contents [] in
   let str_status =
      match status with
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

let status_all_aux clean_item clean_module =
   let f item db =
      eprintf "Expanding `%s':%t" (let name, _, _, _ = item#edit_get_contents [] in name) eflush;
      let modified =
         try item#edit_interpret [] ProofExpand with
            Invalid_argument _
          | _ ->
               false
      in
         status item;
         modified
   in
      apply_all f true clean_item clean_module

let status_all () =
   status_all_aux dont_clean_item dont_clean_module

let status_and_abandon_all () =
   status_all_aux clean_resources clean_and_abandon

(* For debugging memory usage *)
let debug_status_all () =
   let f item db =
      let modified =
         try item#edit_interpret [] ProofExpand with
            Invalid_argument _
          | _ ->
               false
      in
         modified
   in
      apply_all f true clean_resources clean_and_abandon

declare refiner_status[name:s] : Dform
declare status_bf[s] : Dform

dform refiner_status_df : except_mode[src] :: refiner_status[name:s] =
   info["Refiner status:"] space bf[name] space

dform refiner_status_df : mode[src] :: refiner_status[name:s] =
   `"Refiner status: " slot[name] `" "

dform status_df : except_mode[src] :: status_bf[s] =
   bf[s]

dform status_df : mode[src] :: status_bf[s] =
   `"*" slot[s] `"*"

let check_all () =
   let check item db =
      let name, _, _, _ = item#edit_get_contents [] in
      let buf = new_buffer () in
      let modified, status = item#edit_check in
         format_szone buf;
         format_pushm buf 3;
         Dform.format_term db buf <:con< refiner_status[$name$:s] >>;
         begin match status with
            RefPrimitive ->
               format_string buf "is a primitive axiom"
          | RefIncomplete (c1, c2) ->
               format_string buf "is a derived object with an ";
               Dform.format_term db buf << status_bf["incomplete"] >>;
               format_string buf (sprintf " proof (%i rule boxes, %i primitive steps)" c1 c2)
          | RefComplete (c1, c2, l) ->
               format_string buf (sprintf "is a derived object with a complete grounded proof (%i rule boxes, %i primitive steps, %i dependencies)" c1 c2 (List.length l))
          | RefUngrounded (c1, c2, op) ->
               format_string buf "is a derived object with a complete ";
               Dform.format_term db buf << status_bf["ungrounded"] >>;
               format_string buf (sprintf " proof (%i rule boxes, %i primitive steps) that depends on an incomplete " c1 c2);
               Dform.format_term db buf <:con< status_bf[$mk_dep_name op$:s] >>
         end;
         format_popm buf;
         format_ezone buf;
         format_newline buf;
         print_rbuffer buf;
         modified
   in
      apply_all check true dont_clean_item dont_clean_module

(*
 * Navigation
 *)
let up i =
   ignore (cd (String.make (i + 1) '.'));
   ls ""

let down i =
   ignore (cd (string_of_int i));
   ls ""

let yank () =
   cd (Shell_register.top ())
let swap () =
   cd (Shell_register.swap ())
let prev i =
   cd (Shell_register.prev i)
let next i =
   cd (Shell_register.next i)

let push () =
   let path = pwd () in
      Shell_register.push path;
      path

let pop = Shell_register.pop
let clear = Shell_register.clear

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
