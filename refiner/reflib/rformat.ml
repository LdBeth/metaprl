(*
 * Formatter like in the standard library.
 * Output is organized into boxes, each of which has an indentation.
 *
 * Commands:
 *    format_sbreak str str': soft break is taken if necessary
 *        if taken, str is printed after the current line
 *        if not, str' is printed
 *    format_break str str': hard break is takenin groups
 *        if taken, str is printed
 *        if not, str' is printed
 *
 *    format_lzone: begin a zone with no breaks
 *    format_szone: soft break zone (all or no hard breaks are taken)
 *    format_hzone: all hard breaks are taken.
 *    format_ezone: end the current zone.
 *
 *    format_pushm i: push left margin from here by i more spaces
 *    format_popm: pop last pushm
 *
 *    format_char: add a single char
 *    format_int: print a number
 *    format_string: add a string to the buffer
 *
 * ----------------------------------------------------------------
 *
 * This file is part of Nuprl-Light, a modular, higher order
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

open Printf

open Nl_debug

(*
 * Show the file loading.
 *)
let _ =
   if !debug_load then
      eprintf "Loading Rformat%t" eflush

let debug_dform =
   create_debug (**)
      { debug_name = "dform";
        debug_description = "Display display-form operations";
        debug_value = false
      }

let debug_simple_print =
   create_debug (**)
      { debug_name = "simple_print";
        debug_description = "Display simple printing operations";
        debug_value = false
      }

(************************************************************************
 * TYPES                                                                *
 ************************************************************************)

(*
 * A print command is
 *   1. a string of text,
 *   2. a hard Break, which contains:
 *      a. the number of the corresponding zone
 *      b. a string to append to the current line if the break is taken
 *      c. a string to append if it is not
 *   3. a soft SBreak, which contains:
 *      a. a unique number
 *      b. a string to append to the current line if the break is taken
 *      c. a string to append if it is not
 *   4. zone control
 *   5. left margin control
 *   6. an inline buffer, which contains
 *      a. a number ofr top level hard breaks
 *      b. contents of the buffer
 *)
type print_command =
   (* Printing text, keep the length *)
   Text of int * string

   (*
    * Line breaks; each break is assigned a number,
    * and we keep track of the string lengths.
    *)
 | Break of int * int * int * string * string
 | IBreak of int * int * int * string * string
 | SBreak of int * int * int * string * string
 | HBreak

   (* Break zones plus zone numbers *)
 | LZone
 | HZone of int
 | SZone of int
 | EZone of int

   (* Margin control *)
 | PushM of int
 | PopM

   (* Recursive buffer *)
 | Inline of buffer

(*
 * An output buffer contains all the input data as:
 *    commands @ (rev commands')
 *
 * zone_number: the current zone number
 * szone_number: the current number for soft breaks
 *
 * parents: this is a set of parents that use this buffer.
 * children: this is a list of buffers used by this buffer
 *     (this is for computing break offsets)
 *)
and buffer =
   { mutable commands : print_command list;
     mutable commands' : print_command list;

     (* Keep track of zone numbers for spcifying breaks *)
     mutable zone_numbers : int list;
     mutable szone_number : int;

     (* Links for recursive buffers *)
     mutable parents : buffer list;
     mutable children : buffer list
   }

(*
 * We also contruct trees of line break vectors.
 *)
type break_tree =
   BreakNode of (bool array) * (break_tree list)

(************************************************************************
 * IMPLEMENTATION                                                       *
 ************************************************************************)

(*
 * New buffer.
 *)
let new_buffer () =
   { commands = [];
     commands' = [];
     zone_numbers = [];
     szone_number = 1;
     parents = [];
     children = []
   }

(*
 * Linking.  The children are collected with duplicates.
 * The parents have a single copy.
 *)
let add_child buf buf' =
   buf.children <- buf'::buf.children;
   if not (List.memq buf buf'.parents) then
      buf'.parents <- buf::buf'.parents

(*
 * This is called when buf' is removed from buf.
 *)
let remove_parent buf' buf =
   buf.parents <- List_util.removeq buf' buf.children

(*
 * Empty the buffer.
 * The parents are unchanged.
 *)
let clear_buffer buf =
   buf.commands <- [];
   buf.commands' <- [];
   buf.zone_numbers <- [];
   buf.szone_number <- 0;

   (* Children have been removed *)
   List.iter (remove_parent buf) buf.children;
   buf.children <- []

(*
 * Compute the minimum horizontal space used by the list of items.
 *)
let get_nspace =
   let rec aux nspace = function
      [] -> nspace
    | h::t ->
         begin
            match h with
               Text (i, _) -> aux (nspace + i) t
             | HBreak -> nspace
             | SBreak (_, take, _, _, _) -> nspace + take
             | Break (_, take, _, _, _) -> nspace + take
             | IBreak (_, take, _, _, _) -> nspace + take
             | _ -> aux nspace t
         end
   in
      aux 0

(*
 * Collect the zones that are referenced.
 *)
let referenced_zones zones prog =
   let collect = function
      Break (i, _, _, _, _) ->
         zones.(i) <- true
    | _ ->
         ()
   in
      List.iter collect prog;
      zones

(*
 * Remove the zones that never are referenced.
 *)
let remove_extra_zones zones prog =
   let rec strip = function
      h::t ->
         begin
            match h with
               SZone i ->
                  if zones.(i) then
                     h :: strip t
                  else
                     strip t
             | _ ->
                  h :: strip t
         end
    | [] ->
         []
   in
      strip prog

(*
 * Optimize the prog.
 *)
let optimize_prog zones prog =
   remove_extra_zones (referenced_zones zones prog) prog

(*
 * Normalize the commands.
 *)
let normalize_buffer buf =
   if buf.commands' <> [] then
      begin
         buf.commands <- (buf.commands @ (List.rev buf.commands'));
         buf.commands' <- []
      end

(************************************************************************
 * INPUT                                                                *
 ************************************************************************)

(*
 * Add a command.
 *)
let push_command buf = function
   Text (i, t) ->
      (* Compress test *)
      buf.commands' <-
         begin
            match buf.commands' with
               (Text (i', t'))::tl -> (Text (i + i', t' ^ t))::tl
             | l -> (Text (i, t))::l
         end
 | com ->
      (* Other commands are just pushed *)
      buf.commands' <- com::buf.commands'

(*
 * Zone pushing.
 *)
let incr_zone buf =
   let i = buf.szone_number + 1 in
      buf.szone_number <- i;
      i

let zone_number buf =
   match buf.zone_numbers with
      [] -> 0
    | i::_ -> i

let push_zone buf =
   let i = incr_zone buf in
      buf.zone_numbers <- i::buf.zone_numbers;
      i

let pop_zone buf =
   match buf.zone_numbers with
      [] -> 0
    | i::t ->
         buf.zone_numbers <- t;
         i

let format_lzone buf =
   push_command buf LZone

let format_hzone buf =
   push_command buf (HZone (push_zone buf))

let format_szone buf =
   push_command buf (SZone (push_zone buf))

let format_ezone buf =
   push_command buf (EZone (pop_zone buf))

(*
 * Add breaks.
 *)
let format_sbreak buf str str' =
   let l = String.length str in
   let l' = String.length str' in
      push_command buf (SBreak (incr_zone buf, l, l', str, str'))

let format_break buf str str' =
   let l = String.length str in
   let l' = String.length str' in
      push_command buf (Break (zone_number buf, l, l', str, str'))

let format_ibreak buf str str' =
   let l = String.length str in
   let l' = String.length str' in
      push_command buf (IBreak (zone_number buf, l, l', str, str'))

let format_newline buf =
   push_command buf HBreak

let format_space buf =
   format_sbreak buf "" " "

let format_hspace buf =
   format_break buf "" " "

(*
 * Indentation pushing.
 *)
let format_pushm buf i =
   push_command buf (PushM i)

let format_popm buf =
   push_command buf PopM

(*
 * Actual printing.
 *)
let format_char buf c =
   if c = '\n' then
      format_newline buf
   else
      push_command buf (Text (1, String.make 1 c))

(*
 * Check for newlines in a string.
 *)
let rec format_string buf s =
   try
       let i = String_util.strchr s '\n' in
          push_command buf (Text (i, String_util.sub "Rformat.format_string" s 0 i));
          format_newline buf;
          let l = (String.length s) - i - 1 in
             if l > 0 then
                format_string buf (String_util.sub "Rformat.format_string" s (i + 1) l)
   with
      Not_found ->
         push_command buf (Text (String.length s, s))

(*
 * Print a string, and quote it if necessary.
 *)
let format_quoted_string buf str =
   let length = String.length str in
   let rec quotep i =
      if i < length then
         match str.[i] with
            '\n'
          | '\r'
          | '\t'
          | ' '
          | '"' ->
               true
          | _ ->
               quotep (i + 1)
      else
         false
   in
   let rec format_string' i =
      if i < length then
         let c = str.[i] in
            match c with
               '\n' -> format_string buf "\\n"
             | '\r' -> format_string buf "\\r"
             | '\t' -> format_string buf "\\r"
             | '"' -> format_string buf "\\\""
          | _ -> format_char buf c
   in
   let quote_flag = (length = 0) or (quotep 0) or (str.[0] = '\'') in
      if !debug_simple_print then
         eprintf "Rformat.format_quoted_string: quote_flag: %b%t" quote_flag eflush;
      if quote_flag then
         begin
            format_char buf '"';
            format_string' 0;
            format_char buf '"'
         end
      else
         format_string buf str

(*
 * Standard int.
 *)
let format_int buf i =
   let s = string_of_int i in
      push_command buf (Text (String.length s, s))

(*
 * Num.num numbers.
 *)
let format_num buf n =
   let s = Nl_num.string_of_num n in
      push_command buf (Text (String.length s, s))

(*
 * Print a buffer.
 *)
let format_buffer buf buf' =
   add_child buf buf';
   push_command buf (Inline buf')

(************************************************************************
 * FORMATTING                                                           *
 ************************************************************************)

(*
 * Make a first pass at formatting.
 * Arguments:
 *    margins: left margin stack
 *    breaks: the line break
 *    cbreaks: the breaks in the children
 *    lzone: currently in a linear zone?
 *    curx: current column
 *    maxx: maximum column that has been used
 *    cury: current row
 *
 * Layout algorithm:
 *    1. If in linear mode and past the right margin,
 *           throw a margin error
 *    2. When text is added, add to the current column
 *    3. On HBreak, take it
 *    4. On SBreak
 *       don't take it in a linear zone
 *       otherwise, take it if the next thing would
 *          cross the right margin
 *    5. On Break
 *       take it iff the break.(i) flag is set
 *    6. On LZone
 *       do a linear zone
 *    7. On HZone
 *       set the break.(i) flag unless in a linear zone
 *    8. On SZone
 *       a. in a leanear zone, continue
 *       b. else try a linear zone, with break.(i) = false
 *       c. else do break.(i) = true
 *    9. On EZone
 *       Pop the current zone
 *    10. On PushM:
 *       Push the left margin to the current column + the margin
 *    11. On PopM
 *       Pop to the last left margin
 *
 * Return:
 *    coff: the new child zone offset
 *    curx: the current column
 *    maxx: the maximum column
 *    cury:  the current row
 *    search: perform search on the current szone
 *    scol: the column of the last szone
 *)
exception MarginError of int

let get_margin = function
   [] -> 0
 | i::_ -> i

let softcdr = function
   [] -> []
 | _::t -> t

let compute_breaks buf rmargin =
   let rec aux ((margins, breaks, refd, cbreaks, curx, maxx, cury, catch, search, nest) as args) = function
      [] ->
         BreakNode (breaks, List.rev cbreaks), curx, maxx, cury
    | info::t ->
         (* Check that we haven't reached the margin *)
         if catch & curx > rmargin then
            begin
               let _ =
                  if !debug_dform then
                     match search with
                     None ->
                        eprintf "raise MarginError with None %d > %d%t" curx rmargin eflush
                   | Some (i, col) ->
                        eprintf "raise MarginError with Some(%d, %d) %d > %d%t" i col curx rmargin eflush
               in
                  raise (MarginError curx)
            end;

         (* Set this item *)
         begin
            match info with
               Text (i, s) ->
                  (* Text *)
                  let curx' = curx + i in
                  let maxx' = max maxx curx' in
                     (* eprintf "Text: %s: %d -> %d%t" s curx curx' eflush; *)
                     aux (margins, breaks, refd, cbreaks, curx', maxx', cury, catch, search, nest) t

             | HBreak ->
                  (* Always take hard breaks *)
                  let curx' = get_margin margins in
                  let search =
                     match search with
                        Some (0, col) when curx' <= col ->
                           if !debug_dform then
                              eprintf "Restarting search: hard%t" eflush;
                           None
                      | _ ->
                           search
                  in
                     aux (margins, breaks, refd, cbreaks, curx', maxx, cury + 1, catch, search, nest) t

             | SBreak (i, take, notake, _, _) ->
                  if (notake + (get_nspace t) < rmargin) then
                     (* Don't take the break *)
                     let curx' = curx + notake in
                     let maxx' = max curx' maxx in
                        breaks.(i) = false;
                        aux (margins, breaks, refd, cbreaks, curx', maxx', cury, catch, search, nest) t
                  else
                     (* Take the break *)
                     let curx' = (get_margin margins) + take in
                     let maxx' = max maxx curx in
                        breaks.(i) <- true;
                        aux (margins, breaks, refd, cbreaks, curx', maxx', cury + 1, catch, search, nest) t

             | Break (i, take, notake, _, _) ->
                  if breaks.(i) = false then
                     (* Don't take the break *)
                     let curx' = curx + notake in
                     let maxx' = max curx' maxx in
                        breaks.(i) <- false;
                        aux (margins, breaks, refd, cbreaks, curx', maxx', cury, catch, search, nest) t

                  else
                     (* Take the break *)
                     let curx' = (get_margin margins) + take in
                     let maxx' = max maxx curx in
                     let search =
                        match search with
                           Some (0, col) when curx' <= col ->
                              if !debug_dform then
                                 eprintf "Restarting search: %d%t" i eflush;
                              None
                         | _ ->
                              search
                     in
                        breaks.(i) <- true;
                        aux (margins, breaks, refd, cbreaks, curx', maxx', cury + 1, catch, search, nest) t

             | IBreak (i, take, notake, _, _) ->
                  if breaks.(i) = false then
                     (* Don't take the break *)
                     let curx' = curx + notake in
                     let maxx' = max curx' maxx in
                        breaks.(i) = false;
                        aux (margins, breaks, refd, cbreaks, curx', maxx', cury, catch, search, nest) t

                  else
                     (* Take the break *)
                     let curx' = curx + take in
                     let maxx' = max maxx curx in
                        breaks.(i) <- true;
                        aux (margins, breaks, refd, cbreaks, curx', maxx', cury + 1, catch, search, nest) t

             | LZone ->
                  (* Try linear mode, an error signals end of format *)
                  begin
                     try aux (margins, breaks, refd, cbreaks, curx, maxx, cury, true, search, nest) t with
                        MarginError curx' ->
                           BreakNode (breaks, List.rev cbreaks), curx', maxx, cury
                  end

             | HZone i ->
                  (* Initiate a breaking zone *)
                  begin
                     breaks.(i) <- true;
                     aux (margins, breaks, refd, cbreaks, curx, maxx, cury, catch, search, nest) t
                  end

             | SZone i ->
                  (* Try out both cases *)
                  if search = None & refd.(i) then
                     begin
                        breaks.(i) <- false;
                        if !debug_dform then
                           eprintf "Try %d linear: %d, %d%t" i curx nest eflush;
                        try aux (margins, breaks, refd, cbreaks, curx, maxx, cury, true, Some (i, curx), nest + 1) t with
                           MarginError _ ->
                              breaks.(i) <- true;
                              if !debug_dform then
                                 eprintf "Fail %d break: %b%t" i catch eflush;
                              aux (margins, breaks, refd, cbreaks, curx, maxx, cury, false, None, nest) t
                     end
                  else
                     begin
                        breaks.(i) <- false;
                        aux (margins, breaks, refd, cbreaks, curx, maxx, cury, catch, search, nest) t
                     end

             | EZone i ->
                  let search =
                     match search with
                        Some (i', col) when i' = i ->
                           if !debug_dform then
                              eprintf "Closing zone %d%t" i eflush;
                           Some (0, col)
                      | _ ->
                           search
                  in
                     aux (margins, breaks, refd, cbreaks, curx, maxx, cury, catch, search, nest) t

             | PushM i ->
                  (* Push the margin *)
                  let lmargin = curx + i in
                     aux (lmargin::margins, breaks, refd, cbreaks, curx, maxx, cury, catch, search, nest) t

             | PopM ->
                  (* Pop the margin *)
                  aux (softcdr margins, breaks, refd, cbreaks, curx, maxx, cury, catch, search, nest) t

             | Inline b ->
                  (* Recursive buffer *)
                  let breaks' = Array.create (b.szone_number + 1) false in
                  let refd' = Array.create (b.szone_number + 1) false in
                  let _ = normalize_buffer b in
                  let coms = b.commands in
                  let _ = referenced_zones refd' coms in
                  let breaks'', curx', maxx', cury' =
                     aux (margins, breaks', refd', [], curx, maxx, cury, catch, search, nest) coms
                  in
                     aux (margins, breaks, refd, breaks''::cbreaks, curx', maxx', cury', catch, search, nest) t
         end
   in
   let breaks = Array.create (buf.szone_number + 1) false in
   let refd = Array.create (buf.szone_number + 1) false in
   let prog = buf.commands in
   let _ = referenced_zones refd prog in
   let breaks', _, _, _ = aux ([], breaks, refd, [], 0, 0, 1, false, None, 0) prog in
      breaks'

(*
 * "tab" to a position on the next line.
 *)
let tab printer pos =
   printer ("\n" ^ (String_util.make "Rformat.tab" pos ' '))

(*
 * Given a break vector, print out the data.
 * Return the x position.
 *)
let format_to_handler printer rmargin (BreakNode (breaks, cbreaks)) =
   (* This printer watches the right margin *)
   let print_text curx i s =
      (* Watch the right margin
      if curx < rmargin then
         if curx + i > rmargin then
            let amount = rmargin - curx - 1 in
               if amount > 0 then
                  printer (String_util.sub "Rformat.format_to_handler" s 0 amount);
               printer "$"
         else *)
            printer s
   in

   (* Print the entire box *)
   let rec aux ((margins, breaks, cbreaks, curx) as args) = function
      [] ->
         curx
    | h::t ->
         begin
            match h with
               Text (i, s) ->
                  print_text curx i s;
                  aux (margins, breaks, cbreaks, curx + i) t

             | HBreak ->
                  let lmargin = get_margin margins in
                     tab printer lmargin;
                     aux (margins, breaks, cbreaks, lmargin) t

             | SBreak (i, take, notake, str, str') ->
                  if breaks.(i) then
                     let lmargin = get_margin margins in
                        tab printer lmargin;
                        if !debug_dform then
                           print_text lmargin take ("<" ^ string_of_int i ^ ">");
                        print_text lmargin take str;
                        aux (margins, breaks, cbreaks, lmargin + take) t
                  else
                     begin
                        if !debug_dform then
                           print_text curx notake ("<" ^ string_of_int i ^ ">");
                        print_text curx notake str';
                        aux (margins, breaks, cbreaks, curx + notake) t
                     end

             | Break (i, take, notake, str, str') ->
                  if breaks.(i) then
                     let lmargin = get_margin margins in
                        tab printer lmargin;
                        if !debug_dform then
                           print_text lmargin take ("<!" ^ string_of_int i ^ ">");
                        print_text lmargin take str;
                        aux (margins, breaks, cbreaks, lmargin + take) t
                  else
                     begin
                        if !debug_dform then
                           print_text curx notake ("<!" ^ string_of_int i ^ ">");
                        print_text curx notake str';
                        aux (margins, breaks, cbreaks, curx + notake) t
                     end

             | IBreak (i, take, notake, str, str') ->
                  if breaks.(i) then
                     begin
                        print_text curx take str;
                        aux (margins, breaks, cbreaks, curx + take) t
                     end
                  else
                     begin
                        print_text curx notake str';
                        aux (margins, breaks, cbreaks, curx + notake) t
                     end

             | PushM i ->
                  aux ((curx + i)::margins, breaks, cbreaks, curx) t

             | PopM ->
                  aux (softcdr margins, breaks, cbreaks, curx) t

             | Inline b ->
                  begin
                     match cbreaks with
                        [] ->
                           raise (Invalid_argument "format_to_handler")
                      | (BreakNode (breaks', cbreaks'))::cbreaks'' ->
                           let curx' = aux (margins, breaks', cbreaks', curx) b.commands in
                              aux (margins, breaks, cbreaks'', curx') t
                  end

             | SZone i ->
                  if !debug_dform then
                     print_text curx "" ("[[" ^ string_of_int i);
                  aux args t
             | EZone i ->
                  if !debug_dform then
                     print_text curx "" (string_of_int i ^ "]]");
                  aux args t

             | _ ->
                  aux args t
         end
   in
      aux ([], breaks, cbreaks, 0)

(*
 * Print to an IO buffer.
 *)
let print_to_channel rmargin buf ch =
   let breaks = Array.create (buf.szone_number + 1) false in
   let _ = normalize_buffer buf in
   let breaks = compute_breaks buf rmargin in
      format_to_handler (output_string ch) rmargin breaks buf.commands;
      ()

(*
 * Print to a string.
 *)
let print_to_string rmargin buf =
   let buffer = ref ([] : string list) in
   let handle s =
      buffer := s :: !buffer
   in
   let rec smash s = function
      [] ->
         s
    | s'::t' ->
         smash (s' ^ s) t'
   in
   let _ = normalize_buffer buf in
   let breaks = compute_breaks buf rmargin in
      format_to_handler handle rmargin breaks buf.commands;
      smash "" !buffer

(*
 * -*-
 * Local Variables:
 * Caml-master: "manager"
 * End:
 * -*-
 *)
