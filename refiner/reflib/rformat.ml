(*
 * Formatter like in the standard library.
 * Output is organized into boxes, each of which has an indentation.
 *
 * Commands:
 *    format_sbreak str str': soft break is taken if necessary
 *        if taken, str is printed after the current line
 *        if not, str' is printed
 *    format_cbreak str str': soft break is taken only if the very next word causes a break
 *        if taken, str is printed after the current line
 *        if not, str' is printed
 *    format_hbreak str str': hard breaks are taken in groups
 *        if taken, str is printed
 *        if not, str' is printed
 *
 *    format_lzone: begin a zone with no breaks
 *    format_szone: soft break zone (all or no hard breaks are taken)
 *    format_hzone: all hard breaks are taken.
 *    format_ezone: end the current zone.
 *    format_izone: start an invisible zone.
 *    format_tzone: start a tagged zone
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
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/index.html for information on Nuprl,
 * OCaml, and more information about this system.
 *
 * Copyright (C) 1998, 1999 Jason Hickey, Cornell University
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
 * Author: Jason Hickey <jyh@cs.cornell.edu>
 * Modified by: Aleksey Nogin <nogin@cs.cornell.edu>
 *)
open Printf

open Lm_debug

(*
 * Show the file loading.
 *)
let _ =
   show_loading "Loading Rformat%t"

let debug_rformat =
   create_debug (**)
      { debug_name = "rformat";
        debug_description = "display text formatting operations";
        debug_value = false
      }

let default_width = 80

(************************************************************************
 * TYPES                                                                *
 ************************************************************************)

(*
 * Identify the zone types.
 *)
type zone_tag =
   LZoneTag
 | HZoneTag
 | SZoneTag
 | IZoneTag
 | TZoneTag of string
 | MZoneTag of int * string

(*
 * A print command is
 *   1. a string of text,
 *   2. a Break
 *      Either all Breaks are taken in a zone, or all are not
 *      a. the number of the corresponding zone
 *      b. the length to begin the next line if the break is taken
 *      c. the length to append to the current line if it is not
 *      d. a string to insert on the next line if the break is taken
 *      e. a string to append if it is not
 *
 *   4. zone control
 *      LZone: no line breaks are allowed in a linear zone
 *      HZone: all Breaks are takes in a hard zone
 *      SZone: either all Breaks are taken, or they are not
 *      IZone: no lines breaks or margin control
 *      MZone: push the left margin to the current column plus the offset
 *)
type print_command =
   (* Printing text, keep the length *)
   Text of int * string

   (*
    * Break contains a break number, and the line breaking text.
    * HBreak is a break that is always taken.
    *)
 | Break of int * int * int * string * string
 | CBreak of int * int * int * string * string
 | HBreak

   (*
    * Inlined buffer.
    *)
 | Inline of buffer

(*
 * This is the info that is computed once a buffer has
 * been formatted.
 *)
and formatted_info =
   { formatted_commands : print_command list;
     formatted_breaks : bool array;     (* Flags for breaks that were taken *)
     formatted_col : int;               (* Starting column *)
     formatted_maxx : int;              (* Max layout column *)
     formatted_lmargin : int * string;  (* Left margin provided by environment *)
     formatted_search : bool            (* Was this zone formatted in linear mode? *)
   }

(*
 * This is the info for buffers that are being constructed.
 *   formatting_commands: commands that have been added (reversed)
 *   formatting_index: total number of breaks inserted so far
 *   formatting_buf: current buffer being inserted into
 *)
and unformatted_info =
   { unformatted_commands : print_command list;
     unformatted_index : int
   }

and formatting_info =
   { mutable formatting_commands : print_command list;
     mutable formatting_index : int;
     formatting_buf : buffer
   }

and formatting_stack =
   { mutable formatting_stack : formatting_info list }

(*
 * This is the info collected for the buffer.
 *)
and format_info =
   Formatted of formatted_info
 | Unformatted of unformatted_info
 | Formatting of formatting_stack

(*
 * Input data is formatted on a stack.
 *)
and buffer =
   { (* What format is this zone *)
     buf_tag : zone_tag;

     (* Name of this zone *)
     buf_index : int;

     (* Compiled info about this buffer *)
     mutable buf_info : format_info;

     (* Parent buffer to notify when the format changes *)
     buf_parent : buffer option;

     (* Save the root buffer *)
     buf_root : root
   }

(*
 * The root contains info for general formatting.
 *    root_index: zones are given unique numbers,
 *       the root_index is the index of the next zone
 *    root_bound: max number of visible characters in the buffer
 *    root_count: number of visible characters in the buffer
 *)
and root =
   { mutable root_index : int;
     mutable root_bound : int;
     mutable root_count : int
   }

(*
 * A printer contains a tabbing function,
 * a function to print to strings,
 * and a function to print tags.
 *
 * The second argument to print_tab is the list of boxes
 * (inner to outher) that need to closed and restarted.
 *)
type printer =
   { print_string : string -> unit;
     print_invis : string -> unit;
     print_tab : int * string -> string list -> unit;
     print_begin_tag : string -> unit;
     print_end_tag : string -> unit;
   }

(*
 * Buffer overflow is raised when too much visible text is
 * put in the buffer.
 *)
exception BufferOverflow

(************************************************************************
 * IMPLEMENTATION                                                       *
 ************************************************************************)

(*
 * Default empty buffer.
 *)
let empty_info = Unformatted { unformatted_commands = []; unformatted_index = 0 }

(*
 * Aleksey's hack to limit the left margin.
 *)
let lcol_const = 3

(*
 * Create a new empty buffer.
 *)
let new_buffer () =
   let root =
      { root_index = 0;
        root_bound = max_int;
        root_count = 0
      }
   in
      { buf_tag = SZoneTag;
        buf_index = 0;
        buf_info = empty_info;
        buf_parent = None;
        buf_root = root
      }

(*
 * Empty the buffer.
 * The parents are unchanged.
 *)
let clear_buffer buf =
   buf.buf_info <- empty_info

(*
 * Switch this buffer to formatting mode.
 *)
let get_formatting_stack buf =
   match buf.buf_info with
      Formatting info ->
         info
    | Unformatted { unformatted_commands = commands;
                    unformatted_index = index
      } ->
         let stack =
            { formatting_stack =
                 [{ formatting_commands = commands;
                    formatting_index = index;
                    formatting_buf = buf
                  }]
            }
         in
            buf.buf_info <- Formatting stack;
            stack
    | Formatted { formatted_commands = commands;
                  formatted_breaks = breaks
      } ->
         let stack =
            { formatting_stack =
                 [{ formatting_commands = commands;
                    formatting_index = Array.length breaks;
                    formatting_buf = buf
                  }]
            }
         in
            buf.buf_info <- Formatting stack;
            stack

(*
 * End the formatting mode.
 *)
let flush_formatting buf =
   match buf.buf_info with
      Formatting stack ->
         let rec flush = function
            [{ formatting_commands = commands;
               formatting_index = index
             }] ->
               buf.buf_info <- Unformatted { unformatted_commands = List.rev commands;
                                             unformatted_index = index
                               }
          | _ ->
               raise (Invalid_argument "Rformat.flush_formatting: unballanced buffer (use debug_dform_depth to debug)")
         in
            flush stack.formatting_stack

    | Unformatted _
    | Formatted _ ->
         ()

(*
 * Depth of nesting.
 *)
let zone_depth = function
   { buf_info = Formatting stack } ->
      List.length stack.formatting_stack - 1
 | _ ->
      0

(*
 * Start a new zone.
 *)
let push_zone buf tag =
   let stack = get_formatting_stack buf in
   let root = buf.buf_root in
   let index = succ root.root_index in
   let buf' =
      { buf_tag = tag;
        buf_index = index;
        buf_info = empty_info;
        buf_parent = Some buf;
        buf_root = root
      }
   in
   let _ =
      match stack.formatting_stack with
         head :: _ ->
            head.formatting_commands <- Inline buf' :: head.formatting_commands
       | [] ->
            raise (Invalid_argument "Rformat.push_zone")
   in
   let info =
      { formatting_commands = [];
        formatting_index = 0;
        formatting_buf = buf'
      }
   in
      root.root_index <- index;
      stack.formatting_stack <- info :: stack.formatting_stack

let format_lzone buf =
   push_zone buf LZoneTag

let format_hzone buf =
   push_zone buf HZoneTag

let format_szone buf =
   push_zone buf SZoneTag

let format_izone buf =
   push_zone buf IZoneTag

let format_tzone buf tag =
   push_zone buf (TZoneTag tag)

let format_pushm buf off =
   let off =
      if off < 0 then
         begin
            eprintf "Rformat.format_pushm: negative margin %d%t" off eflush;
            0
         end
      else
         off
   in
      push_zone buf (MZoneTag (off, String.make off ' '))

let format_pushm_str buf s =
   push_zone buf (MZoneTag (String.length s, s))

(*
 * End the zone by popping the last entry off the stack.
 *)
let format_ezone buf =
   let stack = get_formatting_stack buf in
      match stack.formatting_stack with
         { formatting_commands = commands;
           formatting_index = index;
           formatting_buf = buf'
         } :: ((head :: _) as tail) ->
            let info =
               { unformatted_commands = List.rev commands;
                 unformatted_index = index
               }
            in
               buf'.buf_info <- Unformatted info;
               stack.formatting_stack <- tail
       | _ ->
            raise (Invalid_argument "Rformat.format_ezone (AKA format_popm): unballanced buffer (use debug_dform_depth to debug)")

let format_popm = format_ezone

(*
 * Get the head entry on the stack.
 *)
let get_formatting_head buf =
   match (get_formatting_stack buf).formatting_stack with
      head :: _ ->
         head
    | [] ->
         raise (Invalid_argument "Rformat.get_formatting_head")

(*
 * Push a command onto the stack.
 *)
let push_command buf command =
   let entry = get_formatting_head buf in
   let len =
      match command with
         Text (len, _) -> len
       | _ -> 0
   in
   let root = buf.buf_root in
   let { root_bound = bound;
         root_count = count
       } = root
   in
      entry.formatting_commands <- command :: entry.formatting_commands;
      if count >= bound then begin
         let stack = get_formatting_stack buf in
         while List.length stack.formatting_stack > 2 do format_ezone buf done;
         raise BufferOverflow
      end;
      root.root_count <- count + len

(*
 * Set the bound in the buffer.
 * If you set this, you must be prepared to catch BufferOverflow.
 *)
let format_bound buf bound =
   buf.buf_root.root_bound <- bound

(*
 * Add breaks.
 *)
exception NoBinder

let rec get_soft_binder buf =
   let rec search = function
      head :: tl ->
         let buf = head.formatting_buf in
         let index =
            match buf.buf_tag with
               LZoneTag ->
                  raise NoBinder
             | IZoneTag ->
                  raise(Invalid_argument "dform error: soft break in izone")

             | SZoneTag
             | HZoneTag ->
                  (* Allocate a new binding occurrence *)
                  let index = succ head.formatting_index in
                     head.formatting_index <- index;
                     index

             | MZoneTag _ | TZoneTag _ ->
                  (* These zones are invisible to binders *)
                  search tl
         in
            index
    | [] ->
         (* No binding occurrence *)
         raise NoBinder
   in
      search (get_formatting_stack buf).formatting_stack

let rec get_hard_binder buf =
   let rec search = function
      head :: tl ->
         let buf = head.formatting_buf in
         let index =
            match buf.buf_tag with
               LZoneTag ->
                  raise NoBinder
             | IZoneTag ->
                  raise(Invalid_argument "dform error: hard break in izone")

             | HZoneTag
             | SZoneTag ->
                  (* Return the hard break binder *)
                  0

             | MZoneTag _ | TZoneTag _ ->
                  (* These zones are invisible to binders *)
                  search tl
         in
            index
    | [] ->
         (* No binding occurrence *)
         raise NoBinder
   in
      search (get_formatting_stack buf).formatting_stack

let format_raw_string buf s =
   push_command buf (Text (String.length s, s))

let format_cbreak buf str str' =
   let l = String.length str in
   let l' = String.length str' in
      try
         push_command buf (CBreak (get_soft_binder buf, l, l', str, str'))
      with
         NoBinder ->
            format_raw_string buf " "

let format_sbreak buf str str' =
   let l = String.length str in
   let l' = String.length str' in
      try
         push_command buf (Break (get_soft_binder buf, l, l', str, str'))
      with
         NoBinder ->
            format_raw_string buf " "

let format_hbreak buf str str' =
   let l = String.length str in
   let l' = String.length str' in
      try
         push_command buf (Break (get_hard_binder buf, l, l', str, str'))
      with
         NoBinder ->
            format_raw_string buf " "

let format_newline buf =
   push_command buf HBreak

let format_space buf =
   format_sbreak buf "" " "

let format_hspace buf =
   format_hbreak buf "" " "

(*
 * Actual printing.
 *)
let format_char buf c =
   if c = '\n' then
      format_newline buf
   else
      push_command buf (Text (1, String.make 1 c))

(*
 * Check for newlines in the string.
 *)
let rec format_string buf s =
   try
       let i = String.index s '\n' in
          push_command buf (Text (i, Lm_string_util.sub "Rformat.format_string" s 0 i));
          format_newline buf;
          let l = (String.length s) - i - 1 in
             if l > 0 then
                format_string buf (Lm_string_util.sub "Rformat.format_string" s (i + 1) l)
   with
      Not_found ->
         format_raw_string buf s

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
          | '\034' ->
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
             | '\034' -> format_string buf "\\\034"
             | _ -> format_char buf c
   in
   let quote_flag = (length = 0) or (quotep 0) or (str.[0] = '\'') in
      if quote_flag then
         begin
            format_char buf '\034';
            format_string' 0;
            format_char buf '\034'
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
   let s = Lm_num.string_of_num n in
      push_command buf (Text (String.length s, s))

(************************************************************************
 * FORMATTING                                                           *
 ************************************************************************)

(*
 * This first part doesn't do the actual formatting, it just computes
 * which line breaks should be taken.
 *
 * For each zone, there is a function to compute line breaks for its
 * content.
 *
 * The arguments are:
 *    lmargin: position of the left margin
 *    rmargin: maximum width of the line
 *    col: current position in the line
 *    search: true if an exception should be raised when the right margin is exceeded
 *       if this is true, we are in a linear search
 *
 * The result is a tuple:
 *    col: the position in the current line
 *)

exception MarginError

(*
 * Build a margin string.
 * str1 is the margin prefix
 * space is the number of additional chars to before the suffix
 * str2 is the margin suffix
 *
 * Note that space may be nagative in cases like this:
 *    pushm[5] pushm[3]
 *
 * We allow this case, but print a warning message.
 * In this case, the strings should overlap.  The margins are always
 * positive, so we are guaranteed that the overlap can be taken from
 * the first margin.
 *)
let margin_string str1 space str2 =
   if space > 0 then
      str1 ^ String.make space ' ' ^ str2
   else if space = 0 then
      str1 ^ str2
   else
      (* Remove <space> chars of overlap *)
      let space = -space in
      let len1 = String.length str1 in
      let len2 = String.length str2 in
         assert(len1 >= space);
         let overlap = min space len2 in
         let s1 = String.sub str1 (len1 - space) overlap in
         let s2 = String.sub str2 0 overlap in
            if s1 <> s2 then
               eprintf "Rformat.margin_string: strings do not overlap: \"%s\":%d:\"%s\"%t" (**)
                  (String.escaped str1) space (String.escaped str2) eflush;
            String.sub str1 0 (len1 - space) ^ str2

(*
 * Get the info from the buffer.
 *)
let get_unformatted buf =
   match buf.buf_info with
      Unformatted info ->
         info
    | Formatted { formatted_commands = commands;
                  formatted_breaks = breaks
      } ->
         { unformatted_commands = commands;
           unformatted_index = Array.length breaks
         }
    | Formatting _ ->
         raise (Invalid_argument "Rformat.get_unformatted")

(*
 * Get format info from the buffer.
 *)
let get_formatted buf =
   match buf.buf_info with
      Formatted info ->
         info
    | Unformatted { unformatted_commands = commands;
                    unformatted_index = index
      } ->
         { formatted_commands = commands;
           formatted_breaks = Array.create (succ index) false;
           formatted_col = 0;
           formatted_maxx = 0;
           formatted_lmargin = 0, "";
           formatted_search = false
         }
    | Formatting _ ->
         raise (Invalid_argument "Rformat.get_formatted")

(*
 * Search for the next Text item.
 *)
let rec next_text_len stack = function
   Text (len, _) :: _ when len > 0 ->
      len
 | Inline buf :: t when buf.buf_tag <> IZoneTag ->
      let { unformatted_commands = commands } = get_unformatted buf in
         next_text_len (t :: stack) commands
 | _ :: t ->
      next_text_len stack t
 | [] ->
      match stack with
         t :: tl ->
            next_text_len tl t
       | [] ->
            0

(*
 * Breaks are never taken in a linear zone.
 *)
let rec search_lzone buf lmargin rmargin col maxx search =
   if !debug_rformat then
      eprintf "Rformat.search_lzone%t" eflush;
   let rec collect col maxx = function
      h :: t ->
         let col, maxx =
            match h with
               Text (len, _) ->
                  let col = col + len in
                     col, max col maxx
             | Break (_, _, notake_len, _, _) ->
                  let col = col + notake_len in
                     col, max col maxx
             | CBreak (_, _, notake_len, _, _) ->
                  let col = col + notake_len in
                     col, max col maxx
             | HBreak ->
                  let col = succ col in
                     col, max col maxx
             | Inline buf' ->
                  search_lzone buf' lmargin rmargin col maxx search
         in
            if search && col >= rmargin then
               raise MarginError;
            collect col maxx t
    | [] ->
         col, maxx
   in
   let { unformatted_commands = commands } = get_unformatted buf in
   let col, maxx = collect col maxx commands in
   let formatted =
      { formatted_commands = commands;
        formatted_breaks = [||];
        formatted_col = col;
        formatted_maxx = maxx;
        formatted_lmargin = lmargin;
        formatted_search = search
      }
   in
      buf.buf_info <- Formatted formatted;
      col, maxx

(*
 * This is the generic search for a non-linear zone.
 * The break array is passed as an argument, and this
 * function is both for tagged zones, and as the inner
 * search of hard/soft breaking zones.
 *)
and search_tzone buf stack ((lmargin, _) as lmargin') rmargin col maxx breaks search =
   if !debug_rformat then
      eprintf "Rformat.search_tzone%t" eflush;
   let rec collect col maxx search = function
      h :: t ->
         begin
            match h with
               Text (len, _) ->
                  let col = col + len in
                     if search && col >= rmargin then
                        raise MarginError;
                     collect col (max col maxx) search t

             | CBreak (index, take_len, notake_len, _, _) ->
                  if !debug_rformat then
                     eprintf "CBreak col=%d rmargin=%d search=%b index=%d breaks=%b%t" col rmargin search index breaks.(index) eflush;
                  if search then
                     (* Searching for margin error in linear mode *)
                     let col = col + notake_len in
                        if col >= rmargin then
                           raise MarginError;
                        collect col (max col maxx) true t

                  else if breaks.(index) then
                     (* Break has been chosen, and we are not searching *)
                     collect (lmargin + take_len) maxx search t

                  else
                     let len = next_text_len stack t in
                     let col' = col + notake_len in
                        if col' + len >= rmargin then
                           begin
                              breaks.(index) <- true;
                              collect (lmargin + take_len) maxx false t
                           end
                        else
                           collect (col + notake_len) (max col' maxx) false t

             | Break (index, take_len, notake_len, _, _) ->
                  if !debug_rformat then
                     eprintf "Break col=%d rmargin=%d search=%b index=%d breaks=%b%t" col rmargin search index breaks.(index) eflush;
                  if search then
                     (* Searching for margin error in linear mode *)
                     let col = col + notake_len in
                        if col >= rmargin then
                           raise MarginError;
                        collect col (max col maxx) true t

                  else if breaks.(index) then
                     (* Break has been chosen, and we are not searching *)
                     collect (lmargin + take_len) maxx search t

                  else
                     (* Not searching, and break hasn't been tried yet *)
                     (try
                         let col = col + notake_len in
                            collect col (max col maxx) true t
                      with
                         MarginError ->
                            breaks.(index) <- true;
                            collect (lmargin + take_len) maxx false t)

             | HBreak ->
                  (* Hard breaks are always taken *)
                  collect lmargin maxx search t

             | Inline buf' ->
                  let col, maxx = search_zone buf' (t :: stack) lmargin' rmargin col maxx breaks search in
                     collect col maxx search t
         end

    | [] ->
         col, maxx
   in
   let { unformatted_commands = commands } = get_unformatted buf in
   let col, maxx = collect col maxx search commands in
   let formatted =
      { formatted_commands = commands;
        formatted_breaks = breaks;
        formatted_col = col;
        formatted_maxx = maxx;
        formatted_lmargin = lmargin';
        formatted_search = search
      }
   in
      buf.buf_info <- Formatted formatted;
      col, maxx

(*
 * Hard breaks are always taken in a hard zone.
 * Soft breaks are taken only if the margin would be exceeded otherwise.
 *)
and search_hzone buf stack lmargin rmargin col maxx search =
   let { unformatted_index = index } = get_unformatted buf in
   let breaks = Array.create (succ index) false in
      breaks.(0) <- true;
      search_tzone buf stack lmargin rmargin col maxx breaks search

and search_szone buf stack lmargin rmargin col maxx search =
   let { unformatted_index = index } = get_unformatted buf in
   let breaks = Array.create (succ index) false in
      if search then
         search_tzone buf stack lmargin rmargin col maxx breaks search
      else
         try search_tzone buf stack lmargin rmargin col maxx breaks true with
            MarginError ->
               breaks.(0) <- true;
               search_tzone buf stack lmargin rmargin col maxx breaks false

(*
 * Generic zone searcher.
 *)
and search_zone buf stack lmargin rmargin col maxx breaks search =
   if !debug_rformat then
      eprintf "Rformat.search_zone%t" eflush;
   match buf.buf_tag with
      LZoneTag ->
         search_lzone buf lmargin rmargin col maxx search
    | HZoneTag ->
         search_hzone buf stack lmargin rmargin col maxx search
    | SZoneTag ->
         search_szone buf stack lmargin rmargin col maxx search
    | IZoneTag ->
         (* All text inside is invisible to margin calculations *)
         col, maxx
    | MZoneTag (off, str) ->
         (* Adjust the offset, so we don't go too far right *)
         let col' = col + off in
         let col' = min col' (rmargin / lcol_const) in
         let off = col' - col in

         (* Adjust the left margin *)
         let lmargin, str' = lmargin in
         let space = col - lmargin in
         let lmargin = col + off, margin_string str' space str in
            search_tzone buf stack lmargin rmargin col maxx breaks search
    | TZoneTag _ ->
         search_tzone buf stack lmargin rmargin col maxx breaks search

(*
 * Calculate all the line breaks.
 *)
let compute_breaks buf width =
   if !debug_rformat then
      eprintf "Rformat.compute_breaks%t" eflush;
   flush_formatting buf;
   search_zone buf [] (0, "") width 0 0 [||] false

(*
 * Refresh a buffer.
 *)
let refresh_breaks buf =
   flush_formatting buf;
   match buf.buf_info with
      Formatted { formatted_breaks = breaks;
                  formatted_col = col;
                  formatted_maxx = rmargin;
                  formatted_lmargin = lmargin;
                  formatted_search = search
      } ->
         search_zone buf [] lmargin rmargin col col breaks false

    | Unformatted _
    | Formatting _ ->
         raise (Invalid_argument "Rformat.compute_breaks")

(*
 * "tab" to a position on the next line.
 *)
let tab printer pos =
   printer ("\n" ^ (Lm_string_util.make "Rformat.tab" pos ' '))

(*
 * Some empty print functions.
 *)
let print_arg1_invis _ =
   ()

let print_arg2_invis _ _ =
   ()

(*
 * Format a linear zone.
 * Stop when the right margin is exceeded.
 *)
let rec print_lzone buf rmargin col printer tags =
   if !debug_rformat then
      eprintf "Rformat.print_lzone%t" eflush;
   let rec print col = function
      h :: t ->
         begin
            match h with
               Text (len, s) ->
                  printer.print_string s;
                  print (col + len) t

             | Break (_, _, notake_len, _, notake) ->
                  printer.print_string notake;
                  print (col + notake_len) t

             | CBreak (_, _, notake_len, _, notake) ->
                  printer.print_string notake;
                  print (col + notake_len) t

             | HBreak ->
                  printer.print_string " ";
                  print (succ col) t

             | Inline buf' ->
                  print (print_zone buf' rmargin col printer true tags) t
         end

    | [] ->
         col
   in
      print col (get_formatted buf).formatted_commands

(*
 * Format a tagged zone.
 *)
and print_tzone buf rmargin col printer tags =
   if !debug_rformat then
      eprintf "Rformat.print_tzone%t" eflush;
   let { formatted_commands = commands;
         formatted_breaks = breaks;
         formatted_lmargin = ((lmargin', str) as lmargin)
       } = get_formatted buf
   in
   let rec print col = function
      h :: t ->
         begin
            match h with
               Text (len, s) ->
                  printer.print_string s;
                  print (col + len) t

             | Break (index, take_len, notake_len, take, notake) ->
                  if breaks.(index) then
                     begin
                        printer.print_tab lmargin tags;
                        printer.print_string take;
                        print lmargin' t
                     end
                  else
                     begin
                        printer.print_string notake;
                        print (col + notake_len) t
                     end

             | CBreak (index, take_len, notake_len, take, notake) ->
                  if breaks.(index) then
                     begin
                        printer.print_tab lmargin tags;
                        printer.print_string take;
                        print lmargin' t
                     end
                  else
                     begin
                        printer.print_string notake;
                        print (col + notake_len) t
                     end

             | HBreak ->
                  printer.print_tab lmargin tags;
                  print lmargin' t

             | Inline buf' ->
                  print (print_zone buf' rmargin col printer false tags) t
         end
    | [] ->
         col
   in
      print col commands

(*
 * Generic formatter.
 *)
and print_ltzone linear =
   if linear then print_lzone else print_tzone

and print_zone buf rmargin col printer linear tags =
   if !debug_rformat then
      eprintf "Rformat.print_zone%t" eflush;
   match buf.buf_tag with
      LZoneTag ->
         print_lzone buf rmargin col printer tags

    | HZoneTag
    | SZoneTag ->
         print_ltzone linear buf rmargin col printer tags

    | IZoneTag ->
         let printer =
            { printer with
              print_string = printer.print_invis;
              print_tab = print_arg2_invis;
            }
         in
            print_ltzone linear buf rmargin col printer tags

    | MZoneTag (off, str) ->
         (print_ltzone linear buf rmargin col printer tags) + off

    | TZoneTag tag ->
         printer.print_begin_tag tag;
         let col = print_ltzone linear buf rmargin col printer (tag::tags) in
            printer.print_end_tag tag;
            col

let print_buf buf rmargin printer =
   if !debug_rformat then
      eprintf "Rformat.print_buf%t" eflush;
   print_zone buf rmargin 0 printer false

(************************************************************************
 * PRINTING                                                             *
 ************************************************************************)

(*
 * Channel printer.
 *)
let make_channel_printer out =
   let print_string s =
      output_string out s
   in
   let print_tab (_, str) _ =
      output_char out '\n';
      output_string out str
   in
      { print_string = print_string;
        print_invis = print_string;
        print_tab = print_tab;
        print_begin_tag = print_arg1_invis;
        print_end_tag = print_arg1_invis;
      }

(*
 * A string printer.
 *)
let make_string_printer () =
   let strings = ref [] in
   let print_string s =
      strings := s :: !strings
   in
   let print_tab (_, str) _ =
      strings := ("\n" ^ str) :: !strings
   in
   let printer =
      { print_string = print_string;
        print_invis = print_arg1_invis;
        print_tab = print_tab;
        print_begin_tag = print_arg1_invis;
        print_end_tag = print_arg1_invis;
      }
   in
   let get_string () =
      let rec total_length i = function
         h :: t ->
            total_length (i + String.length h) t
       | [] ->
            i
      in
      let rec copy buf i = function
         h :: t ->
            let len = String.length h in
               String.blit h 0 buf i len;
               copy buf (i + len) t
       | [] ->
            ()
      in
      let strs = List.rev !strings in
      let len = total_length 0 strs in
      let buf = String.create len in
         copy buf 0 strs;
         strings := [];
         buf
   in
      get_string, printer

(*
 * We hack the indentation in the HTML printer.
 * Format the data into lines, and print the tabstops in
 * the background color.
 *
 * The prefix is the white space that is inserted to
 * get the left margin right.
 *)
type html_buffer =
   { mutable html_current_line : (bool * string) list;
     mutable html_prefix : string;
     html_out : out_channel
   }

(*
 * Have to escape special characters.
 *)
let html_escape_string s =
   let len = String.length s in
   let rec collect i j =
      if j = len then
         if i = 0 then
             s
         else if i < j then
            String.sub s i (j - i)
         else
            ""
      else
         match s.[j] with
            '<' ->
               collect_escape i j "&lt;"
          | '>' ->
               collect_escape i j "&gt;"
          | '&' ->
               collect_escape i j "&amp;"
          | _ ->
               collect i (succ j)
   and collect_escape i j s' =
      if i < pred j then
         String.sub s i (j - i) ^ s' ^ collect (succ j) (succ j)
      else
         s' ^ collect (succ j) (succ j)
   in
      collect 0 0

(*
 * Print strings.
 *)
let html_print_string buf s =
   buf.html_current_line <- (true, s) :: buf.html_current_line

let html_print_invis buf s =
   buf.html_current_line <- (false, s) :: buf.html_current_line

(*
 * Extract the entire line.
 *)
let html_line buf =
   let rec collect line = function
      (vis, h) :: t ->
         let h =
            if vis then
               html_escape_string h
            else
               h
         in
            collect (h ^ line) t
    | [] ->
         line
   in
      collect "" buf.html_current_line

let html_visible buf =
   let rec collect line = function
      (true, h) :: t ->
         collect (h ^ line) t
    | _ :: t ->
         collect line t
    | [] ->
         line
   in
      collect "" buf.html_current_line

let html_push_line buf =
   let line = html_line buf in
      output_string buf.html_out line;
      buf.html_current_line <- []

(*
 * Set up all pending tabstops.
 *)
let html_tab_line buf =
   buf.html_prefix ^ html_visible buf

(*
 * Newline.
 * Compute all pending tabstops,
 * then push the line and the new tabstop.
 *)
let html_tab buf (col, _) _ =
   if col = 0 then
      begin
         html_push_line buf;
         output_string buf.html_out "<br>\n";
         buf.html_prefix <- ""
      end
   else
      let tabline = buf.html_prefix ^ html_visible buf in
         html_push_line buf;
         let prefix =
            if col >= String.length tabline then
               tabline
            else
               String.sub tabline 0 col
         in
         let spacer = sprintf "<font color=white style=\"visibility:hidden\">%s</font>" prefix in
            buf.html_prefix <- prefix;
            output_string buf.html_out "<br>\n";
            output_string buf.html_out spacer

let html_tag buf s =
   output_string buf.html_out ("<" ^ s ^ ">")

let html_etag buf s =
   output_string buf.html_out ("</" ^ s ^ ">")

(*
 * An HTML printer.
 *)
let make_html_printer out =
   let buf =
      { html_current_line = [];
        html_out = out;
        html_prefix = ""
      }
   in
      { print_string = html_print_string buf;
        print_invis = html_print_invis buf;
        print_tab = html_tab buf;
        print_begin_tag = html_tag buf;
        print_end_tag = html_etag buf;
      }

(*
 * We hack the indentation in the HTML printer.
 * Format the data into lines, and print the tabstops in
 * the background color.
 *
 * The prefix is the white space that is inserted to
 * get the left margin right.
 *)
type tex_buffer =
   { mutable tex_current_line : (bool * string) list;
     mutable tex_prefix : string;
     tex_out : out_channel
   }

(*
 * Have to escape special characters.
 *)
let tex_escape_string linebreaks s =
   let len = String.length s in
   let rec collect i j =
      if j = len then
         if i = 0 then
             s
         else if i < j then
            String.sub s i (j - i)
         else
            ""
      else
         match s.[j] with
            ' ' ->
               collect_escape i j "\\ "
          | '_' ->
               collect_escape i j "\\_"
          | '^' ->
               collect_escape i j "{\\makehat}"
          | '&' ->
               collect_escape i j "\\&"
          | '#' ->
               collect_escape i j "\\#"
          | '[' ->
               collect_escape i j "{[}"
          | ']' ->
               collect_escape i j "{]}"
          | '{' ->
               collect_escape i j "\\{"
          | '}' ->
               collect_escape i j "\\}"
          | '\\' ->
               collect_escape_space i j "{\\backslash}"
          | '$' ->
               collect_escape i j "\\$"
          | '%' ->
               collect_escape i j "\\%"
          | '|' ->
               collect_escape i j "\\|"
          | '<' ->
               collect_escape_space i j "{\\lt}"
          | '>' ->
               collect_escape_space i j "{\\gt}"
          | _ ->
               collect i (succ j)
   and collect_esc i j s' =
      if i < j then
         String.sub s i (j - i) ^ s'
      else
         s'
   and collect_escape i j s =
      collect_esc i j (s ^ collect (succ j) (succ j))
   and collect_escape_space i j s =
      let s' = collect (succ j) (succ j) in
      let s'' =
         if s' = "" then " " ^ s' else
            match s'.[0] with
               ' ' | '\\' | '$' | '_' | '^' | '&' | '}' | '{' -> s'
          | _ ->
               " " ^ s'
      in
         collect_esc i j (s ^ s'')
   in
      collect 0 0

(*
 * Print strings.
 *)
let tex_print_string buf s =
   buf.tex_current_line <- (true, s) :: buf.tex_current_line

let tex_print_invis buf s =
   buf.tex_current_line <- (false, s) :: buf.tex_current_line

(*
 * Extract the entire line.
 *)
let tex_line buf =
   let rec collect line = function
      (vis, h) :: t ->
         collect ((if vis then (tex_escape_string true h) else h) ^ line) t
    | [] ->
         line
   in
      collect "" buf.tex_current_line

let tex_visible buf =
   let rec collect line = function
      (vis, h) :: t ->
         collect (if vis then h ^ line else line) t
    | [] ->
         line
   in
      collect "" buf.tex_current_line

let make_tag s =
   (false, "\\" ^ s ^ "{")

let tex_push_line buf tags =
   let line = tex_line buf in
      output_string buf.tex_out line;
      buf.tex_current_line <- [];
      if line <> "" && line.[String.length line - 1] != '\n' then begin
         output_string buf.tex_out (String.make (List.length tags) '}');
         output_string buf.tex_out "\\\\\n";
         buf.tex_current_line <- List.map make_tag tags
      end

(*
 * Set up all pending tabstops.
 *)
let tex_tab_line buf =
   buf.tex_prefix ^ tex_visible buf

(*
 * Newline.
 * Compute all pending tabstops,
 * then push the line and the new tabstop.
 *)
let tex_tab buf (col, _) tags =
   if col = 0 then
      begin
         tex_push_line buf tags;
         buf.tex_prefix <- ""
      end
   else
      let tabline = tex_tab_line buf in
         tex_push_line buf tags;
         let prefix =
            if col >= String.length tabline then
               tabline
            else
               String.sub tabline 0 col
         in
         let spacer = sprintf "\\phantom{%s}" (tex_escape_string false prefix) in
            buf.tex_prefix <- prefix;
            buf.tex_current_line <- buf.tex_current_line @ [false, spacer]

let tex_tag buf s =
   buf.tex_current_line <- (make_tag s) :: buf.tex_current_line

let tex_etag buf _ =
   buf.tex_current_line <- (false, "}") :: buf.tex_current_line

(*
 * A TeX printer.
 *)
let make_tex_printer out =
   let buf =
      { tex_current_line = [];
        tex_out = out;
        tex_prefix = ""
      }
   in
      { print_string = tex_print_string buf;
        print_invis = tex_print_invis buf;
        print_tab = tex_tab buf;
        print_begin_tag = tex_tag buf;
        print_end_tag = tex_etag buf;
      }

(*
 * Generic printer.
 *)
let print_to_printer buf rmargin printer =
   ignore (compute_breaks buf rmargin);
   ignore (print_buf buf rmargin printer [])

(*
 * Print to an IO buffer.
 *)
let print_to_channel rmargin buf out =
   let rmargin = Mp_term.term_width out rmargin in
      print_to_printer buf rmargin (make_channel_printer out)

let print_to_string rmargin buf =
   let get_string, printer = make_string_printer () in
      print_to_printer buf rmargin printer;
      get_string ()

(*
 * Print to HTML.
 *)
let print_to_html rmargin buf out =
   print_to_printer buf rmargin (make_html_printer out)

(*
 * TeX formatting.
 *)
let print_to_tex rmargin buf out =
   output_string out "\\iftex\\begin{tabbing}\n";
   print_to_printer buf rmargin (make_tex_printer out);
   output_string out "\\end{tabbing}\\fi\n"

(*
 * Print to a single line.
 *)
let line_format length fmt_fun =
   if length < 3 then
      raise (Invalid_argument "Rformat.line_format");
   let buf = new_buffer () in
   let s, overflow =
      format_bound buf length;
      format_lzone buf;
      let over =
         try fmt_fun buf; false
         with BufferOverflow -> true
      in
      format_ezone buf;
      print_to_string length buf, over
   in
   let len = String.length s in
   let s =
      if len > length then String.sub s 0 length
      else if overflow && (len <= (length - 3)) then (s ^ "...")
      else if overflow && (len < length) then s ^ (String.make (length - len) '.')
      else s
   in
      if (overflow && len > (length - 3)) || len > length then begin
         s.[length - 3] <- '.';
         s.[length - 2] <- '.';
         s.[length - 1] <- '.'
      end;
      s

(*
 * -*-
 * Local Variables:
 * Caml-master: "manager"
 * End:
 * -*-
 *)
