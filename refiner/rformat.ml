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
 *)

open Printf

open Debug

(*
 * Show the file loading.
 *)
let _ =
   if !debug_load then
      eprintf "Loading Rformat%t" eflush

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
     szone_number = 0;
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
 * Normalize the commands.
 *)
let normalize_buffer buf =
   if buf.commands' <> [] then
      begin
         buf.commands <- buf.commands @ (List.rev buf.commands');
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
          push_command buf (Text (i, String.sub s 0 i));
          format_newline buf;
          let l = (String.length s) - i - 1 in
             if l > 0 then
                format_string buf (String.sub s (i + 1) l)
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
   let s = Num.string_of_num n in
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
 *)
exception MarginError of int

let get_margin = function
   [] -> 0
 | i::_ -> i

let softcdr = function
   [] -> []
 | _::t -> t

let compute_breaks buf rmargin =
   let rec aux ((margins, breaks, cbreaks, lzone, curx, maxx, cury, catch) as args) = function
      [] ->
         BreakNode (breaks, List.rev cbreaks), curx, maxx, cury
    | info::t ->
         (* Check that we haven't reached the margin *)
         if catch & curx > rmargin then
            raise (MarginError curx);

         (* Set this item *)
         begin
            match info with
               Text (i, _) ->
                  (* Text *)
                  let curx' = curx + i in
                  let maxx' = max maxx curx' in
                     aux (margins, breaks, cbreaks, lzone, curx', maxx', cury, catch) t
   
             | HBreak ->
                  (* Always take hard breaks *)
                  let curx' = get_margin margins in
                     aux (margins, breaks, cbreaks, lzone, curx', maxx, cury + 1, catch) t
   
             | SBreak (i, take, notake, _, _) ->
                  if lzone or (notake + (get_nspace t) < rmargin) then
                     (* Don't take the break *)
                     let curx' = curx + notake in
                     let maxx' = max curx' maxx in
                        breaks.(i) = false;
                        aux (margins, breaks, cbreaks, lzone, curx', maxx', cury, catch) t
                  else
                     (* Take the break *)
                     let curx' = (get_margin margins) + take in
                     let maxx' = max maxx curx in
                        breaks.(i) <- true;
                        aux (margins, breaks, cbreaks, lzone, curx', maxx', cury + 1, catch) t

             | Break (i, take, notake, _, _) ->
                  if lzone or (breaks.(i) = false) then
                     (* Don't take the break *)
                     let curx' = curx + notake in
                     let maxx' = max curx' maxx in
                        breaks.(i) = false;
                        aux (margins, breaks, cbreaks, lzone, curx', maxx', cury, catch) t

                  else
                     (* Take the break *)
                     let curx' = (get_margin margins) + take in
                     let maxx' = max maxx curx in
                        breaks.(i) <- true;
                        aux (margins, breaks, cbreaks, lzone, curx', maxx', cury + 1, catch) t

             | IBreak (i, take, notake, _, _) ->
                  if lzone or (breaks.(i) = false) then
                     (* Don't take the break *)
                     let curx' = curx + notake in
                     let maxx' = max curx' maxx in
                        breaks.(i) = false;
                        aux (margins, breaks, cbreaks, lzone, curx', maxx', cury, catch) t

                  else
                     (* Take the break *)
                     let curx' = curx + take in
                     let maxx' = max maxx curx in
                        breaks.(i) <- true;
                        aux (margins, breaks, cbreaks, lzone, curx', maxx', cury + 1, catch) t

             | LZone ->
                  (* Linear zone *)
                  if lzone then
                     (* Already linear, just continue *)
                     aux args t 
                  else
                     (* Try linear mode, an error signals end of format *)
                     begin
                        try aux (margins, breaks, cbreaks, true, curx, maxx, cury, true) t with
                           MarginError curx' ->
                              BreakNode (breaks, List.rev cbreaks), curx', maxx, cury
                     end

             | HZone i ->
                  (* Initiate a breaking zone *)
                  if lzone then
                     aux args t
                  else
                     begin
                        breaks.(i) <- true;
                        aux (margins, breaks, cbreaks, false, curx, maxx, cury, catch) t
                     end

             | SZone i ->
                  (* Try out both cases *)
                  if lzone then
                     aux args t
                  else
                     begin
                        breaks.(i) <- false;
                        try aux (margins, breaks, cbreaks, true, curx, maxx, cury, true) t with
                           MarginError _ ->
                              breaks.(i) <- true;
                              aux (margins, breaks, cbreaks, false, curx, maxx, cury, catch) t
                     end

             | EZone _ ->
                  (* Don't do anything *)
                  aux args t
                           
             | PushM i ->
                  (* Push the margin *)
                  let lmargin = curx + i in
                     aux (lmargin::margins, breaks, cbreaks, lzone, curx, maxx, cury, catch) t

             | PopM ->
                  (* Pop the margin *)
                  aux (softcdr margins, breaks, cbreaks, lzone, curx, maxx, cury, catch) t

             | Inline b ->
                  (* Recursive buffer *)
                  let breaks' = Array.create (b.szone_number + 1) false in
                  let _ = normalize_buffer b in
                  let coms = b.commands in
                  let breaks'', curx', maxx', cury' =
                     aux (margins, breaks', [], lzone, curx, maxx, cury, catch) coms
                  in
                     aux (margins, breaks, breaks''::cbreaks, lzone, curx', maxx', cury', catch) t
         end
   in
   let breaks = Array.create (buf.szone_number + 1) false in
   let breaks', _, _, _ = aux ([], breaks, [], false, 0, 0, 1, false) buf.commands in
      breaks'

(*
 * "tab" to a position on the next line.
 *)
let tab printer pos =
   printer ("\n" ^ (String.make pos ' '))

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
                  printer (String.sub s 0 amount);
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
                        print_text lmargin take str;
                        aux (margins, breaks, cbreaks, lmargin + take) t
                  else
                     begin
                        print_text curx notake str';
                        aux (margins, breaks, cbreaks, curx + notake) t
                     end

             | Break (i, take, notake, str, str') ->
                  if breaks.(i) then
                     let lmargin = get_margin margins in
                        tab printer lmargin;
                        print_text lmargin take str;
                        aux (margins, breaks, cbreaks, lmargin + take) t
                  else
                     begin
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

             | _ ->
                  (* Do nothing *)
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
 * $Log$
 * Revision 1.5  1998/04/28 18:30:47  jyh
 * ls() works, adding display.
 *
 * Revision 1.4  1998/04/24 02:42:54  jyh
 * Added more extensive debugging capabilities.
 *
 * Revision 1.3  1998/04/21 19:54:16  jyh
 * Upgraded refiner for program extraction.
 *
 * Revision 1.2  1998/03/20 22:16:20  eli
 * Eli: Changed integer parameters to Num.num's.
 *
 * Revision 1.1  1997/04/28 15:51:37  jyh
 * This is the initial checkin of Nuprl-Light.
 * I am porting the editor, so it is not included
 * in this checkin.
 *
 * Directories:
 *     refiner: logic engine
 *     filter: front end to the Ocaml compiler
 *     editor: Emacs proof editor
 *     util: utilities
 *     mk: Makefile templates
 *
 * Revision 1.6  1996/05/21 02:14:12  jyh
 * This is a semi-working version before Wisconsin vacation.
 *
 * Revision 1.5  1996/03/28 02:58:22  jyh
 * Prelim checkin for an partial version of the refiner document in the
 * first version of README.tex.
 *
 * Revision 1.4  1996/03/25 20:50:38  jyh
 * Intermediate commit while modifying grammer.  Restricting
 * ML hooks to condition terms.
 *
 * Revision 1.3  1996/02/25 15:16:12  jyh
 * This is a partial checkin as filterModule is being developed.
 * After the meta-logical framework is developed, sequent.* will go away.
 *
 * Revision 1.1  1996/02/18 23:32:27  jyh
 * Changin Format module to more Nuprl-like format.
 *
 * -*-
 * Local Variables:
 * Caml-master: "manager"
 * End:
 * -*-
 *)
