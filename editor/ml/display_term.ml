(*
 * This is the standard interface to the window system.
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
 * Copyright (C) 1999 Jason Hickey, Cornell University
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
 *)

open Printf
open Mp_debug

open Refiner.Refiner.TermType
open Refiner.Refiner.Term
open Refiner.Refiner.TermOp
open Refiner.Refiner.TermMan
open Dform_print
open Simple_print

open Mux_channel

(************************************************************************
 * TYPES                                                                *
 ************************************************************************)

(*
 * A generic term window.
 *)
type t =
   { win_channel : channel;
     mutable win_width : int;
     mutable win_dfmode : string;
     win_dfbase : dform_mode_base;
     win_callback : callback;
     mutable win_dir : string;
     mutable win_term : term
   }

(*
 * A proof window has three parts.
 *)
and proof =
   { proof_goal : t;
     proof_rule : t;
     proof_subgoals : t
   }

(*
 * events in the window.
 *)
and event =
   TermSelection of term

and callback = t -> event -> unit

(************************************************************************
 * IMPLEMENTATION                                                       *
 ************************************************************************)

(*
 * Handle input events.
 *)
let handle_key window code =
   ()

let handle_mouse window x y =
   ()

let handle_size window w h =
   try
      window.win_width <- max (int_of_string w) 80
   with
      Failure "int_of_string" ->
         ()

let input_callback window _ args =
   match args with
      ["key"; code] ->
         handle_key window code
    | ["mouse"; x; y] ->
         handle_mouse window x y
    | ["size"; x; y] ->
         handle_size window x y
    | _ ->
         ()

(*
 * Generic window.
 *)
let create_window channel dfbase dfmode =
   let _ =
      try
         Unix.mkdir "cache" 511
      with
         Unix.Unix_error _ ->
            ()
   in
      { win_channel = channel;
        win_dfmode = dfmode;
        win_dfbase = dfbase;
        win_width = 80;
        win_callback = (fun _ _ -> ());
        win_dir = "/";
        win_term = xnil_term
      }

(*
 * Standard menu window.
 *)
let create_menu port dfbase =
   create_window (Mux_channel.standard_menu port) dfbase "html"

let create_term port dfbase =
   create_window (Mux_channel.new_menu port) dfbase "html"

let create_proof port dfbase =
   let { goal_channel = goal_channel;
         rule_channel = rule_channel;
         subgoals_channel = subgoals_channel
       } = Mux_channel.new_proof port
   in
      { proof_goal = create_window goal_channel dfbase "html";
        proof_rule = create_window rule_channel dfbase "src";
        proof_subgoals = create_window subgoals_channel dfbase "html"
      }

(*
 * Set a callback for the window.
 *)
let set_callback window callback =
   Mux_channel.set_callback window.win_channel (input_callback window)

(*
 * Set the directory.
 *)
let set_dir window dir =
   window.win_dir <- dir

(*
 * Heading.
 *)
let head_string =
   "<head>
    <title>Nuprl term</title>
    <style>
    .mp {font-family:symbol}
    </style>
    </head>"

(*
 * Display a term in the window.
 *)
let set window term =
   let { win_channel = chan;
         win_dfbase = dfbase;
         win_dfmode = dfmode;
         win_width = width;
         win_dir = dir
       } = window
   in
   let df = get_mode_base dfbase dfmode in
   let buf = Rformat.new_buffer () in
   let _ =
      (* eprintf "Term: %a%t" SimplePrint.print_simple_term_fp term eflush; *)
      Dform.format_term df buf term
   in
   let host, port = host_of_channel chan in
      if dfmode = "html" then
         let out = open_out_bin (sprintf "cache/%s%d.html" host port) in
         let _ = fprintf out "<html>%s<body bgcolor=white face=\"Lucida Sans Unicode\"><table face=\"Lucida Sans Unicode\"><tr><td>" head_string in
         let _ = Rformat.print_to_html width buf out in
         let _ = fprintf out "</table></body></html>%t" eflush in
         let _ = close_out out in
         let s =
            match Mux_channel.url_of_channel chan with
               Some url ->
                  sprintf "\027]0;%s\007\027]1;%s\007Hello" url dir
             | None ->
                  sprintf "\027]1;%s\007Hello" dir
         in
            Mux_channel.output_string chan s;
            window.win_term <- term
      else
         let s = Rformat.print_to_string width buf in
            Mux_channel.output_string chan s

(*
 * -*-
 * Local Variables:
 * Caml-master: "nl"
 * End:
 * -*-
 *)
