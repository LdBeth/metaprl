(*
 * Imperative interface to the TeX output file.
 *
 * ----------------------------------------------------------------
 *
 * Copyright (C) 2000 Jason Hickey, Caltech
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
 * jyh@cs.caltech.edu
 *)

(*
 * open Dform_print
 * open Refiner
 * open Theory
 * open Mp_resource
 *)

(*
 * TeX prologue.
 *)
let tex_prologue = "% -*- Mode: fundamental -*-
\\documentclass{article}

%
% Ludica-Bright fonts
%
\\usepackage{lucidbry}
%\\usepackage[ansinew]{texnansi}
%\\usepackage[LY1]{fontenc}

%
% Hyperlinks
%
\usepackage{hyperref}

%
% Definitions.
%
\\input{metaprl}
\\makeindex

\\begin{document}

\\sloppy

"

let tex_epilogue = "
\\end{document}
"

(*
 * The output file is an out_channel option.
 *)
let output = ref None

(*
 * Open an output file.
 * Return the output if already defined.
 *)
let open_file () =
   match !output with
      Some out ->
         out
    | None ->
         let out = open_out "output.tex" in
            output_string out tex_prologue;
            out

(*
 * Close the output file.
 *)
let close_file out =
   match !output with
      Some out ->
         flush out
    | None ->
         output_string out tex_epilogue;
         close_out out

(*
 * Set the output file.
 *)
let set_file name =
   let _ =
      match !output with
         Some out ->
            close_out out;
            output := None
       | None ->
            ()
   in
   let name =
      if Filename.check_suffix name ".tex" then
         Filename.chop_suffix name ".tex"
      else
         name
   in
   let outer_name = name ^ ".tex" in
   let body_name = name ^ "-body.tex" in
   let out = open_out outer_name in
   let _ =
      Printf.fprintf out "%s\n\\input{%s}\n%s" tex_prologue (Filename.basename body_name) tex_epilogue;
      close_out out
   in
   let out = open_out body_name in
      output_string out "% -*- Mode: fundamental -*-\n";
      flush out;
      output := Some out;
      close_file out

(*
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
