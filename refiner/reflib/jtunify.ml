(*
 * Unification procedures for JProver. See jall.mli for more
 * information on JProver.
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
 * Copyright (C) 2000 Stephan Schmitt
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
 * Author: Stephan Schmitt <schmitts@spmail.slu.edu>
 * Modified by: Aleksey Nogin <nogin@cs.cornell.edu>
 *)
open Lm_printf

exception Not_unifiable
exception Failed

let jprover_bug = Invalid_argument "Jprover bug (Jtunify module)"

(* ************ T-STRING UNIFICATION *********************************)


(* ******* printing ********** *)

let rec list_to_string s =
   match s with
      [] -> ""
    | f::r ->
         f^"."^(list_to_string r)

let rec print_eqlist eqlist =
   match eqlist with
      [] ->
         print_endline ""
    | (atnames,f)::r ->
         let (s,t) = f in
         let ls = list_to_string s
         and lt = list_to_string t in
         begin
            print_endline ("Atom names: "^(list_to_string atnames));
            print_endline (ls^" = "^lt);
            print_eqlist r
         end

let print_equations eqlist =
   begin
      open_box 0;
      force_newline ();
      print_endline "Equations:";
      print_eqlist eqlist;
      force_newline ();
   end

let rec print_subst sigma =
   match sigma with
      [] ->
         print_endline ""
    | f::r ->
         let (v,s) = f in
         let ls = list_to_string s in
         begin
            print_endline (v^" = "^ls);
            print_subst r
         end

let print_tunify sigma =
   let (n,subst) = sigma in
   begin
      print_endline " ";
      print_endline ("MaxVar = "^(string_of_int (n-1)));
      print_endline " ";
      print_endline "Substitution:";
      print_subst subst;
      print_endline " "
   end

 (*****************************************************)

let is_const name  =
  (String.get name 0) = 'c'

let is_var name  =
  (String.get name 0) = 'v'

let r_1 s ft rt =
   (s = []) && (ft = []) && (rt = [])

let r_2 s ft rt =
   (s = []) && (ft = []) && (List.length rt >= 1)

let r_3 s ft rt =
   ft=[] && (List.length s >= 1) && (List.length rt >= 1) && (List.hd s = List.hd rt)

let r_4 s ft rt =
   ft=[]
      && (List.length s >= 1)
      && (List.length rt >= 1)
      && is_const (List.hd s)
      && is_var (List.hd rt)

let r_5 s rt =
   rt=[]
      && (List.length s >= 1)
      && is_var (List.hd s)

let r_6 s ft rt =
   ft=[]
      && (List.length s >= 1)
      && (List.length rt >= 1)
      && is_var (List.hd s)
      && is_const (List.hd rt)

let r_7 s rt =
   List.length s >= 1
      && (List.length rt >= 2)
      && is_var (List.hd s)
      && is_const (List.hd rt)
      && is_const (List.hd (List.tl rt))

let r_8 s ft rt =
  ft=[]
    && List.length s >= 2
    && List.length rt >= 1
    && let v = List.hd s
       and v1 = List.hd rt in
         (is_var v) & (is_var v1) & (v <> v1)

let r_9 s ft rt =
   (List.length s >= 2) && (List.length ft >= 1) && (List.length rt >= 1)
      && let v = (List.hd s)
         and v1 = (List.hd rt) in
         (is_var v) & (is_var v1) & (v <> v1)

let r_10 s rt =
   (List.length s >= 1) && (List.length rt >= 1)
   && let v = List.hd s
      and x = List.hd rt in
      (is_var v) && (v <> x)
      && (((List.tl s) =[]) or (is_const x) or ((List.tl rt) <> []))

let rec com_subst ov ovlist = function
   [] -> []
 | f::r as l->
      if f = ov then
         (ovlist @ r)
      else
         let rest = com_subst ov ovlist r in
            if rest == r then l else f :: rest

let rec apply_element v slist fs ft =
   match (fs,ft) with
      [], [] ->
         [], []
    | [], (ft_first::ft_rest) ->
         let _, ft = apply_element v slist [] ft_rest in
         [], (if ft_first = v then slist @ ft else ft_first :: ft)
    | ((fs_first::fs_rest),[]) ->
         let fs, _ = apply_element v slist fs_rest [] in
         (if fs_first = v then slist @ fs else fs_first :: fs), []
    | ((fs_first::fs_rest),(ft_first::ft_rest)) ->
         let fs, ft = apply_element v slist fs_rest ft_rest in
         (if fs_first = v then slist @ fs else fs_first :: fs),
         (if ft_first = v then slist @ ft else ft_first :: ft)

(* type of one unifier: int * (string * string) list  *)
