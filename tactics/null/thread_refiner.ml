(*
 * This is the null thread implementation.
 *
 * ----------------------------------------------------------------
 *
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/htmlman/default.html or visit http://metaprl.org/
 * for more information.
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
 *)
open Refiner.Refiner.RefineError

module ThreadRefinerTacticals =
struct
   type ('arg, 'extract) extract =
      Leaf of 'extract
    | Node of ('arg, 'extract) extract * ('arg, 'extract) extract list
    | Wrap of 'arg * ('arg, 'extract) extract
   type ('term, 'arg, 'extract) t = 'term list * ('arg, 'extract) extract
   type ('term, 'arg, 'extract) tactic = 'term -> ('term, 'arg, 'extract) t

   (*
    * Constructors.
    *)
   let create_value args ext =
      args, Leaf ext

   (*
    * Forced error.
    *)
   let force debug tac arg =
      try tac arg with
         RefineError (name, err) ->
            raise (RefineForceError (debug, name, err))

   (*
    * Choice.
    *)
   let rec first tacs arg =
      match tacs with
         [tac] ->
            tac arg
       | tac :: tacs ->
            begin
               try tac arg with
                  RefineError _ ->
                     first tacs arg
            end
       | [] ->
            raise (RefineError ("first", StringError "no tactics"))

   (*
    * Composition forms.
    *)
   let rec apply1 tac = function
      arg :: args ->
         let args', ext = tac arg in
         let argsl, extl = apply1 tac args in
            args' @ argsl, ext :: extl
    | [] ->
         [], []

   let rec apply2 args1 args2 =
      match args1, args2 with
         arg1 :: args1, arg2 :: args2 ->
            let args, ext = arg1 arg2 in
            let argsl, extl = apply2 args1 args2 in
               args @ argsl, ext :: extl
       | [], [] ->
            [], []
       | _ ->
            raise (RefineError ("compose2", StringError "argument lists do not match"))

   let rec flatten = function
      (args, ext) :: tl ->
         let argsl, extl = flatten tl in
            args @ argsl, ext :: extl
    | [] ->
         [], []

   let wrap wrap_arg tac arg =
      let args, ext = tac arg in
         args, Wrap (wrap_arg, ext)

   let wrap_terms f tac arg =
      let args, ext = tac arg in
         List.map f args, ext

   let check_terms f tac arg =
      let args, ext = tac arg in
         f args;
         args, ext

   let compose1 tac1 tac2 arg =
      let args, ext = tac1 arg in
      let argsl, extl = apply1 tac2 args in
         argsl, Node (ext, extl)

   let compose2 tac1 tacs2 arg =
      let args, ext = tac1 arg in
      let argsl, extl = apply2 tacs2 args in
         argsl, Node (ext, extl)

   let composef tac1 tacf arg =
      let args, ext = tac1 arg in
      let argsl, extl = flatten (tacf args) in
         argsl, Node (ext, extl)
end

module ThreadRefiner =
struct
   (************************************************************************
    * TYPES                                                                *
    ************************************************************************)

   type ('term, 'arg, 'extract) t = ('term, 'arg, 'extract) ThreadRefinerTacticals.t
   type ('term, 'arg, 'extract) tactic = ('term, 'arg, 'extract) ThreadRefinerTacticals.tactic
   type 'share key = 'share
   type ('term, 'share, 'arg, 'extract) server =
      { compose : 'extract -> 'extract list -> 'extract;
        wrap : 'arg -> 'extract -> 'extract
      }

   (************************************************************************
    * IMPLEMENTATION                                                       *
    ************************************************************************)

   (*
    * Server is empty.
    *)
   let create _ compose wrap =
      { compose = compose;
        wrap = wrap
      }

   (*
    * Tacticals.
    *)
   let create_value = ThreadRefinerTacticals.create_value

   let wrap = ThreadRefinerTacticals.wrap
   let first = ThreadRefinerTacticals.first
   let force = ThreadRefinerTacticals.force
   let compose1 = ThreadRefinerTacticals.compose1
   let compose2 = ThreadRefinerTacticals.compose2
   let composef = ThreadRefinerTacticals.composef

   (*
    * Evaluation just composes the extract
    *)
   let eval { compose = compose; wrap = wrap } (arg, ext) =
      let rec compose' = function
         ThreadRefinerTacticals.Leaf ext ->
            ext
       | ThreadRefinerTacticals.Node (ext, extl) ->
            compose (compose' ext) (List.map compose' extl)
       | ThreadRefinerTacticals.Wrap (arg, ext) ->
            wrap arg (compose' ext)
      in
         arg, compose' ext

   (*
    * Shared memory.
    *)
   let share _ _ f =
      f ()

   let arg_of_key _ x =
      x

   (*
    * Nothing in main loop.
    *)
   let args _ =
      []

   let main_loop _ =
      ()
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
