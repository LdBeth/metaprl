(*
 * This module defines functions used to read and write terms in a robust ASCII-based format
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
 * Copyright (C) 1999 Alexey Nogin, Cornell University
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
 * Author: Alexey Nogin
 * nogin@cs.cornell.edu
 *
 *)

open Opname
open String_set

open Termmod_hash_sig

module MakeAsciiIO (TM: TermModuleHashSig) =
struct
   open TM
   open TM.TermType
   open TM.Term
   open TM.TermMan
   open TM.TermHash

   type term = TM.TermType.term
   type param = TM.TermType.param
   type bound_term = TM.TermType.bound_term
   type hypothesis = TM.TermType.hypothesis
   type esequent = TM.TermType.esequent

   type io_item = string * string * string list
   type io_table = io_item list ref

   let init_size = 97

   let initialize () =
      ref []

   let add_line t i =
      t:=i::(!t)

   type io_record =
    { io_terms : (string,term_index) Hashtbl.t;
      io_ops : (string,opname * hashed_param list) Hashtbl.t;
      io_opnames : (string,opname) Hashtbl.t;
      io_params : (string,hashed_param) Hashtbl.t;
      io_hyps : (string,hypothesis_header) Hashtbl.t;
      mutable io_seq : (string * term_index * hypothesis_header list) option;
      io_bterms : (string,bound_term_header) Hashtbl.t
    }

   let new_record () =
      let r =
      { io_terms = Hashtbl.create init_size;
        io_ops = Hashtbl.create init_size;
        io_opnames = Hashtbl.create init_size;
        io_params = Hashtbl.create init_size;
        io_hyps = Hashtbl.create init_size;
        io_bterms = Hashtbl.create init_size;
        io_seq = None
      } in
      Hashtbl.add r.io_opnames "nil" nil_opname;
      Hashtbl.add r.io_opnames "NIL" nil_opname;
      r

   let fail s =
      raise (Failure ("ASCII IO: invalid entry incountered by " ^ s ^ " function."))

   let hash_add_new tbl key data =
      if Hashtbl.mem tbl key then fail "hash_add_new" else Hashtbl.add tbl key data

   let add_term r = function
      (_, name, op::bterms) ->
         let (op,params) = Hashtbl.find r.io_ops op in
         let btrms = List.map (Hashtbl.find r.io_bterms) bterms in
         let t = lookup (Term { op_name = op; op_params = params; term_terms = btrms }) in
         hash_add_new r.io_terms name t;
         t
    | _ ->
         fail "add_term"

   let add_op r = function
      (_, name, op::params) ->
         let op = Hashtbl.find r.io_opnames op in
         let parms = List.map (Hashtbl.find r.io_params) params in
         hash_add_new r.io_ops name (op,parms)
    | _ ->
         fail "add_op"

   let add_name r = function
      (_, name, [nm;op]) ->
         let op = Hashtbl.find r.io_opnames op in
         hash_add_new r.io_opnames name (mk_opname nm op)
    | _ ->
         fail "add_name"

   let add_bterm r = function
      (_, name, term::vars) ->
         let term = Hashtbl.find r.io_terms term in
         hash_add_new r.io_bterms name { bvars = vars; bterm = term }
     | _ ->
         fail "add_bterm"

   let add_hyp r = function
      (_, name, [var;term]) ->
         let term = Hashtbl.find r.io_terms term in
         hash_add_new r.io_hyps name (Hypothesis(var,term))
    | _ ->
         fail "add_hyp"

   let add_context r = function
      (_, name, v::terms) ->
         let terms = List.map (Hashtbl.find r.io_terms) terms in
         hash_add_new r.io_hyps name (Context(v,terms))
    | _ ->
         fail "add_context"

   let add_seq r = function
      (_, name, args::hyps) ->
         let args = Hashtbl.find r.io_terms args in
         let hyps = List.map (Hashtbl.find r.io_hyps) hyps in
         r.io_seq <- Some (name, args, hyps)
    | _ ->
         fail "add_seq"

   let add_goal r = function
      (_, name, goals) ->
         begin match r.io_seq with
            Some (name',args,hyps) when name = name' ->
               if name <> name' then fail "add_goal" else
               let goals = List.map (Hashtbl.find r.io_terms) goals in
               let t = lookup ( Seq { seq_arg = args;
                                      seq_hyps = hyps;
                                      seq_goals = goals
                                    } ) in
               hash_add_new r.io_terms name t;
              t
          | _ ->
               fail "add_goal"
         end

   let rec level_exp_vars = function
      var::offset::vars ->
         make_level_var { le_var = var; le_offset = int_of_string offset } ::
            level_exp_vars vars
    | [] ->
         []
    | _ ->
         fail "level_exp_vars"

   let add_param r = function
      (_, name, ["Number"; n]) ->
         hash_add_new r.io_params name (constr_param (Number (Mp_num.num_of_string n)))
    | (_, name, ["String"; s]) ->
         hash_add_new r.io_params name (constr_param (String s))
    | (_, name, ["Token"; s]) ->
         hash_add_new r.io_params name (constr_param (Token s))
    | (_, name, ["Var"; s]) ->
         hash_add_new r.io_params name (constr_param (Var s))
    | (_, name, ["MNumber"; s]) ->
         hash_add_new r.io_params name (constr_param (MNumber s))
    | (_, name, ["MToken"; s]) ->
         hash_add_new r.io_params name (constr_param (MToken s))
    | (_, name, ["MVar"; s]) ->
         hash_add_new r.io_params name (constr_param (MVar s))
    | (_, name, "MLevel" :: n :: vars) ->
         let l = make_level { le_const = int_of_string n; le_vars = level_exp_vars vars } in
         hash_add_new r.io_params name (constr_param (MLevel l))
    | _ ->
         fail "add_param"

   let rec add_items r = function
      (long,_,_) as item :: items ->
         add_items r items;
         begin match long.[0] with
            'T'|'t' -> ignore (add_term r item)
          | 'G'|'g' -> ignore (add_goal r item)
          | 'B'|'b' -> add_bterm r item
          | 'H'|'h' -> add_hyp r item
          | 'C'|'c' -> add_context r item
          | 'S'|'s' -> add_seq r item
          | 'O'|'o' -> add_op r item
          | 'N'|'n' -> add_name r item
          | 'P'|'p' -> add_param r item
          | _ -> fail "add_items"
         end
    | [] -> ()

   let get_term t =
      try begin match !t with
         (long,_,_) as item :: items ->
            let r = new_record () in
            add_items r items;
            retrieve (
               match long.[0] with
                  'T'|'t' -> add_term r item
                | 'G'|'g' -> add_goal r item
                | _ -> fail "get_term"
            )
       | [] -> fail "get_term"
      end with Not_found -> fail "get_term"

   type out_control =
    { out_name_op : opname -> param list -> string * string;
      out_name_param : param -> string * string;
      out_name_term : term -> string * string;
      out_name_bterm : bound_term -> string * string;
      out_name_hyp : hypothesis -> string * string;
       (* arg+hyps long name, goals long name, short name *)
      out_name_seq : esequent -> string * string * string;
      out_line : io_item -> unit
    }

   type out_item =
      New of io_item
    | Old of string

   type out_data =
    { mutable io_names : StringSet.t; (* names of old items included in a new version *)
      mutable new_names : StringSet.t; (* names of items included in a new version *)
      mutable all_names : StringSet.t; (* names of all the available items (old and new) *)
      mutable out_items : out_item list;
      out_terms : string HashTerm.t;
      out_ops : (opname * hashed_param list, string) Hashtbl.t;
      out_opnames : (opname, string) Hashtbl.t;
      out_params : (hashed_param, string) Hashtbl.t;
      out_hyps : string HashHyp.t;
      out_bterms : string HashBTerm.t
    }

   let new_out_data () =
    { io_names = StringSet.empty;
      new_names = StringSet.empty;
      all_names = StringSet.empty;
      out_items = [];
      out_terms = HashTerm.create init_size;
      out_ops = Hashtbl.create init_size;
      out_opnames = Hashtbl.create init_size;
      out_params = Hashtbl.create init_size;
      out_hyps = HashHyp.create init_size;
      out_bterms = HashBTerm.create init_size;
    }

   let init_data r =
      let data = new_out_data () in
      Hashtbl.iter
       ( fun name ind ->
            data.all_names <- StringSet.add name data.all_names;
            HashTerm.add data.out_terms ind name
       ) r.io_terms;
      Hashtbl.iter
       ( fun name op ->
            data.all_names <- StringSet.add name data.all_names;
            Hashtbl.add data.out_ops op name
       ) r.io_ops;
      Hashtbl.iter
       ( fun name opname ->
            data.all_names <- StringSet.add name data.all_names;
            Hashtbl.add data.out_opnames opname name
       ) r.io_opnames;
      Hashtbl.iter
       ( fun name param ->
            data.all_names <- StringSet.add name data.all_names;
            Hashtbl.add data.out_params param name
       ) r.io_params;
      Hashtbl.iter
       ( fun name hyp ->
            data.all_names <- StringSet.add name data.all_names;
            HashHyp.add data.out_hyps hyp name
       ) r.io_hyps;
      Hashtbl.iter
       ( fun name bt ->
            data.all_names <- StringSet.add name data.all_names;
            HashBTerm.add data.out_bterms bt name
       ) r.io_bterms;
      data

   let rec do_rename name names i =
      let name' = name ^ (string_of_int i) in
      if StringSet.mem names name' then
         do_rename name names (succ i)
      else name'

   let rename name data =
      let names = data.all_names in
      let name' =
         if StringSet.mem names name then
            do_rename name names 1
         else name
      in
      data.all_names <- StringSet.add name' names;
      data.new_names <- StringSet.add name' data.new_names;
      name'

   let check_old data name =
      if not (StringSet.mem data.new_names name) then begin
         (* This item existed in the old version of the file *)
         data.new_names <- StringSet.add name data.new_names;
         data.io_names <- StringSet.add name data.io_names;
         data.out_items <- Old name :: data.out_items
      end

   let rec out_term ctrl data t =
      if is_sequent_term t then
         let seq = explode_sequent t in
         let (arg_name, arg_ind) = out_term ctrl data seq.sequent_args in
         let hyps = List.map (out_hyp ctrl data) (SeqHyp.to_list seq.sequent_hyps) in
         let goals = List.map (out_term ctrl data) (SeqGoal.to_list seq.sequent_goals) in
         let ind = lookup ( Seq { seq_arg = arg_ind;
                                  seq_hyps = List.map snd hyps;
                                  seq_goals = List.map snd goals } ) in
         try
            let name = HashTerm.find data.out_terms ind in
            if not (StringSet.mem data.new_names name) then begin
               (* This item existed in the old version of the file *)
               data.new_names <- StringSet.add name data.new_names;
               data.io_names <- StringSet.add name data.io_names;
               data.out_items <- Old name :: Old name :: data.out_items
               (* We need two "Old name" entries becase sequent is printed on two lines *)
            end;
            (name, ind)
         with Not_found ->
            let (hyps_lname, goals_lname, name) = ctrl.out_name_seq seq in
            let name = rename name data in
            HashTerm.add data.out_terms ind name;
            data.out_items <-
               New ("G" ^ goals_lname, name, List.map fst goals) ::
               New ("S" ^ hyps_lname, name, arg_name :: List.map fst hyps) ::
               data.out_items;
            (name, ind)
      else
         let t' = dest_term t in
         let (oper_name, (op, params)) = out_op ctrl data t'.term_op in
         let btrms = List.map (out_bterm ctrl data) t'.TermType.term_terms in
         let ind = lookup ( Term { op_name = op;
                                   op_params = params;
                                   term_terms = List.map snd btrms } ) in
         try
            let name = HashTerm.find data.out_terms ind in
            check_old data name;
            (name, ind)
         with Not_found ->
            let (lname, name) = ctrl.out_name_term t in
            let name = rename name data in
            HashTerm.add data.out_terms ind name;
            data.out_items <-
               New ("T" ^ lname, name, oper_name :: List.map fst btrms) :: data.out_items;
            (name, ind)

   and out_hyp ctrl data = function
      TermType.Hypothesis (v,t) as h -> begin
         let (t_name, t_ind) = out_term ctrl data t in
         let hyp = Hypothesis (v,t_ind) in
         try
            let name = HashHyp.find data.out_hyps hyp in
            check_old data name;
            (name, hyp)
         with Not_found ->
            let (lname, name) = ctrl.out_name_hyp h in
            let name = rename name data in
            HashHyp.add data.out_hyps hyp name;
            data.out_items <-
               New ("H" ^ lname, name, [v; t_name]) :: data.out_items;
            (name, hyp)
      end
    | TermType.Context (v,ts) as h -> begin
         let terms = List.map (out_term ctrl data) ts in
         let hyp = Context (v, List.map snd terms) in
         try
            let name = HashHyp.find data.out_hyps hyp in
            check_old data name;
            (name, hyp)
         with Not_found ->
            let (lname, name) = ctrl.out_name_hyp h in
            let name = rename name data in
            HashHyp.add data.out_hyps hyp name;
            data.out_items <-
               New ("C" ^ lname, name, v :: (List.map fst terms) ) :: data.out_items;
            (name, hyp)
      end

   and out_op ctrl data op =
      let op' = dest_op op in
      let (name_name, opname) = out_name ctrl data op'.TermType.op_name in
      let params = List.map (out_param ctrl data) op'.TermType.op_params in
      let op'' = (opname,List.map snd params) in
      try
         let name = Hashtbl.find data.out_ops op'' in
         check_old data name;
         (name, op'')
      with Not_found ->
         let (lname, name) = ctrl.out_name_op opname op'.TermType.op_params in
         let name = rename name data in
         Hashtbl.add data.out_ops op'' name;
         data.out_items <-
            New ("O" ^ lname, name, name_name :: List.map fst params) :: data.out_items;
         (name, op'')

   and out_bterm ctrl data bt =
      let bt' = dest_bterm bt in
      let (term_name, term_ind) = out_term ctrl data bt'.TermType.bterm in
      let bt'' = { bvars = bt'.TermType.bvars; bterm = term_ind } in
      try
         let name = HashBTerm.find data.out_bterms bt'' in
         check_old data name;
         (name, bt'')
      with Not_found ->
         let (lname, name) = ctrl.out_name_bterm bt in
         let name = rename name data in
         HashBTerm.add data.out_bterms bt'' name;
         data.out_items <-
            New ("B" ^ lname, name, term_name :: bt'.TermType.bvars) :: data.out_items;
         (name, bt'')

   and out_name ctrl data opname =
      if eq opname nil_opname then "NIL", nil_opname else
      let (name', op') = dst_opname opname in
      let (inner_name, inner_op) = out_name ctrl data op' in
      let op = mk_opname name' inner_op in
      try
         let name = Hashtbl.find data.out_opnames op in
         check_old data name;
         name, op
      with Not_found ->
         let name = rename name' data in
         Hashtbl.add data.out_opnames op name;
         data.out_items <-
            New ("N" ^ (string_of_opname opname), name, [name';inner_name]) :: data.out_items;
         name, op

   and map_level_vars = function
      [] -> []
    | v :: vs ->
         let v' = dest_level_var v in
         v'.le_var :: (string_of_int v'.le_offset) :: map_level_vars vs

   and out_param ctrl data param =
      let param' = constr_param (dest_param param) in
      try
         let name = Hashtbl.find data.out_params param' in
         check_old data name;
         name, param'
      with Not_found ->
         let new_rec =
            match dest_param param with
               Number n -> ["Number"; Mp_num.string_of_num n]
             | String s -> ["String"; s]
             | Token s -> ["Token"; s]
             | Var s -> ["Var"; s]
             | MNumber s -> ["MNumber"; s]
             | MToken s -> ["MToken"; s]
             | MVar s -> ["Mvar"; s]
             | MLevel le ->
                  let le' = dest_level le in
                  "MLevel" :: (string_of_int le'.le_const) :: (map_level_vars le'.le_vars)
             | _ -> fail "out_param"
         in
         let (lname, name) = ctrl.out_name_param param in
         let name = rename name data in
         Hashtbl.add data.out_params param' name;
         data.out_items <-
            New ("P"^lname, name, new_rec) :: data.out_items;
         name, param'

   let rec print_out out_line names o n =
      match n, o with
         [], _ ->
            ()
       | (New item :: rest), _ ->
            out_line item;
            print_out out_line names o rest
       | (Old name :: nrest), ((_, name', _) as item :: orest) when name = name' ->
            out_line item;
            print_out out_line names orest nrest
       | (Old _ :: _), ((_, name, _) as item :: orest) ->
            if StringSet.mem names name then out_line item;
            print_out out_line names orest n
       | (Old _ :: _), [] ->
            raise (Invalid_argument "Ascii_io.print_out")

   let output_term inputs ctrl t =
      let r = new_record () in
      add_items r !inputs;
      let data = init_data r in
      ignore (out_term ctrl data t);
      print_out ctrl.out_line data.io_names (List.rev !inputs) (List.rev data.out_items)

   let simple_name_op _ _ = "",""
   let simple_name_param _ = "",""
   let simple_name_term _ = "",""
   let simple_name_bterm _ = "",""
   let simple_name_hyp _ = "",""
   let simple_name_seq _ = "","",""

   let simple_output_line out (str1, str2, strs) =
      Printf.fprintf out "%s\t%s\t%s\n"
         (String_util.quote str1)
         (String_util.quote str2)
         (String.concat " " (List.map String_util.quote strs))

   let make_simple_control out =
    { out_name_op = simple_name_op;
      out_name_param = simple_name_param;
      out_name_term = simple_name_term;
      out_name_bterm = simple_name_bterm;
      out_name_hyp = simple_name_hyp;
      out_name_seq = simple_name_seq;
      out_line = simple_output_line out }

end

module AsciiIO=MakeAsciiIO (Refiner.Refiner)
