(*
 * This module defines functions used to read and write terms in a robust ASCII-based format
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
open Lm_debug
open Lm_symbol
open Lm_string_set

open Opname
open Term_sig
open Termmod_hash_sig

let debug_ascii_io =
   create_debug (**)
      { debug_name = "ascii_io";
        debug_description = "report ASCII IO errors verbosely";
        debug_value = false
      }

let eprintf = Lm_printf.eprintf
let eflush  = Lm_printf.eflush

(*
 * When this file changes, we might need to update filter_magic.ml.
 * The following line is a marker for the automated magic number computation,
 * _do not delete_.
 * %%MAGICBEGIN%%
 *)
module MakeAsciiIO (TM: TermModuleHashSig) =
struct
   open TM.TermMan
   open TM.TermHash
   open TM.TermShape

   type term = TM.TermType.term
   type param = TM.TermType.param
   type bound_term = TM.TermType.bound_term
   type hypothesis = TM.TermType.hypothesis
   type esequent = TM.TermType.esequent

   type io_item = string * string * string list list
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
      mutable io_seq : (string * term_index * hypothesis_header list) option; (*XXX HACK: field needed for compatibility with file format versions 1.0.6 and below *)
      io_bterms : (string,bound_term_header) Hashtbl.t;
      mutable io_old_format : bool; (* XXX HACK: field needed for compatibility with file format versions 1.0.7 and below *)
    }

   let new_record () =
      let r =
      { io_terms = Hashtbl.create init_size;
        io_ops = Hashtbl.create init_size;
        io_opnames = Hashtbl.create init_size;
        io_params = Hashtbl.create init_size;
        io_hyps = Hashtbl.create init_size;
        io_bterms = Hashtbl.create init_size;
        io_seq = None;
        io_old_format = false;
      } in
      Hashtbl.add r.io_opnames "nil" nil_opname;
      Hashtbl.add r.io_opnames "NIL" nil_opname;
      r

   let fail s =
      raise (Invalid_argument ("ASCII IO: invalid entry encountered by " ^ s ))

   let fail2 s key =
      raise (Invalid_argument ("ASCII IO: invalid entry encountered by " ^ s ^ " key=" ^ key))

   let hash_add_new tbl key data =
      if Hashtbl.mem tbl key then
         fail2 "hash_add_new" key
      else
         Hashtbl.add tbl key data

   let add_term r = function
      (_, name, [op::bterms]) ->
         let (op,params) = Hashtbl.find r.io_ops op in
         let btrms = List.map (Hashtbl.find r.io_bterms) bterms in
         let t =
            if Opname.eq op var_opname && btrms <> [] then (* XXX HACK: Version <= 1.0.7 compatiility *)
               let t = retrieve (lookup (Term { op_name = op; op_params = params; term_terms = [] })) in
               match TM.Term.dest_params (TM.Term.dest_op (TM.Term.dest_term t).term_op).Term_sig.op_params with
                  [Var v] ->
                     let dest bt =
                        if bt.TM.TermHash.bvars = [] then bt.TM.TermHash.bterm else
                        raise(Invalid_argument "Ascii_io: invalid old-style variable term")
                     in
                        lookup (SOVar(v, [v], List.map dest btrms))
                | _ -> raise(Invalid_argument "Ascii_io: invalid old-style variable term")
            else
               lookup (Term { op_name = op; op_params = params; term_terms = btrms })
         in
         hash_add_new r.io_terms name t;
         t
    | _ ->
         fail "add_term"

   let add_op r = function
      (_, name, [op::params]) ->
         let op = Hashtbl.find r.io_opnames op in
         let parms = List.map (Hashtbl.find r.io_params) params in
         hash_add_new r.io_ops name (op,parms)
    | _ ->
         fail "add_op"

   let add_name r = function
      (_, name, [[nm;op]]) ->
         let op = Hashtbl.find r.io_opnames op in
         hash_add_new r.io_opnames name (mk_opname nm op)
    | _ ->
         fail "add_name"

   let add_bterm r = function
      (_, name, [term::vars]) ->
         let term = Hashtbl.find r.io_terms term in
         let vars = List.map Lm_symbol.add vars in
         hash_add_new r.io_bterms name { bvars = vars; bterm = term }
     | _ ->
         fail "add_bterm"

   let v1010 = Lm_symbol.make "" 1010 (* XXX HACK: format versions <= 1.0.10 compatibility *)

   let add_hyp r = function
      (_, name, [[var;term]]) ->
         let term = Hashtbl.find r.io_terms term in
         hash_add_new r.io_hyps name (Hypothesis(Lm_symbol.add var,term))
    | (_, name, [[term]]) -> (* XXX HACK: format versions <= 1.0.10 compatibility *)
         let term = Hashtbl.find r.io_terms term in
         hash_add_new r.io_hyps name (Hypothesis(v1010, term))
    | _ ->
         fail "add_hyp"

   let add_context r = function
      (_, name, [[v];conts;terms]) ->
         let terms = List.map (Hashtbl.find r.io_terms) terms in
         hash_add_new r.io_hyps name (Context(Lm_symbol.add v,List.map Lm_symbol.add conts,terms))
    | (_, name, [v::terms]) -> (* XXX HACK: format versions <= 1.0.7 compatibility *)
         let terms = List.map (Hashtbl.find r.io_terms) terms in
         let v = Lm_symbol.add v in
         r.io_old_format <- true;
         hash_add_new r.io_hyps name (Context(v,[v],terms))
    | _ ->
         fail "add_context"

   let find_concl = function (* XXX HACK: file format version <= 1.0.14 comatibility *)
      [concl] -> concl (* This is the only possibility in formats >= 1.0.15 *)
    | concl :: _ -> concl
    | [] -> "t4" (* XXX HACK squared - t4 is what xnil_term happens to be in mmc_core_cps.prla *)

   let add_seq r = function
      (_, name, [[args];hyps;concls]) ->
         let concl = find_concl concls in (* XXX HACK: file format version <= 1.0.14 comatibility; in >= 1.0.15 concls is a singleton list *)
         let args = Hashtbl.find r.io_terms args in
         let hyps = List.map (Hashtbl.find r.io_hyps) hyps in
         let concl = Hashtbl.find r.io_terms concl in
         let t = lookup ( Seq { seq_arg = args;
                                seq_hyps = hyps;
                                seq_concl = concl
                              } ) in
         hash_add_new r.io_terms name t;
         t
    | (_, name, [args::hyps]) -> (* XXX HACK: file format versions 1.0.6 and below compatibility mode *)
         let args = Hashtbl.find r.io_terms args in
         let hyps = List.map (Hashtbl.find r.io_hyps) hyps in
         r.io_seq <- Some (name, args, hyps);
         args
    | _ ->
         fail "add_seq"

   let add_goal r (_, name, goals) = (* XXX HACK: function needed only for file format versions 1.0.6 and below compatibility *)
         match goals, r.io_seq with
            [concls], Some (name',args,hyps) when name = name' ->
               let concl = find_concl concls in
               let concl = Hashtbl.find r.io_terms concl in
               let t = lookup ( Seq { seq_arg = args;
                                      seq_hyps = hyps;
                                      seq_concl = concl
                                    } ) in
               hash_add_new r.io_terms name t;
               t
          | _ ->
               fail "add_goal"

   let add_var r = function
      (_, name, [[var];conts;terms]) ->
         let t = lookup (SOVar (Lm_symbol.add var, List.map Lm_symbol.add conts, List.map (Hashtbl.find r.io_terms) terms)) in
            hash_add_new r.io_terms name t;
            t
    | _ -> fail "add_var"

   let rec level_exp_vars = function
      var::offset::vars ->
         TM.Term.make_level_var { le_var = Lm_symbol.add var; le_offset = int_of_string offset } ::
            level_exp_vars vars
    | [] ->
         []
    | _ ->
         fail "level_exp_vars"

   let add_param r = function
      (_, name, [["Number"; n]]) ->
         hash_add_new r.io_params name (constr_param (Number (Lm_num.num_of_string n)))
    | (_, name, [["String"; s]]) ->
         hash_add_new r.io_params name (constr_param (String s))
    | (_, name, [["Token"; s]]) ->
         hash_add_new r.io_params name (constr_param (Token (Hashtbl.find r.io_opnames s)))
    | (_, name, [["Shape"; s]]) ->
         hash_add_new r.io_params name (constr_param (Shape (shape_of_term (retrieve (Hashtbl.find r.io_terms s)))))
    | (_, name, [["Operator"; s]]) ->
         hash_add_new r.io_params name (constr_param (Operator (opparam_of_term (retrieve (Hashtbl.find r.io_terms s)))))
    | (_, name, [["Var"; s]]) ->
         hash_add_new r.io_params name (constr_param (Var (Lm_symbol.add s)))
    | (_, name, [["Quote"]]) ->
         hash_add_new r.io_params name (constr_param Quote)
    | (_, name, [["MNumber"; s]]) ->
         hash_add_new r.io_params name (constr_param (MNumber (Lm_symbol.add s)))
    | (_, name, [["MString"; s]]) ->
         hash_add_new r.io_params name (constr_param (MString (Lm_symbol.add s)))
    | (_, name, [["MToken"; s]]) ->
         hash_add_new r.io_params name (constr_param (MToken (Lm_symbol.add s)))
    | (_, name, [["MShape"; s]]) ->
         hash_add_new r.io_params name (constr_param (MShape (Lm_symbol.add s)))
    | (_, name, [["MOperator"; s]]) ->
         hash_add_new r.io_params name (constr_param (MOperator (Lm_symbol.add s)))
    | (_, name, [["MVar"; s]]) (* XXX HACK: support file versions 1.0.5 and below *) ->
         hash_add_new r.io_params name (constr_param (Var (Lm_symbol.add s)))
    | (_, name, ["MLevel" :: n :: vars]) ->
         let l = TM.Term.make_level { le_const = int_of_string n; le_vars = level_exp_vars vars } in
         hash_add_new r.io_params name (constr_param (MLevel l))
    | _ ->
         fail "add_param"

   let add_items_aux r ((long, _, _) as item) =
      match long.[0] with
         'T'|'t' -> ignore (add_term r item)
       | 'G'|'g' -> ignore (add_goal r item)  (* XXX: HACK: Version <= 1.0.6 compatibility *)
       | 'S'|'s' -> ignore (add_seq r item)
       | 'V'|'v' -> ignore (add_var r item)
       | 'B'|'b' -> add_bterm r item
       | 'H'|'h' -> add_hyp r item
       | 'C'|'c' -> add_context r item
       | 'O'|'o' -> add_op r item
       | 'N'|'n' -> add_name r item
       | 'P'|'p' -> add_param r item
       | _ -> fail ("add_items: " ^ long)

   let add_items_debug r ((long, short, rest) as item) =
      eprintf "%s %s %s%t" long short (String.concat "\\\\ " (List.map (String.concat " ") rest)) eflush;
      try
         match long.[0] with
            'T'|'t' -> ignore (add_term r item)
          | 'G'|'g' -> ignore (add_goal r item)
          | 'S'|'s' -> ignore (add_seq r item)
          | 'V'|'v' -> ignore (add_var r item)
          | 'B'|'b' -> add_bterm r item
          | 'H'|'h' -> add_hyp r item
          | 'C'|'c' -> add_context r item
          | 'O'|'o' -> add_op r item
          | 'N'|'n' -> add_name r item
          | 'P'|'p' -> add_param r item
          | _ -> fail ("add_items: " ^ long)
      with
         Not_found -> fail ("add_items_debug: " ^ long ^ " " ^ short ^ " ...")

   let add_items r items =
      let items = List.rev items in
         if !debug_ascii_io then
            List.iter (add_items_debug r) items
         else
            try List.iter (add_items_aux r) items with
               Not_found ->
                  fail "add_items"

   let get_term t =
      try begin match !t with
         (long,short,_) as item :: items ->
            let r = new_record () in
            add_items r items;
            let res =
            retrieve (
               match long.[0] with
                  'T'|'t' -> add_term r item
                | 'G'|'g' -> add_goal r item
                | 'S'|'s' -> add_seq r item
                | 'V'|'v' -> add_var r item
                | _ -> fail ("get_term: " ^ long)
            ) in
            if r.io_old_format then begin
               eprintf "Warning: converting a term read from an old .prla file%t" eflush;
               TM.TermMeta.term_of_parsed_term (fun _ -> true) res (* XXX HACK: format version <= 1.0.7 support *)
            end else res
       | [] -> fail "get_term1"
      end with Not_found -> fail "get_term - empty file"

   let get_named_term t name =
      let r = new_record () in
      add_items r !t;
      let res = retrieve (Hashtbl.find r.io_terms name) in
      if r.io_old_format then begin
         eprintf "Warning: converting a term read from an old .prla file%t" eflush;
         TM.TermMeta.term_of_parsed_term (fun _ -> true) res (* XXX HACK: format version <= 1.0.7 support *)
      end else res

   let read_table inx =
      let table = initialize () in
      let rec collect lineno =
         let line = input_line inx in
            if String.length line = 0 || line.[0] = '#' then
               collect (succ lineno)
            else
               match Lm_string_util.parse_args_list line with
                  (arg1 :: arg2 :: args)::more_args ->
                     add_line table (arg1, arg2, (args::more_args));
                     collect (succ lineno)
                | _ ->
                     eprintf "Ascii_io: syntax error on line %d%t" lineno eflush
      in
      let _ =
         try collect 1 with
            End_of_file ->
               ()
      in
         table

   let read_from_file file =
      get_named_term (read_table (open_in file))

   (************************************************************************
    * OUTPUT                                                               *
    ************************************************************************)

   type out_control =
    { out_name_op : opname -> param list -> string * string;
      out_name_param : param -> string * string;
      out_name_term : term -> string * string;
      out_name_bterm : bound_term -> string * string;
      out_name_hyp : hypothesis -> string * string;
       (* arg+hyps long name, goals long name, short name *)
      out_name_seq : esequent -> string * string;
      out_line : io_item -> unit
    }

   type out_item =
      New of io_item
    | Old of string

   type out_data =
    { mutable io_names : StringSet.t; (* names of old items included in a new version *)
      mutable new_names : StringSet.t; (* names of items included in a new version *)
      mutable all_names : StringSet.t; (* names of all the available items (old and new) *)
      mutable name_index : int;        (* index for the next name to be allocated *)
      old_items : (string, io_item) Hashtbl.t; (* a hash table with all the old items *)
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
      name_index = 1;
      old_items = Hashtbl.create init_size;
      out_items = [];
      out_terms = HashTerm.create init_size;
      out_ops = Hashtbl.create init_size;
      out_opnames = Hashtbl.create init_size;
      out_params = Hashtbl.create init_size;
      out_hyps = HashHyp.create init_size;
      out_bterms = HashBTerm.create init_size;
    }

   let init_data (inputs : io_item list ref) =
      (*
       * First, we want to make sure that every value occurs at most once in the list.
       * Duplicate names for the same value (which could have only been introduced
       * by manual editing) need to be detected and standardized.
       *)
      let h_reverse = Hashtbl.create 19 in
      let h_rename = Hashtbl.create 19 in
      let rename name =
         match Hashtbl.find_all h_rename name with
            [] -> name
          | name' :: _ ->
               name'
      in let smap_rename = Lm_list_util.smap (Lm_list_util.smap rename) in
      let clean_inputs tail ((comment,name,record) as item) =
         let c = Char.uppercase comment.[0] in
         let record' = match c, record with
            ('T' | 'O' | 'G' | 'S'), _ ->
               smap_rename record
          | 'B', [term :: vars] ->
               let term' = rename term in if term == term' then record else [term'::vars]
          | 'H', [[var;name]] ->
               let name' = rename name in if name == name' then record else [[var;name']]
          | 'H', [[_]] -> (* XXX HACK: format versions <= 1.0.10 compatibility *)
               smap_rename record
          | 'N', [[_;"NIL"]] ->
               record
          | 'N', [var::rest] ->
               let rest' = Lm_list_util.smap rename rest in if rest == rest' then record else [var::rest']
          | ('C' | 'V'), [var; conts; terms] ->
               let terms' = Lm_list_util.smap rename terms in if terms' == terms then record else [var; conts; terms']
          | ('C' | 'V'), [var::rest] -> (* XXX HACK: format versions <= 1.0.7 compatibility *)
               let rest' = Lm_list_util.smap rename rest in if rest == rest' then record else [var::rest']
          | 'P', [[("Token"|"Shape"|"Operator" as knd); s]] ->
               let s' = rename s in if s == s' then record else [[knd; s]]
          | 'P', _ ->
               record
          | _ ->
               raise (Invalid_argument ("ASCII IO: internal error: clean_inputs does not know what to do with "^comment))
         in
         match Hashtbl.find_all h_reverse (c, record'), c with
            [], _ | _, ('S'| 'G' ) -> (* XXX HACK: S/G exception only needed for format version <= 1.0.6??? *)
               begin match c with
                  'S'| 'G' -> ()
                | _ -> Hashtbl.add h_reverse (c, record') name
               end;
               let item' =
                  if record==record' then item else (comment,name,record')
               in
                  item'::tail
          | name'::_, _ ->
               Hashtbl.add h_rename name name';
               tail
      in
      inputs := List.fold_left clean_inputs [] (List.rev !inputs);
      let r = new_record () in
      add_items r !inputs;
      let data = new_out_data () in
      List.iter (fun ((_,name,_) as item) -> Hashtbl.add data.old_items name item) !inputs;
      Hashtbl.iter
       ( fun name ind ->
            data.all_names <- StringSet.add data.all_names name;
            HashTerm.add data.out_terms ind name;
       ) r.io_terms;
      Hashtbl.iter
       ( fun name op ->
            data.all_names <- StringSet.add data.all_names name;
            Hashtbl.add data.out_ops op name
       ) r.io_ops;
      Hashtbl.iter
       ( fun name opname ->
            data.all_names <- StringSet.add data.all_names name;
            Hashtbl.add data.out_opnames opname name
       ) r.io_opnames;
      Hashtbl.iter
       ( fun name param ->
            data.all_names <- StringSet.add data.all_names name;
            Hashtbl.add data.out_params param name
       ) r.io_params;
      Hashtbl.iter
       ( fun name hyp ->
            data.all_names <- StringSet.add data.all_names name;
            HashHyp.add data.out_hyps hyp name
       ) r.io_hyps;
      Hashtbl.iter
       ( fun name bt ->
            data.all_names <- StringSet.add data.all_names name;
            HashBTerm.add data.out_bterms bt name
       ) r.io_bterms;
      data

   let rec do_rename name names i =
      let name' = name ^ (string_of_int i) in
         if StringSet.mem names name' then
            do_rename name names (succ i)
         else
            name', i

   let rename name data =
      let names = data.all_names in
      let name' =
         if StringSet.mem names name then
            let name, index = do_rename name names data.name_index in
               data.name_index <- index;
               name
         else
            name
      in
         data.all_names <- StringSet.add names name';
         data.new_names <- StringSet.add data.new_names name';
         name'

   let check_old data name i_data =
      if not (StringSet.mem data.new_names name) then begin
         (* This item existed in the old version of the file *)
         data.new_names <- StringSet.add data.new_names name;
         let (lname,_,io_data) = Hashtbl.find data.old_items name in
         if i_data<> io_data then begin
            if !debug_ascii_io then
               eprintf "ASCII IO: Duplicate entry updated: %s%t" name eflush;
            data.out_items <- New (lname, name, i_data) :: data.out_items
         end else begin
            data.io_names <- StringSet.add data.io_names name;
            data.out_items <- Old name :: data.out_items
         end
      end

   let rec out_term ctrl data t =
      if is_so_var_term t then
         let v, conts, ts = dest_so_var t in
         let ts_names, ts_inds = List.split (List.map (out_term ctrl data) ts) in
         let ind = lookup (SOVar(v, conts, ts_inds)) in
         let v = string_of_symbol v in let conts = List.map string_of_symbol conts in
         try
            let name = HashTerm.find data.out_terms ind in
            if not (StringSet.mem data.new_names name) then begin (* XXX HACK: check_old replacement for versions <= 1.0.7 *)
               (* This item existed in the old version of the file *)
               data.new_names <- StringSet.add data.new_names name;
               match Hashtbl.find_all data.old_items name with
                  (_, _, [[v2]; conts2; ts_names2]) :: _ when v = v2 && conts = conts2 && ts_names = ts_names2 ->
                     data.io_names <- StringSet.add data.io_names name;
                     data.out_items <- Old name :: data.out_items
                | (l,_,([_;_;_] as io_data)) :: _ when (l.[0]='V') || (l.[0]='v') ->
                     data.out_items <- New (l, name, io_data) :: data.out_items
                | (l, name, _) :: _ ->  (* XXX HACK: version <= 1.0.7 *)
                     let l = "V" ^ String.sub l 1 (String.length l - 1) in
                     data.out_items <- New (l, name, [[v]; conts; ts_names]) :: data.out_items
                | _ ->
                     fail ("out_term - SO variable entry " ^ name ^ " was invalid")
            end;
            (name, ind)
         with Not_found ->
            let name = rename v data in
            HashTerm.add data.out_terms ind name;
            data.out_items <- New ("V" ^ v, name, [[v]; conts; ts_names]) :: data.out_items;
            (name, ind)
      else if is_sequent_term t then
         let seq = explode_sequent t in
         let arg_name, arg_ind = out_term ctrl data seq.TM.TermType.sequent_args in
         let hyps = List.map (out_hyp ctrl data) (TM.Term.SeqHyp.to_list seq.TM.TermType.sequent_hyps) in
         let concl_name, concl_ind = out_term ctrl data seq.TM.TermType.sequent_concl in
         let ind = lookup ( Seq { seq_arg = arg_ind;
                                  seq_hyps = List.map snd hyps;
                                  seq_concl = concl_ind } ) in
         let i_data3 = [concl_name] in
         let i_data2 = List.map fst hyps in
         try
            let name = HashTerm.find data.out_terms ind in
            if not (StringSet.mem data.new_names name) then begin (* XXX HACK: check_old replacement for versions <= 1.0.7 *)
               (* This item existed in the old version of the file *)
               data.new_names <- StringSet.add data.new_names name;
               match Hashtbl.find_all data.old_items name with
                  (l2,_,io_data2)::(l1,_,io_data1)::_     (* XXX HACK: version <= 1.0.6 *)
                     when (l1.[0]='G' || l1.[0]='g') && (l2.[0]='S' || l2.[0]='s') ->
                        if !debug_ascii_io then
                           eprintf "ASCII IO: old-style sequent entry converted: %s%t" name eflush;
                        data.out_items <- New (l2, name, [[arg_name];i_data2; i_data3]) :: data.out_items
                | (_,_,[[io_arg]; io_data2; io_data3]) :: _ when io_data2 = i_data2 && io_data3 = i_data3 ->
                     data.io_names <- StringSet.add data.io_names name;
                     data.out_items <- Old name :: data.out_items
                | (l,_,([_;_;_] as io_data)) :: _ when (l.[0]='S') || (l.[0]='s') ->
                     data.out_items <- New (l, name, io_data) :: data.out_items
                | _ ->
                     fail ("out_term - sequent entry " ^ name ^ " was invalid")
            end;
            (name, ind)
         with Not_found ->
            let lname, name = ctrl.out_name_seq seq in
            let name = rename name data in
            HashTerm.add data.out_terms ind name;
            data.out_items <- New ("S" ^ lname, name, [[arg_name]; i_data2; i_data3]) :: data.out_items;
            (name, ind)
      else
         let t' = TM.Term.dest_term t in
         let (oper_name, (op, params)) = out_op ctrl data t'.term_op in
         let btrms = List.map (out_bterm ctrl data) t'.Term_sig.term_terms in
         let ind = lookup ( Term { op_name = op;
                                   op_params = params;
                                   term_terms = List.map snd btrms } ) in
         let i_data = [oper_name :: List.map fst btrms] in
         try
            let name = HashTerm.find data.out_terms ind in
            check_old data name i_data;
            (name, ind)
         with Not_found ->
            let (lname, name) = ctrl.out_name_term t in
            let name = rename name data in
            HashTerm.add data.out_terms ind name;
            data.out_items <-
               New ("T" ^ lname, name, i_data) :: data.out_items;
            (name, ind)

   and out_hyp ctrl data = function
      Term_sig.Hypothesis (v,t) as h -> begin
         let (t_name, t_ind) = out_term ctrl data t in
         let hyp = TM.TermHash.Hypothesis (v,t_ind) in
         let i_data = [[string_of_symbol v; t_name]] in
         try
            let name = HashHyp.find data.out_hyps hyp in
            check_old data name i_data;
            (name, hyp)
         with Not_found -> begin
            let (lname, name) = ctrl.out_name_hyp h in
            let name = rename name data in
            HashHyp.add data.out_hyps hyp name;
            data.out_items <-
               New ("H" ^ lname, name, i_data) :: data.out_items;
            (name, hyp)
         end
      end
    | Term_sig.Context (v,conts,ts) as h -> begin
         let terms = List.map (out_term ctrl data) ts in
         let hyp = TM.TermHash.Context (v, conts, List.map snd terms) in
         let i_data = [[string_of_symbol v]; List.map string_of_symbol conts; List.map fst terms] in
         try
            let name = HashHyp.find data.out_hyps hyp in
            check_old data name i_data;
            (name, hyp)
         with Not_found ->
            let (lname, name) = ctrl.out_name_hyp h in
            let name = rename name data in
            HashHyp.add data.out_hyps hyp name;
            data.out_items <-
               New ("C" ^ lname, name, i_data ) :: data.out_items;
            (name, hyp)
      end

   and out_op ctrl data op =
      let op' = TM.Term.dest_op op in
      let (name_name, opname) = out_name ctrl data op'.Term_sig.op_name in
      let params = List.map (out_param ctrl data) op'.Term_sig.op_params in
      let op'' = (opname,List.map snd params) in
      let i_data = [name_name :: List.map fst params] in
      try
         let name = Hashtbl.find data.out_ops op'' in
         check_old data name i_data;
         (name, op'')
      with Not_found ->
         let (lname, name) = ctrl.out_name_op opname op'.Term_sig.op_params in
         let name = rename name data in
         Hashtbl.add data.out_ops op'' name;
         data.out_items <-
            New ("O" ^ lname, name, i_data) :: data.out_items;
         (name, op'')

   and out_bterm ctrl data bt =
      let bt' = TM.Term.dest_bterm bt in
      let (term_name, term_ind) = out_term ctrl data bt'.Term_sig.bterm in
      let bt'' = { TM.TermHash.bvars = bt'.Term_sig.bvars; TM.TermHash.bterm = term_ind } in
      let i_data = [term_name :: List.map string_of_symbol bt'.Term_sig.bvars] in
      try
         let name = HashBTerm.find data.out_bterms bt'' in
         check_old data name i_data;
         (name, bt'')
      with Not_found ->
         let (lname, name) = ctrl.out_name_bterm bt in
         let name = rename name data in
         HashBTerm.add data.out_bterms bt'' name;
         data.out_items <-
            New ("B" ^ lname, name, i_data) :: data.out_items;
         (name, bt'')

   and out_name ctrl data opname =
      if eq opname nil_opname then "NIL", nil_opname else
      let (name', op') = dst_opname opname in
      let (inner_name, inner_op) = out_name ctrl data op' in
      let op = mk_opname name' inner_op in
      let i_data = [[name';inner_name]] in
      try
         let name = Hashtbl.find data.out_opnames op in
         check_old data name i_data;
         name, op
      with Not_found ->
         let name = rename name' data in
         Hashtbl.add data.out_opnames op name;
         data.out_items <-
            New ("N" ^ (string_of_opname opname), name, i_data) :: data.out_items;
         name, op

   and map_level_vars = function
      [] -> []
    | v :: vs ->
         let v' = TM.Term.dest_level_var v in
         string_of_symbol v'.le_var :: (string_of_int v'.le_offset) :: map_level_vars vs

   and out_param ctrl data param =
      let param' = constr_param (TM.Term.dest_param param) in
      let i_data =
         [match TM.Term.dest_param param with
            Number n    -> ["Number"; Lm_num.string_of_num n]
          | String s    -> ["String"; s]
          | Token op    -> ["Token"; fst (out_name ctrl data op)]
          | Var s       -> ["Var"; string_of_symbol s]
          | Shape sh    -> ["Shape"; fst (out_term ctrl data (canonical_term_of_shape sh))]
          | Operator op -> ["Operator"; fst (out_term ctrl data (canonical_term_of_opparam op))]
          | Quote       -> ["Quote"]
          | MNumber s   -> ["MNumber"; string_of_symbol s]
          | MString s   -> ["MString"; string_of_symbol s]
          | MToken s    -> ["MToken"; string_of_symbol s]
          | MShape s    -> ["MShape"; string_of_symbol s]
          | MOperator s -> ["MOperator"; string_of_symbol s]
          | MLevel le ->
               let le' = TM.Term.dest_level le in
               "MLevel" :: (string_of_int le'.le_const) :: (map_level_vars le'.le_vars)
          | ParamList _ | ObId _ -> fail "out_param"]
      in try
         let name = Hashtbl.find data.out_params param' in
         check_old data name i_data;
         name, param'
      with Not_found ->
         let (lname, name) = ctrl.out_name_param param in
         let name = rename name data in
         Hashtbl.add data.out_params param' name;
         data.out_items <-
            New ("P"^lname, name, i_data) :: data.out_items;
         name, param'

   let rec print_out out_line printed data o n =
      match n, o with
         [], _ ->
            ()
       | (New ((_, name, _) as item) :: rest), _ ->
            out_line item;
            print_out out_line (StringSet.add printed name) data o rest
       | (Old name :: nrest), _ when StringSet.mem printed name ->
            print_out out_line printed data o nrest
       | (Old _ :: _), ((_, name, _) as item :: ((_, name', _) as item') :: orest)
            when name=name' && StringSet.mem data.io_names name ->
            (* XXX: HACK: Version <= 1.0.6 compatibility *)
            out_line item;
            out_line item';
            print_out out_line (StringSet.add printed name) data orest n
       | (Old _ :: _), ((_, name, _) as item :: orest) when StringSet.mem data.io_names name ->
            out_line item;
            print_out out_line (StringSet.add printed name) data orest n
       | (Old oname :: nrest), ((_, name, _) :: orest) when StringSet.mem data.new_names name && not (StringSet.mem printed name) ->
            List.iter out_line (Hashtbl.find_all data.old_items oname);
            data.io_names <- StringSet.remove data.io_names oname;
            print_out out_line (StringSet.add printed oname) data o nrest
       | (Old _ :: _), _ :: orest ->
            print_out out_line printed data orest n
       | (Old _ :: _), [] ->
            raise (Invalid_argument "Ascii_io.print_out")

   let output_term inputs ctrl t =
      let data = init_data inputs in
      ignore (out_term ctrl data t);
      print_out ctrl.out_line StringSet.empty data (List.rev !inputs) (List.rev data.out_items)

   let simple_name_op _ _ = "","o"
   let simple_name_param _ = "","p"
   let simple_name_term _ = "","t"
   let simple_name_bterm _ = "","b"
   let simple_name_hyp _ = "","h"
   let simple_name_seq _ = "","s"

   let output_aux strs =
      String.concat " " (List.map Lm_ctype.quote strs)

   let simple_output_line out (str1, str2, strs) =
      output_string out (Lm_ctype.quote str1);
      output_char out '\t';
      output_string out (Lm_ctype.quote str2);
      output_char out '\t';
      let s = (String.concat "\\\\ " (List.map output_aux strs)) in
      let l = String.length s - 1 in
         if l>=0 && s.[l] = ' ' then begin
            s.[l] <- '\n'; output_string out s
         end else begin
            output_string out s; output_char out '\n'
         end

   let make_simple_control out =
    { out_name_op = simple_name_op;
      out_name_param = simple_name_param;
      out_name_term = simple_name_term;
      out_name_bterm = simple_name_bterm;
      out_name_hyp = simple_name_hyp;
      out_name_seq = simple_name_seq;
      out_line = simple_output_line out }

   let write_term outx table term =
      output_term table (make_simple_control outx) term
end

module AsciiIO = MakeAsciiIO (Refiner.Refiner)
