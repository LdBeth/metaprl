(* The unification of terms with bindings based on the ideas of
 * Martelli&Montanary unification algorithm
 *------------------------------------------------------------
 *
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/index.html for information on Nuprl,
 * OCaml, and more information about this system.
 *
 * Copyright (C) 1998 Alexey Nogin, Cornell University
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
 * Author: Vladimir Krupski
 * 
 *)




open Printf
open Mp_debug

open Refine_error_sig
open Term_ds_sig
open Term_ds

module TermSubstMm
(Term : TermDsSig
        with type level_exp_var = TermType.level_exp_var
        with type level_exp = TermType.level_exp
        with type param = TermType.param
        with type operator = TermType.operator
        with type term = TermType.term
        with type term_core = TermType.term_core
        with type bound_term = TermType.bound_term
        with type esequent = TermType.esequent
        with type seq_hyps = TermType.seq_hyps
        with type seq_goals = TermType.seq_goals
        with type string_set = TermType.StringSet.t
        with type hypothesis = TermType.hypothesis

        with type level_exp_var' = TermType.level_exp_var'
        with type level_exp' = TermType.level_exp'
        with type object_id = TermType.object_id
        with type param' = TermType.param'
        with type operator' = TermType.operator'
        with type term' = TermType.term'
        with type bound_term' = TermType.bound_term'

        with type term_subst = TermType.term_subst)
(RefineError : RefineErrorSig
               with type level_exp = TermType.level_exp
               with type param = TermType.param
               with type term = TermType.term
               with type bound_term = TermType.bound_term)
=
struct

   open RefineError
   open TermType
   open Term

   type term = TermType.term
   type param = TermType.param


   type term_subst = TermType.term_subst



   let subst t tl vl =
      do_term_subst (List.combine vl tl) t

   let is_free_var v t = StringSet.mem (term_free_vars t) v

   let free_vars t = StringSet.elements (term_free_vars t)

   (*
    * Collect all binding vars.
    *)
   let rec binding_vars_term t bvars =
      binding_vars_bterms bvars (dest_term t).term_terms

   and binding_vars_bterms bvars = function
      bt::l ->
         binding_vars_bterms (binding_vars_term bt.bterm (List.fold_right StringSet.add bt.bvars bvars)) l
    | [] -> bvars

   let rec binding_vars_set t =
      match get_core t with
         Term t ->
            binding_vars_bterms StringSet.empty t.term_terms
       | Sequent seq ->
            let hyps = seq.sequent_hyps in
            let len = SeqHyp.length hyps in
            let rec coll_hyps i =
               if i = len then binding_vars_term seq.sequent_args StringSet.empty else
                  match SeqHyp.get hyps i with
                     Hypothesis (v,t) ->
                        binding_vars_term t (StringSet.add v (coll_hyps (succ i)))
                   | Context (v,ts) ->
                        List.fold_right binding_vars_term ts (coll_hyps (succ i))
            in
            let goals = seq.sequent_goals in
            let len = SeqGoal.length goals in
            let rec coll_goals i =
               if i = len then coll_hyps 0 else
                  binding_vars_term (SeqGoal.get goals i) (coll_goals (succ i))
            in coll_goals 0
       | FOVar _ -> StringSet.empty
       | Subst _ -> fail_core "binding_vars"
       | Hashed d ->
            binding_vars_set (Weak_memo.TheWeakMemo.retrieve_hack d)

   let binding_vars t =
      StringSet.elements (binding_vars_set t)

   let add_vars vars term =
      StringSet.union vars (term_free_vars term)

   let free_vars_set = function
      [hd] ->
         term_free_vars hd
    | hd :: tl ->
         List.fold_left add_vars (term_free_vars hd) tl
    | [] ->
         StringSet.empty

   let free_vars_terms terms =
      StringSet.elements (free_vars_set terms)

   let is_free_var_list vars terms =
      match vars with
         [] ->
            false
       | _ ->
            let free_set = free_vars_set terms in
               List.exists (fun v -> StringSet.mem free_set v) vars

   let context_vars t =
      match get_core t with
         Sequent seq ->
            let hyps = seq.sequent_hyps in
            let len = SeqHyp.length hyps in
            let rec context_vars i =
               if i = len then [] else
               match SeqHyp.get hyps i with
                  Hypothesis _ -> context_vars (succ i)
                | Context (v,_) -> v::context_vars (succ i)
            in context_vars 0
       | _ -> []


module Names =
struct
     type fun_name= TermType.operator
     and  var_name = Vinit | V of string
end

module Mm_unif =
struct

open Names

type system = { mutable t: multeq list; mutable u: upart }
and upart =
  { mutable multeq_number: int;
    mutable zero_counter_multeq: multeq list;
    mutable equations: multeq list }
and multeq =
  { mutable counter: int;
    mutable var_number: int;
    mutable s: variable list;
    mutable m: multiterm list }
and variable = { mutable name: Names.var_name; mutable m_v: multeq }
and temp_multeq =
  { mutable s_t: variable Queue.t;
    mutable m_t: multiterm list }
and multiterm =
  { mutable fsymb: mm_operator;
    mutable args: temp_multeq list }

(* part for bound terms *)
and binding = ((bound_variable list) array) array

and bound_variable = {mutable name_bv:Names.var_name;
                      mutable fsymb_bv:op_with_binding;
                      mutable arg_numb:int;
                      mutable binding_numb:int
                     }

and op_with_binding = {mutable opsymb:Names.fun_name;
                       mutable oparity_n:int;
                       mutable oparity_a:int array;
                       mutable b_length: int;
                       mutable opbinding:binding;
                       mutable renamings:((var_name) array) array;
                       mutable timestamp:int
                      }

and mm_operator =  Op of op_with_binding
                 | Bvar of bound_variable
                 | Cnst of Names.var_name

(* till here *)


exception Cycle
exception Field_m_in_multeq_must_be_single_element_or_empty_list
exception Field_m_t_in_temp_multeq_must_be_single_element_or_empty_list
exception Both_fields_in_temp_multeq_cannot_be_empty
exception Clash

        (* val dummy : Names.fun_name *)
let dummy = Vinit

        (* val init_multiterm : multiterm *)
let init_multiterm ={fsymb=(Cnst dummy) ; args =[] }

        (* val init_multeq : multeq *)
let init_multeq ={ counter=0;
                   var_number=0;
                   s=[];
                   m=[init_multiterm]
                  }

        (* val init_temp_multeq : temp_multeq *)
let init_temp_multeq =   { s_t = Queue.create ();
                           m_t = [init_multiterm]
                         }

let init_timestamp_ref = ref 0

(* val new_ts  unit -> int generates new timestamp *)
let new_ts () = init_timestamp_ref:=(!init_timestamp_ref)+1;
                (!init_timestamp_ref)

let opsymb_equal sy0 sy1 =  (sy0 = sy1)                (* !!!!!? operator_eq *) 

        (* val header_equal : multiterm -> multiterm -> bool *)
           (* all the job of merging the bindings we try to implement
            * as a side effect of header_equal when it returnes TRUE
            * in the case multit0.fsymb is (Op of op_with_binding).
            * The updated binding we write into multit0.
            *)

let header_equal multit0 multit1 current_ts =
   (if (multit0.fsymb != multit1.fsymb) then
    begin
        match multit0.fsymb , multit1.fsymb with
          Op op0, Op op1  ->
           if not (
                   (opsymb_equal op0.opsymb op1.opsymb) &&
                   (op0.oparity_n == op1.oparity_n ) &&
                   (op0.oparity_a = op1.oparity_a )
                  ) then false
           else
            begin
              if op0.b_length < op1.b_length then
                    begin
                    let temp = ref op0.opbinding  in
                      op0.opbinding<-op1.opbinding;
                      op1.opbinding<-(!temp)
                    end;
              op0.timestamp <- current_ts  ;
              for i=0 to (op0.oparity_n - 1) do
               for j=0 to ((op0.oparity_a).(i) - 1)  do
                List.iter
                 (function v ->
                  (v.fsymb_bv <-op0;
                   ((op0.opbinding).(i)).(j) <- v::(((op0.opbinding).(i)).(j))
                  )
                 )
                 ((op1.opbinding).(i)).(j) ;
               done
              done;

              op0.b_length<-op0.b_length + op1.b_length ;
              true
            end


        | Bvar bv0 , Bvar bv1 ->
            (
             (
              ((bv0.fsymb_bv.opbinding).(bv0.arg_numb)).(bv0.binding_numb) ==
              ((bv1.fsymb_bv.opbinding).(bv1.arg_numb)).(bv1.binding_numb)
             )
             && (bv0.fsymb_bv.timestamp = current_ts )
             && (bv1.fsymb_bv.timestamp = current_ts )
            )

        | Cnst c0, Cnst c1   ->  c0 == c1

        | _ , _ -> false
    end
    else ( match multit0.fsymb with
              (Op op) -> (op.timestamp <- current_ts ; true)
            | (Bvar bv) -> (bv.fsymb_bv.timestamp = current_ts )
            | _ -> true
         )
   ) &&
   List.length multit0.args = List.length multit1.args

(* val test_timestamps multiterm list -> int -> bool *)
let rec test_timestamps m ts =
      match m with
        [multit] ->if ( match multit.fsymb with
                          (Op op) -> (op.timestamp <- ts ; true)
                        | (Bvar bv) -> (bv.fsymb_bv.timestamp = ts )
                        | _ -> true
                      ) then    
                            List.for_all  
                            (function tmeq ->
                             (test_timestamps_in_temp_multeq tmeq ts)
                            )
                            multit.args
                        else false
      | _        -> true

and test_timestamps_in_temp_multeq tmeq ts =
    if ( (Queue.length tmeq.s_t) = 0 ) then (test_timestamps tmeq.m_t ts)
                                       else true

        (* val select_multeq : upart -> multeq *)

let select_multeq u = match  u.zero_counter_multeq with
                        [] -> raise Cycle
                      | hd::tl -> (u.zero_counter_multeq <- tl;
                                   u.multeq_number <- u.multeq_number - 1;
                                   hd
                                   )


(* val cut_frontier : temp_multeq list -> temp_multeq list * temp_multeq list
* val reduce : multiterm -> temp_multeq list
*)

let rec cut_frontier arg =
      match arg with
        [] -> ([],[])
      | hd::tl -> ( let (l0,l1) = cut_frontier tl in
                    (try
                      ( hd::l0 , { s_t = (let ttt = Queue.create () in
                                          (Queue.add (Queue.peek hd.s_t) ttt;
                                          ttt)
                                         );
                                   m_t = []
                                 }::l1 )
                     with  Queue.Empty -> (match hd.m_t with
                                           [multit] ->
                                             ( (reduce multit)@l0 , hd::l1 )
                                           | _ -> raise Both_fields_in_temp_multeq_cannot_be_empty
                                          )
                    )
                  )

and reduce multit =
       let (frontier,newargs) = cut_frontier multit.args in
                (multit.args <- newargs;
                 frontier
                )



(*
* val compact : temp_multeq list -> upart -> upart
* val compact_hd : temp_multeq -> upart -> unit
* val merge_multeq : multeq ref -> multeq ref -> upart -> unit
* val merge_multiterms : multiterm list -> multiterm list -> int-> multiterm list
*)

let rec compact frontier u = match frontier with
                              [] -> u
                            | hd::tl -> (compact_hd hd u;
                                         compact tl u
                                        )

and compact_hd meq u =
         let multeq0ref = ref (Queue.take meq.s_t).m_v in
           ( (!multeq0ref).counter <- (!multeq0ref).counter - 1;
               (let multeq1ref = ref init_multeq  in
                while (Queue.length meq.s_t > 0) do
                  multeq1ref:= (Queue.take meq.s_t).m_v;
                  (!multeq1ref).counter <- (!multeq1ref).counter - 1;
                  merge_multeq multeq0ref multeq1ref u;
                done
               );
               (* A side effect of merge_multeq is supposed here:
               *    multeq0ref always points at the resulting merged
               *    multieqation; it is in fact an additional entry to
               *    the UPart list of the system stored in u:upart .
               *)
             (!multeq0ref).m <- merge_multiterms (!multeq0ref).m meq.m_t (new_ts () );

             if  ((!multeq0ref).counter = 0) then
               u.zero_counter_multeq <- (!multeq0ref)::(u.zero_counter_multeq)
           )


and merge_multeq meq0ref meq1ref u =
       if ((!meq0ref) != (!meq1ref)) then
       begin
         (let temp =ref (!meq0ref)  in
          if ( (!meq0ref).var_number < (!meq1ref).var_number ) then
              begin
                meq0ref:=(!meq1ref);
                meq1ref:=(!temp)
              end
         );

         (!meq0ref).counter<-(!meq0ref).counter + (!meq1ref).counter;
         (!meq0ref).var_number<-(!meq0ref).var_number + (!meq1ref).var_number;

         List.iter (function v -> (  v.m_v <- (!meq0ref);
                                    (!meq0ref).s <- v::((!meq0ref).s)
                                  )
                   ) (!meq1ref).s ;
         (*  (!meq1ref).s <- [] ;   try to free something ? *)

         (!meq0ref).m <- merge_multiterms (!meq0ref).m (!meq1ref).m (new_ts () );
         (*  (!meq1ref).m <- [] ;   try to free something ? *)

         u.multeq_number <- u.multeq_number - 1

       end


and merge_multiterms m0 m1 current_ts =
      match m0 with
        [] ->if (not (test_timestamps m1 current_ts) ) then raise Clash;
             m1
      | [multit0] -> (match m1 with
                       [] ->if (not (test_timestamps m1 current_ts) ) then raise Clash;
                            m0
                     | [multit1] ->   (
           if not (header_equal multit0 multit1 current_ts ) then raise Clash
           (* all the job of merging the bindings we try to implement
            * as a side effect of header_equal when it returnes TRUE
            * in the case multit0.fsymb is (Op of op_with_binding).
            * the updated binding we write into multiterm0.
            *)
           else
           begin
               let append_to aa bb =Queue.iter (function x -> (Queue.add x bb )
                                               ) aa
                   (* given <aa< , <bb<   makes   <bbaa< , returnes () *)
             in
               let ggg arg0 arg1 =(append_to arg1.s_t arg0.s_t;
                                   arg0.m_t <- (merge_multiterms arg0.m_t arg1.m_t current_ts )
                                  )
             in
               List.iter2 ggg multit0.args multit1.args;
               m0
           end                        )
                     | _  -> raise Field_m_t_in_temp_multeq_must_be_single_element_or_empty_list
                     )

      | _  -> raise Field_m_t_in_temp_multeq_must_be_single_element_or_empty_list



     (* val mm_unify : system -> system *)

let mm_unify =
        let mult = ref init_multeq in
     let rec mm_unify_with_global_mult r =
          if r.u.multeq_number =0 then r
          else ( mult:= select_multeq r.u;
                (
                  (match (!mult).m with
                      [] -> ()
                    | [multit] -> (r.u <- compact (reduce multit) r.u)
                    | _ -> raise Field_m_in_multeq_must_be_single_element_or_empty_list
                   );
                  r.t <- (!mult)::(r.t)
                 );
                 mm_unify_with_global_mult r
               )
                  in
    mm_unify_with_global_mult
end     (* end Mm_unif *)


module Mm_inter =

struct

open Mm_unif
open Names

(*
type simple_term =  Var of Names.var_name
                  | Cterm of composed_term

and composed_term = {mutable tfsymb: Names.fun_name;
                     mutable targs: simple_bterm list
                    }
and simple_bterm = {mutable tbvars: Names.var_name list;
                    mutable tbcore: simple_term
                   }
*)

exception Impossible
exception Not_supposed
exception Not_done

let is_free = is_free_var

let is_constant  = List.mem

(* get_variable : here x is a string.
 * As a side effect it updates u.equations and the hashtable
 * for known variables  -- var_hashtable; it also increments counter
 * in corresponding multiequation when found
 *)
let get_variable x u var_hashtbl=
              try   (let v = (Hashtbl.find  var_hashtbl x )
                        in
                        v.m_v.counter <- v.m_v.counter + 1;
                        v
                    )
              with Not_found -> (let v  ={name = (V x) ;
                                          m_v = init_multeq
                                         }
                                      in
                                      let meq ={counter=1;
                                                var_number=1;
                                                s=[v];
                                                m=[]
                                               }
                                      in
                                      v.m_v <- meq;
                                      u.equations <- meq :: u.equations;
                                      u.multeq_number <- u.multeq_number + 1;
                                      Hashtbl.add var_hashtbl x v ;
                                      v
                                     )

(* cterm2multiterm :
 * Converts composed_term into multiterm.
 * Now composed_term means term' and is the termcore of the form Term:term'.
 * term' = { term_op : operator; term_terms : bound_term list }
 * It also 
 * updates u.equations and var_hashtable;
 * b_assoclist is the association list containing
 * external (for t) associations --
 * bound variable names (strings) to bound_variables
 *)

let rec cterm2multiterm t consts u var_hashtbl b_assoclist =
     let tbvs_list = (List.map
                      (function sbt -> sbt.bvars)
                       t.term_terms
                     )  (* it is a listlist of bound varstrings from t.term_terms *)

         and tbcore_list = (List.map
                            (function sbt -> sbt.bterm)
                             t.term_terms
                           )       (* it is a list of terms (cores) from t.term_terms *)
     in
     let op_n =List.length tbvs_list
     in
     let op_a = Array.of_list
                  (List.map List.length tbvs_list
                  )
     in
     let fs =    {opsymb = t.term_op;
                  oparity_n = op_n;
                  oparity_a = op_a ;
                  b_length = 1;
                  opbinding = (Array.init op_n
                               (function i -> (Array.create op_a.(i) [] )
                               )
                              );
                  renamings = (Array.init op_n
                               (function i -> (Array.create op_a.(i) (V "") )
                               )
                              );
                  timestamp = (-1)
                 }
     in
     let multit ={fsymb= Op fs ; args =[]}
     in
       let i =ref 0 in
        let conv_list l =
        (* uses list l to set the values in
                 fs.opbinding.(!i) :array[1..length(l)]of bound_variable;
           returns an association list which associates
           the bound variable names from l with corresponding
           bound_variables; finally increments (!i)
        *)
         (let j = ref 0 and b_aslist_ref = ref b_assoclist in
           List.iter (function v ->
                      (  let bv ={name_bv = (V v);
                                  fsymb_bv = fs;
                                  arg_numb = (!i);
                                  binding_numb =(!j)
                                 }
                         in
                           ((fs.opbinding).(!i)).(!j)<- [bv];
                           b_aslist_ref:= (v,bv)::(!b_aslist_ref);
                           j:=(!j)+1
                      )
                     ) l ;
           i:=(!i)+1;
           (!b_aslist_ref)
         )

                     in
     multit.args <-
     targs2args tbcore_list consts u var_hashtbl (List.map  conv_list tbvs_list);
                                         (* this List.map... makes
                                            the correct multit.fsymb
                                            as a side effect
                                         *)
     multit




and targs2args li consts u var_hashtbl b_asslistlist =
     (* converts targs into args *)
           let f tt b_asslist =
             (term2temp_multeq tt consts u var_hashtbl b_asslist) in
         List.map2 f li  b_asslistlist

and term2temp_multeq tt consts u var_hashtbl b_asslist =
       match tt.core with

        FOVar x ->(try { s_t = Queue.create () ;
                         m_t = [{fsymb = Bvar (List.assoc x b_asslist);
                                 args = []
                                }]
                       }

                   with Not_found ->
                    if (is_constant x consts) then
                       { s_t = Queue.create () ;
                         m_t = [{fsymb = (Cnst (V x));
                                 args = []
                                }]
                       }

                  else
                     ( let q = Queue.create() in
                       Queue.add (get_variable x u var_hashtbl) q;
                       { s_t = q ; m_t =[]}
                     )
                  )

       |  Term  s -> { s_t = Queue.create () ;
                       m_t = [(cterm2multiterm s consts u var_hashtbl b_asslist) ]
                     }
       
       | Subst _ -> raise Not_done
       | _ -> raise Not_supposed

(* converts 2 terms into temp_multieq *)

let rec terms2temp_multieq t0 t1 consts u var_hashtbl b_asslist0 b_asslist1 =
      let temp_meq ={s_t=Queue.create (); m_t=[] }  in

   match t0.core , t1.core with
    FOVar x, FOVar y  ->
       (try let vx=(List.assoc x b_asslist0) in
                try let vy=(List.assoc y b_asslist1)  in
                 (let multit0 ={fsymb=Bvar vx;
                                args=[]
                               }
                  and multit1 ={fsymb=Bvar vy;
                                args=[]
                               }
                    in
                  if (header_equal multit0 multit1 (-1)) then
                                  (temp_meq.m_t<- [multit0];
                                   temp_meq
                                  )
                  else (raise Clash)
                 )
                with Not_found -> (raise Clash)

        with Not_found ->
               (  (if (List.mem_assoc y b_asslist1) then raise Clash);
                  match (is_constant x consts),(is_constant y consts) with
                  true,true   ->(if x=y then (temp_meq.m_t <-
                                                  [{fsymb = (Cnst (V x));
                                                    args = []
                                                   }];
                                              temp_meq
                                             )
                                 else raise Clash
                                )
                | true,false  ->
                       (
                       temp_meq.m_t <- [{fsymb = (Cnst (V x));
                                         args = []
                                        }];
                       Queue.add (get_variable y u var_hashtbl) temp_meq.s_t;
                       temp_meq
                       )
                | false,true  ->
                       (
                       temp_meq.m_t <- [{fsymb = (Cnst (V y));
                                         args = []
                                        }];
                       Queue.add (get_variable x u var_hashtbl) temp_meq.s_t;
                       temp_meq
                       )
                | false,false  ->
                   (
                    Queue.add (get_variable x u var_hashtbl) temp_meq.s_t;
                    if not (x=y) then
                    Queue.add (get_variable y u var_hashtbl) temp_meq.s_t;
                    temp_meq
                   )
               )
       )

  | FOVar x, Term t ->
         (
          (if ((List.mem_assoc x b_asslist0)||(is_constant x consts))
           then raise Clash
          );
           (* try let vx=(List.assoc x b_asslist0) in (raise Clash )
              with Not_found -> *)
          Queue.add (get_variable x u var_hashtbl) temp_meq.s_t;
          temp_meq.m_t <- [cterm2multiterm t consts u var_hashtbl b_asslist1];
          temp_meq

         )

  | Term t, FOVar y ->
         (
          (if ((List.mem_assoc y b_asslist1)||(is_constant y consts))
           then raise Clash
          );
           (* try let vy=(List.assoc y b_asslist1) in (raise Clash)
              with Not_found -> *)
          Queue.add (get_variable y u var_hashtbl) temp_meq.s_t;
          temp_meq.m_t <- [cterm2multiterm t consts u var_hashtbl b_asslist0];
          temp_meq

         )

  | Term t_0, Term t_1 ->
   (
     let tbvs_list0 = (List.map
                       (function sbt -> sbt.bvars)
                        t_0.term_terms
                      )  (* it is a listlist of bound varnames from t_0.term_terms *)
     and tbvs_list1 = (List.map
                       (function sbt -> sbt.bvars)
                        t_1.term_terms
                      )  (* it is a listlist of bound varnames from t_1.term_terms *)


     and tbcore_list0 = (List.map
                            (function sbt -> sbt.bterm)
                             t_0.term_terms
                        )       (* it is a list of cores from t_0.term_terms *)
     and tbcore_list1 = (List.map
                            (function sbt -> sbt.bterm)
                             t_1.term_terms
                        )       (* it is a list of cores from t_1.term_terms *)



     in
     let op_n0 =List.length tbvs_list0
     and op_n1 =List.length tbvs_list1

     in
     let op_a0 = Array.of_list
                  (List.map List.length tbvs_list0
                  )
     and op_a1 = Array.of_list
                  (List.map List.length tbvs_list1
                  )


     in

    if not ((opsymb_equal t_0.term_op t_1.term_op)&&(op_n0=op_n1)&&(op_a0=op_a1) )   
         then raise Clash
    else
    begin

       let fs =    {opsymb = t_0.term_op;
                    oparity_n = op_n0;
                    oparity_a = op_a0 ;
                    b_length = 2;
                    opbinding = (Array.init op_n0
                                 (function i -> (Array.create op_a0.(i) [] )
                                 )
                                );
                    renamings = (Array.init op_n0
                                 (function i -> (Array.create op_a0.(i) (V "") )
                                 )
                                );
                    timestamp = (-1)
                   }
       in
       let multit ={fsymb= Op fs ; args =[]}

     in
       let i =ref 0 in
        let conv_lists l0 l1 =
        (* uses lists l0 l1 to set the values in
                 fs.opbinding.(!i) :array[1..length(l0)]of bound_variable;
           returns an association list which associates
           the bound variable names from l0 l1 with corresponding
           bound_variables; finally increments (!i)
        *)
         (let j = ref 0
          and b_aslist_ref0 = ref b_asslist0
          and b_aslist_ref1 = ref b_asslist1
          in
           List.iter2 (function v0 -> function v1 ->
                      (  let bv0 ={name_bv = (V v0);
                                   fsymb_bv = fs;
                                   arg_numb = (!i);
                                   binding_numb =(!j)
                                  }
                         and bv1 ={name_bv = (V v1);
                                   fsymb_bv = fs;
                                   arg_numb = (!i);
                                   binding_numb =(!j)
                                  }
                         in
                           ((fs.opbinding).(!i)).(!j)<- [bv0;bv1];
                            b_aslist_ref0:= (v0,bv0)::(!b_aslist_ref0);
                            b_aslist_ref1:= (v1,bv1)::(!b_aslist_ref1);
                           j:=(!j)+1
                      )
                     ) l0 l1;
           i:=(!i)+1;
           (!b_aslist_ref0),(!b_aslist_ref1)
         )

        in
     multit.args <-
     targs2args_for2 tbcore_list0 tbcore_list1 consts u var_hashtbl
        (List.map2  conv_lists tbvs_list0 tbvs_list1);
        (* this List.map2... makes
           the correct multit.fsymb
           as a side effect
        *)
     temp_meq.m_t <- [multit];
     temp_meq
    end
   )

  | Subst _, _ -> raise Not_done
  | _, Subst _ -> raise Not_done
  | _ , _      -> raise Not_supposed


(* converts 2 cores lists cut from targs of 2 terms into args for multiterm;
*  b_assbilistlist is a list of pairs  b_asslist0,b_asslist1  where
*  b_asslist*  are association lists for bindings in 2 terms --
*  contain associations (bound variable name,bound_variable)
*)
and targs2args_for2 li0 li1 consts u var_hashtbl b_assbilistlist =
        match li0,li1,b_assbilistlist with
          [],[],[] -> []
        | (s0::tli0),(s1::tli1),((b_asl0,b_asl1)::tl) ->
             ( (terms2temp_multieq s0 s1 consts u var_hashtbl b_asl0 b_asl1) ::
               (targs2args_for2 tli0 tli1 consts u var_hashtbl tl)
             )

        | _,_,_ ->    raise Impossible



let cterms2upart t_0 t_1 consts var_hashtbl=
  let u = { multeq_number=0;
            zero_counter_multeq=[];
            equations=[] }

  in
  let vinit =                            (* now Vinit is not hashed !! *)
                (let v  ={name = Vinit ;
                          m_v = init_multeq
                         }
                      in
                 let meq ={counter=1;
                           var_number=1;
                           s=[v];
                           m=[]
                          }
                      in
                      v.m_v <- meq;
                      u.equations <- meq :: u.equations;
                      u.multeq_number <- u.multeq_number + 1;
                      v
                )




  in (
      vinit.m_v.counter <- 0;
      u.zero_counter_multeq <- [vinit.m_v];

      vinit.m_v.m <-
        (terms2temp_multieq (make_term t_0) ( make_term t_1) consts u var_hashtbl [] []).m_t;
      u
     )


let cterms2system t_0 t_1 consts var_hashtbl =
                          {t=[];
                           u= (cterms2upart t_0 t_1 consts var_hashtbl)
                          }







let unifiable term0 term1 consts=
    
     let t0 =( match term0.core with   
                Term _    -> term0.core
              | Subst _   -> (make_term (dest_term term0)).core
              | Sequent _ -> raise Not_supposed
              | FOVar   _ -> term0.core 
              | Hashed  _ -> (make_term (dest_term term0)).core 
             )
     and  t1 =( match term1.core with   
                 Term  _   -> term1.core
               | Subst _   -> (make_term (dest_term term1)).core
               | Sequent _ -> raise Not_supposed
               | FOVar  _  -> term1.core 
               | Hashed _  -> (make_term (dest_term term1)).core 
              )  

   in
   match t0, t1 with

    Term t_0, Term t_1 ->(
      let var_hashtbl = (Hashtbl.create 23) in
           try 
             let trash=mm_unify (cterms2system t_0 t_1 consts var_hashtbl)
             in
             true
           with _ -> false
                          )

  | FOVar x, FOVar y  ->
      ( match (is_constant x consts),(is_constant y consts) with
          true,true    -> x=y
        | _            -> true

      )

  | FOVar x, Term _ -> if (is_constant x consts) then false
                       else true
   

  | Term _ , FOVar y -> if (is_constant y consts) then false
                        else true
 
  | _ ->  raise Not_supposed




let alpha_equal_my term0 term1= unifiable term0 term1 (free_vars_terms [term0; term1])


(* Not ready yet! *)




(*********************************************************)
(* Conversion from Mm-unif types to Term                 *) 
(*                                                       *)
(*********************************************************)


let update_subst varstringl terml sigma =
       match terml with
         []  ->( match varstringl with
                  []    -> sigma
                | v::h  -> (List.map 
                            (function x -> (x, mk_var_term v) ) 
                            h
                           )@ sigma
               )

       | [t] ->( match varstringl with
                  []   -> sigma
                | li   -> (List.map 
                          (function x -> (x , (do_term_subst sigma t )) ) 
                          li
                          )@ sigma
                )

       | _   -> raise Impossible



let rec extract_varstrl l =
         match l with
           [] -> []
         | hd::tl ->(match hd.name with
                       Vinit ->(extract_varstrl tl)
                     | V x   -> x::(extract_varstrl tl)
                    )
 
let mulieq2varstringl meq = extract_varstrl meq.s


let pick_name bv consts var_hashtbl =
   let oldname=
     (List.hd
       (((bv.fsymb_bv.opbinding).(bv.arg_numb)).(bv.binding_numb))
     ).name_bv in
   let newname =
     (((bv.fsymb_bv.renamings).(bv.arg_numb)).(bv.binding_numb))
     in
   if ( newname = (V "") ) then
         ( let namestr = match oldname with
             V oldnamestr ->
                  ( let rec new_var cts av v i =
                        let v' = v ^ "_" ^ (string_of_int i) in
                            try (
                                 match (Hashtbl.find  av  v') with
                                 _ -> (new_var cts av v (succ i))
                                 )
                            with Not_found ->
                                  (if (is_constant  v' cts) then
                                     (new_var cts av v (succ i))
                                   else v'
                                  )
                    in
                    new_var consts var_hashtbl oldnamestr 0 (* ??? or hash(bv) *)
                  )
           | _ -> raise Not_supposed
         in
          (
          ((bv.fsymb_bv.renamings).(bv.arg_numb)).(bv.binding_numb) <-
           (V namestr)
          )   ; (V namestr)
         )
   else newname




let rec  multiterm_list2term m consts var_hashtbl=
 ( match m with

       [multit] ->
      ( match multit.fsymb  with
          Op op_w_b ->( make_term {term_op = op_w_b.opsymb ;
                                  term_terms = List.map2
                                               (function x -> function y ->
                                                (temp_multieq2bterm x y consts var_hashtbl)
                                               )
                                               multit.args
                                               (List.map Array.to_list (Array.to_list op_w_b.opbinding))

                                 }
                                 
                      )
                     


       |  Bvar bv -> (match (pick_name bv consts var_hashtbl) with
                       (V v) ->  mk_var_term  v
                      |   _ -> raise Impossible
                     )

       |  Cnst x  -> (match x with
                          (V v) ->  mk_var_term  v
                         |   _ -> raise Impossible
                     )

      )

     | _ -> raise Impossible
 )


(* given temp_multeq and external bound_variable list constructs bterm *)

and temp_multieq2bterm t_meq b_v_list consts var_hashtbl =
   (   make_bterm       
      (   
           if ( (Queue.length t_meq.s_t) = 0 ) then
                   {bvars=(List.map
                           (function l ->( 
                            match     
                            (pick_name (List.hd l) consts var_hashtbl)
                            with  
                               (V v) -> v 
                              | _ -> raise Impossible 
                                          ) 
                           ) 
                           b_v_list
                          );
                    bterm = (multiterm_list2term t_meq.m_t consts var_hashtbl)
                   }
           else
         
                   {bvars=(List.map
                           (function l ->(
                            match     
                            (pick_name (List.hd l) consts var_hashtbl )
                            with  
                               (V v) -> v 
                              | _ -> raise Impossible
                                          ) 
                           )
                           b_v_list
                           );
                    bterm = mk_var_term (match (Queue.peek t_meq.s_t).name with
                                           (V v) -> v 
                                          | _ -> raise Impossible             
                                        )
                   }
               
        
      
      )
   )



let  multieq2terms meq consts var_hashtbl =
       ( match meq.m with
          []  -> []
        | [t] ->  [multiterm_list2term meq.m consts var_hashtbl]
        | _   -> raise Impossible  
       )






     
let solvedpart2subst slvdpt consts var_hashtbl = 
   let rec sp2s l sigma consts var_hashtbl = 
    ( match l with
       [] -> sigma
     | hd::tl -> (sp2s 
                   tl 
                   (update_subst 
                     (mulieq2varstringl hd) 
                     (multieq2terms hd consts var_hashtbl)  
                     sigma 
                   ) 
                   consts 
                   var_hashtbl 
                 )
    )
   in
   sp2s slvdpt [] consts var_hashtbl





let unify term0 term1 consts=
    
     let t0 =( match term0.core with   
                Term _    -> term0.core
              | Subst _   -> (make_term (dest_term term0)).core
              | Sequent _ -> raise Not_supposed
              | FOVar   _ -> term0.core 
              | Hashed  _ -> (make_term (dest_term term0)).core 
             )
     and  t1 =( match term1.core with   
                 Term  _   -> term1.core
               | Subst _   -> (make_term (dest_term term1)).core
               | Sequent _ -> raise Not_supposed
               | FOVar  _  -> term1.core 
               | Hashed _  -> (make_term (dest_term term1)).core 
              )  

   in
   match t0, t1 with

    Term t_0, Term t_1 ->(
      let var_hashtbl = (Hashtbl.create 23) in

           solvedpart2subst
             (mm_unify (cterms2system t_0 t_1 consts var_hashtbl)).t
             consts
             var_hashtbl 

                          )

  | FOVar x, FOVar y  ->
      ( match (is_constant x consts),(is_constant y consts) with
          true,true    -> (if(x=y) then []
                                  else raise Clash
                          )
        
        | true,false   ->  [(y,term0)]

        | false,true   ->  [(x,term1)]                   

        | false,false  ->  [(x,term1)]   


      )

  | FOVar x, Term _ -> if (is_constant x consts) then raise Clash
                       else if (is_free x term1) then raise Cycle
                                     else [(x,term1)]
   

  | Term _ , FOVar y -> if (is_constant y consts) then raise Clash
                       else if (is_free y term0) then raise Cycle
                                     else [(y,term0)]

 
  | _ ->  raise Not_supposed



end           (* end Mm_inter *)






 

end     (* end TermSubstMy  *)

module TermType = Term_ds.TermType
module AddressType = Term_addr_ds.AddressType
module RefineError = Refine_error.MakeRefineError (TermType) (AddressType)
module Term = Term_base_ds.Term (RefineError)
module TermOp = Term_op_ds.TermOp (Term) (RefineError)

(*
module TermTypeMy = Term_ds.TermType
module AddressTypeMy = Term_addr_ds.AddressType
module RefineErrorMy = Refine_error.MakeRefineError (TermTypeMy) (AddressTypeMy)
module TermMy = Term_base_ds.Term (RefineErrorMy)
module TermOpMy = Term_op_ds.TermOp (TermMy) (RefineErrorMy)


module TermSubst = Term_subst_ds.TermSubst (Term) (RefineError)
module TermAddr = Term_addr_ds.TermAddr (Term) (TermOp) (RefineError)
module TermMan = Term_man_ds.TermMan (Term) (TermOp) (TermAddr) (TermSubst) (
RefineError)
*)

module Unification = TermSubstMm(Term)(RefineError)

let unifiable = Unification.Mm_inter.unifiable

let alpha_equal = Unification.Mm_inter.alpha_equal_my

let unify = Unification.Mm_inter.unify

