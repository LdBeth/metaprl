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
 * Copyright (C) 2000, Vladimir N. Krupski, Cornell University
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
 * Author: Vladimir N. Krupski
 * Modified by: Aleksey Nogin
 *)
open Lm_symbol

open Printf
open Lm_debug

open Refiner.Refiner
open RefineError
open TermType
open Term
open TermSubst
open TermMan

(***********************
 * BASIC SUBSTITUTIONS *
 ***********************)

type term_subst = (var * term) list
type eqnlist = (term*term) list

let eqnlist_empty = []

let eqnlist_append_eqn l t1 t2 = (t1,t2) :: l

let eqnlist_append_var_eqn v t l = (mk_var_term v, t) :: l

let eqnlist_append_eqns l es  = es @ l

let eqnlist2ttlist x = x

(*
 * Collect all free vars
 *)
let rec collect_vars = function
   [] -> SymbolSet.empty
 | (t1,t2) :: eqs -> SymbolSet.union (free_vars_set t1) (SymbolSet.union (free_vars_set t2) (collect_vars eqs))

let new_eqns_var eqs v = new_name v (SymbolSet.mem (collect_vars eqs))

let is_free_var v t = SymbolSet.mem (free_vars_set t) v

(******************
 * MM UNIFICATION *
 ******************)

type fun_name= TermType.operator
type var_name = Vinit | V of var

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
and variable = { mutable name: var_name; mutable m_v: multeq }
and temp_multeq =
  { mutable s_t: variable Queue.t;
    mutable m_t: multiterm list }
and multiterm =
  { mutable fsymb: mm_operator;
    mutable args: temp_multeq list }

and binding = ((bound_variable list) array) array

and bound_variable = {mutable name_bv: var_name;
                      mutable fsymb_bv: op_with_binding;
                      mutable arg_numb: int;
                      mutable binding_numb: int
                     }

and op_with_binding = {mutable opsymb: fun_name;
                       mutable oparity_n: int;
                       mutable oparity_a: int array;
                       mutable b_length: int;
                       mutable opbinding: binding;
                       mutable renamings: ((var_name) array) array;
                       mutable timestamp: int
                      }

and mm_operator = Op of op_with_binding
                | Bvar of bound_variable
                | Cnst of var_name

exception Cycle
exception Clash
(*
exception Field_m_in_multeq_must_be_single_element_or_empty_list
exception Field_m_t_in_temp_multeq_must_be_single_element_or_empty_list
exception Both_fields_in_temp_multeq_cannot_be_empty
*)
exception Mm_unif_error

        (* val dummy : fun_name *)
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
let init_temp_multeq = { s_t = Queue.create ();
                         m_t = [init_multiterm]
                       }

let init_timestamp_ref = ref 0

(* val new_ts  unit -> int generates new timestamp *)
let new_ts () = incr init_timestamp_ref;
                (!init_timestamp_ref)

let opsymb_equal op1 op2 =
   let op1 = dest_op op1 in
   let op2 = dest_op op2 in
      Opname.eq op1.op_name op2.op_name && op1.op_params = op2.op_params

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
          Op op0, Op op1 ->
           if not (
                   (opsymb_equal op0.opsymb op1.opsymb) &&
                   (op0.oparity_n == op1.oparity_n ) &&
                   (op0.oparity_a = op1.oparity_a )
                  ) then false
           else
            begin
              if op0.b_length < op1.b_length then
                    begin
                    let temp = ref op0.opbinding in
                      op0.opbinding<-op1.opbinding;
                      op1.opbinding<-(!temp)
                    end;
              op0.timestamp <- current_ts;
              for i=0 to (op0.oparity_n - 1) do
               for j=0 to ((op0.oparity_a).(i) - 1) do
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
        | Cnst c0, Cnst c1 -> c0 == c1
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
      | _ -> true

and test_timestamps_in_temp_multeq tmeq ts =
    if ( (Queue.length tmeq.s_t) = 0 ) then (test_timestamps tmeq.m_t ts)
                                       else true

        (* val select_multeq : upart -> multeq *)

let select_multeq u = match u.zero_counter_multeq with
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
                     with Queue.Empty -> (match hd.m_t with
                                           [multit] ->
                                             ( (reduce multit)@l0 , hd::l1 )
                                           | _ -> raise Mm_unif_error
                                       (* Both_fields_in_temp_multeq_cannot_be_empty *)
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
               (let multeq1ref = ref init_multeq in
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
             if ((!multeq0ref).counter = 0) then
               u.zero_counter_multeq <- (!multeq0ref)::(u.zero_counter_multeq)
           )

and merge_multeq meq0ref meq1ref u =
       if ((!meq0ref) != (!meq1ref)) then
       begin
         (let temp =ref (!meq0ref) in
          if ( (!meq0ref).var_number < (!meq1ref).var_number ) then
              begin
                meq0ref:=(!meq1ref);
                meq1ref:=(!temp)
              end
         );
         (!meq0ref).counter<-(!meq0ref).counter + (!meq1ref).counter;
         (!meq0ref).var_number<-(!meq0ref).var_number + (!meq1ref).var_number;
         List.iter (function v -> (v.m_v <- (!meq0ref);
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
                       [] ->if (not (test_timestamps m0 current_ts) ) then raise Clash;
                            m0
                     | [multit1] -> (
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
           end)
                     | _ -> raise Mm_unif_error
                  (* Field_m_t_in_temp_multeq_must_be_single_element_or_empty_list *)
                     )
      | _ -> raise Mm_unif_error
                  (* Field_m_t_in_temp_multeq_must_be_single_element_or_empty_list *)

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
                    | _ -> raise Mm_unif_error
                      (* Field_m_in_multeq_must_be_single_element_or_empty_list *)
                   );
                  r.t <- (!mult)::(r.t)
                 );
                 mm_unify_with_global_mult r
               )
                  in
    mm_unify_with_global_mult

(***********************
 * INTERFACE FUNCTIONS *
 ***********************)

let seq_unsupported = Invalid_argument "Sequents are not currently supported by unify_mm"
let impossible = Invalid_argument "Bug in unify_mm: this is not supposed to happen"

(* get_variable : here x is a string.
 * As a side effect it updates u.equations and the hashtable
 * for known variables  -- var_hashtable; it also increments counter
 * in corresponding multiequation when found
 *)
let get_variable x u var_hashtbl=
              try (let v = (Hashtbl.find var_hashtbl x )
                        in
                        v.m_v.counter <- v.m_v.counter + 1;
                        v
                    )
              with Not_found -> (let v = {name = (V x) ;
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

let rec get_bvars = function
   [] -> []
 | bt::btrms -> (dest_bterm bt).bvars :: get_bvars btrms

let rec get_bterms = function
   [] -> []
 | bt::btrms -> (dest_bterm bt).bterm :: get_bterms btrms

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
let null_var = Lm_symbol.add ""

let rec cterm2multiterm t consts u var_hashtbl b_assoclist =
     let t = dest_term t in
     let tbvs_list = get_bvars t.term_terms
     and tbcore_list = get_bterms t.term_terms
     in
     let op_n =List.length tbvs_list
     in
     let op_a = Array.of_list
                  (List.map List.length tbvs_list
                  )
     in
     let fs = { opsymb = t.term_op;
                oparity_n = op_n;
                oparity_a = op_a ;
                b_length = 1;
                opbinding = (Array.init op_n
                             (function i -> (Array.create op_a.(i) [] )
                             )
                            );
                renamings = (Array.init op_n
                             (function i -> (Array.create op_a.(i) (V null_var) )
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
                      (let bv ={name_bv = (V v);
                                  fsymb_bv = fs;
                                  arg_numb = (!i);
                                  binding_numb =(!j)
                                 }
                         in
                           ((fs.opbinding).(!i)).(!j)<- [bv];
                           b_aslist_ref:= (v,bv)::(!b_aslist_ref);
                           incr j
                      )
                     ) l ;
           incr i;
           (!b_aslist_ref)
         )
                     in
     multit.args <-
     targs2args tbcore_list consts u var_hashtbl (List.map conv_list tbvs_list);
                                         (* this List.map... makes
                                            the correct multit.fsymb
                                            as a side effect
                                         *)
     multit

(* converts targs into args *)
and targs2args li consts u var_hashtbl b_asslistlist =
    let f tt b_asslist =
       term2temp_multeq tt consts u var_hashtbl b_asslist
    in
       List.map2 f li b_asslistlist

and term2temp_multeq tt consts u var_hashtbl b_asslist =
   if is_var_term tt then
      let x = dest_var tt in
         try { s_t = Queue.create () ;
               m_t = [{fsymb = Bvar (List.assoc x b_asslist); args = []}]
             }
         with Not_found ->
            if (SymbolSet.mem consts x) then
               { s_t = Queue.create () ;
                 m_t = [{fsymb = (Cnst (V x)); args = []}]
                }
            else begin
               let q = Queue.create() in
                  Queue.add (get_variable x u var_hashtbl) q;
                  { s_t = q ; m_t =[]}
            end
   else
      { s_t = Queue.create () ;
        m_t = [(cterm2multiterm tt consts u var_hashtbl b_asslist) ]
      }

(* converts 2 terms into temp_multieq *)

let rec terms2temp_multieq t0 t1 consts u var_hashtbl b_asslist0 b_asslist1 =
   if is_var_term t0 then
      let x = dest_var t0 in
      if is_var_term t1 then
         let y = dest_var t1 in
            (try let vx=(List.assoc x b_asslist0) in
                try let vy=(List.assoc y b_asslist1) in
                 (let multit0 ={fsymb=Bvar vx;
                                args=[]
                               }
                  and multit1 ={fsymb=Bvar vy;
                                args=[]
                               }
                    in
                  if (header_equal multit0 multit1 (-1)) then
                     { m_t = [multit0];
                       s_t = Queue.create () }
                  else (raise Clash)
                 )
                with Not_found -> (raise Clash)
            with Not_found ->
               ((if (List.mem_assoc y b_asslist1) then raise Clash);
                  match SymbolSet.mem consts x, SymbolSet.mem consts y with
                  true,true ->
                     if x=y then
                      { m_t =
                           [{ fsymb = (Cnst (V x));
                              args = [] }];
                        s_t = Queue.create () }
                     else raise Clash
                | true,false ->
                     let q = Queue.create () in
                     Queue.add (get_variable y u var_hashtbl) q;
                     { m_t = [{fsymb = (Cnst (V x));
                               args = [] }];
                       s_t = q }
                | false,true ->
                     let q = Queue.create () in
                     Queue.add (get_variable x u var_hashtbl) q;
                     { m_t = [{fsymb = (Cnst (V y));
                                args = [] }];
                       s_t = q }
                | false,false ->
                    let q = Queue.create () in
                    Queue.add (get_variable x u var_hashtbl) q;
                    if not (x=y) then
                    Queue.add (get_variable y u var_hashtbl) q;
                    { m_t = []; s_t = q }
               )
            )
      else begin
         if (List.mem_assoc x b_asslist0)||(SymbolSet.mem consts x)
            then raise Clash;
           (* try let vx=(List.assoc x b_asslist0) in (raise Clash )
              with Not_found -> *)
         let q = Queue.create () in
         Queue.add (get_variable x u var_hashtbl) q;
         { m_t = [cterm2multiterm t1 consts u var_hashtbl b_asslist1];
           s_t = q }
      end
   else
      if is_var_term t1 then
         let y = dest_var t1 in
            if (List.mem_assoc y b_asslist1)||(SymbolSet.mem consts y)
               then raise Clash;
            (* try let vy=(List.assoc y b_asslist1) in (raise Clash)
               with Not_found -> *)
            let q = Queue.create () in
               Queue.add (get_variable y u var_hashtbl) q;
               { m_t = [cterm2multiterm t0 consts u var_hashtbl b_asslist0];
                 s_t = q }
      else begin
         let t_0 = dest_term t0 in
         let t_1 = dest_term t1 in
         if not (opsymb_equal t_0.term_op t_1.term_op) then raise Clash;
         let tbvs_list0 = get_bvars t_0.term_terms
         and tbvs_list1 = get_bvars t_1.term_terms
         and tbcore_list0 = get_bterms t_0.term_terms
         and tbcore_list1 = get_bterms t_1.term_terms in
         let op_n0 =List.length tbvs_list0
         and op_n1 =List.length tbvs_list1 in
         if not (op_n0=op_n1) then raise Clash;
         let op_a0 = Array.of_list (List.map List.length tbvs_list0)
         and op_a1 = Array.of_list (List.map List.length tbvs_list1) in
         if not (op_a0=op_a1) then raise Clash;
         let fs = { opsymb = t_0.term_op;
                    oparity_n = op_n0;
                    oparity_a = op_a0 ;
                    b_length = 2;
                    opbinding = (Array.init op_n0
                                 (function i -> (Array.create op_a0.(i) [] )
                                 )
                                );
                    renamings = (Array.init op_n0
                                 (function i -> (Array.create op_a0.(i) (V null_var) )
                                 )
                                );
                    timestamp = (-1)
                   } in
         let multit =
            { fsymb = Op fs ;
              args =
               if op_n0 = 0 then
                  targs2args_for2 [] [] consts u var_hashtbl []
               else
                  let i = ref (-1) in
                  let conv_lists l0 l1 =
                    (* uses lists l0 l1 to set the values in
                     *        fs.opbinding.(!i) :array[1..length(l0)]of bound_variable;
                     *  returns an association list which associates
                     * the bound variable names from l0 l1 with corresponding
                     * bound_variables; finally increments (!i)
                     *)
                     incr i;
                     if (l0=[]) && (l1=[]) then b_asslist0,b_asslist1 else
                     let j = ref 0
                     and b_aslist_ref0 = ref b_asslist0
                     and b_aslist_ref1 = ref b_asslist1 in
                     List.iter2
                        (fun v0 v1 ->
                           let bv0 =
                              { name_bv = (V v0);
                                fsymb_bv = fs;
                                arg_numb = (!i);
                                binding_numb =(!j)
                              }
                           and bv1 =
                              { name_bv = (V v1);
                                fsymb_bv = fs;
                              arg_numb = (!i);
                                binding_numb =(!j)
                              } in
                           ((fs.opbinding).(!i)).(!j)<- [bv0;bv1];
                           b_aslist_ref0:= (v0,bv0)::(!b_aslist_ref0);
                           b_aslist_ref1:= (v1,bv1)::(!b_aslist_ref1);
                           incr j
                        ) l0 l1;
                     (!b_aslist_ref0),(!b_aslist_ref1)
                  in
                     targs2args_for2 tbcore_list0 tbcore_list1 consts u var_hashtbl
                        (List.map2 conv_lists tbvs_list0 tbvs_list1);
                     (* this List.map2... makes
                      * the correct multit.fsymb
                      * as a side effect
                      *)
            }
         in
            { m_t = [multit]; s_t = Queue.create () }
      end

(* converts 2 cores lists cut from targs of 2 terms into args for multiterm;
 * b_assbilistlist is a list of pairs  b_asslist0,b_asslist1  where
 * b_asslist*  are association lists for bindings in 2 terms --
 * contain associations (bound variable name,bound_variable)
 *)
and targs2args_for2 li0 li1 consts u var_hashtbl b_assbilistlist =
        match li0,li1,b_assbilistlist with
          [],[],[] -> []
        | (s0::tli0),(s1::tli1),((b_asl0,b_asl1)::tl) ->
             ( (terms2temp_multieq s0 s1 consts u var_hashtbl b_asl0 b_asl1) ::
               (targs2args_for2 tli0 tli1 consts u var_hashtbl tl)
             )
        | _,_,_ -> raise impossible

let cterms2system t_0 t_1 consts var_hashtbl =
   let u =
      { multeq_number=1;
        zero_counter_multeq=[];
        equations=[] } in
   let v =
      { name = Vinit ;
        m_v = init_multeq } in
   let meq =
      { counter=0;
        var_number=1;
        s=[v];
        m=[] } in
   v.m_v <- meq;
   let ml = [meq] in
   u.equations <- ml;
   u.zero_counter_multeq <- ml;
   meq.m <-
      (terms2temp_multieq (make_term t_0) ( make_term t_1) consts u var_hashtbl [] []).m_t;
   { t=[]; u=u }

let unifiable t0 t1 consts=
   if is_var_term t0 then
      let x = dest_var t0 in
      if is_var_term t1 then
         let y = dest_var t1 in
            (not (SymbolSet.mem consts x)) || (not (SymbolSet.mem consts y)) || x=y
      else
         not (SymbolSet.mem consts x)
    else
      if is_var_term t1 then
         not (SymbolSet.mem consts (dest_var t1))
      else
      let var_hashtbl = (Hashtbl.create 23) in
      let t_0 = dest_term t0 in
      let t_1 = dest_term t1 in
           try
             ignore(mm_unify (cterms2system t_0 t_1 consts var_hashtbl));
             true
           with Clash -> false
              | Cycle -> false

let fofeqnlist =
   List.map (fun (x,_) -> mk_bterm [] x)

let sofeqnlist =
   List.map (fun (_,y) -> mk_bterm [] y)

let unifiable_eqnl l consts =
   let opL = mk_op (Opname.make_opname ["L"]) [] in
      unifiable (mk_term opL (fofeqnlist l)) (mk_term opL (sofeqnlist l)) consts

let alpha_equal_my term0 term1 =
   unifiable term0 term1 (SymbolSet.union (free_vars_set term0) (free_vars_set term1))

(*********************************************************)
(* Conversion from Mm-unif types to Term                 *)
(*********************************************************)

module H_multeq =
struct
   type t = multeq
   let equal = (==)
   let hash = Hashtbl.hash
end
(* Hashtbl for term with keys: multeq *)
module Hashtbl_multeq = Hashtbl.Make(H_multeq)

let rec extract_varstrl l =
         match l with
           [] -> []
         | hd::tl ->(match hd.name with
                       Vinit ->(extract_varstrl tl)
                     | V x   -> x::(extract_varstrl tl)
                    )

(* extracts the string list of variable names from meq; deletes Vinit  *)
let mulieq2varstringl meq = extract_varstrl meq.s

let pick_name bv consts var_hashtbl =
   let oldname=
     (List.hd
       (((bv.fsymb_bv.opbinding).(bv.arg_numb)).(bv.binding_numb))
     ).name_bv in
   let newname =
     (((bv.fsymb_bv.renamings).(bv.arg_numb)).(bv.binding_numb))
     in
   if ( newname = (V null_var) ) then
         ( let namestr = match oldname with
             V oldnamestr ->
                let avoid v = SymbolSet.mem consts v || Hashtbl.mem var_hashtbl v in
                new_name oldnamestr avoid        (* ??? or hash(bv) *)
           | _ -> raise (Invalid_argument "Bug in Unify_mm.pick_name!")
         in
          (
          ((bv.fsymb_bv.renamings).(bv.arg_numb)).(bv.binding_numb) <-
           (V namestr)
          ); (V namestr)
         )
   else newname

let rec multiterm_list2term m consts var_hashtbl sub_hashtbl multeq_hashtbl =
 ( match m with
       [multit] ->
      ( match multit.fsymb with
          Op op_w_b ->( make_term {term_op = op_w_b.opsymb ;
                                  term_terms = List.map2
                                               (function x -> function y ->
                                                (temp_multieq2bterm x y consts var_hashtbl sub_hashtbl multeq_hashtbl )
                                               )
                                               multit.args
                                               (List.map Array.to_list (Array.to_list op_w_b.opbinding))
                                 }
                      )
       | Bvar bv -> (match (pick_name bv consts var_hashtbl) with
                       (V v) -> mk_var_term v
                      |  _ -> raise impossible
                     )
       | Cnst x -> (match x with
                          (V v) -> mk_var_term v
                         |  _ -> raise impossible
                     )
      )
     | _ -> raise impossible
 )

(* given temp_multeq and external bound_variable list constructs bterm *)

and temp_multieq2bterm t_meq b_v_list consts var_hashtbl sub_hashtbl multeq_hashtbl =
   ( make_bterm
      (
           if ( (Queue.length t_meq.s_t) = 0 ) then
                   {bvars=(List.map
                           (function l ->(
                            match
                            (pick_name (List.hd l) consts var_hashtbl)
                            with
                               (V v) -> v
                              | _ -> raise impossible
                                          )
                           )
                           b_v_list
                          );
                    bterm = (multiterm_list2term t_meq.m_t consts var_hashtbl sub_hashtbl multeq_hashtbl )
                   }
           else
                   {bvars=(List.map
                           (function l ->(
                            match
                            (pick_name (List.hd l) consts var_hashtbl )
                            with
                               (V v) -> v
                              | _ -> raise impossible
                                          )
                           )
                           b_v_list
                           );
                    bterm = mk_var_term ( let vari=(Queue.peek t_meq.s_t) in
                                          match vari.name with
                                           (V v) ->(if (not (Hashtbl.mem sub_hashtbl v))
                                                    then
                                                     begin
                                                      try
                                                       let trm= (Hashtbl_multeq.find
                                                                 multeq_hashtbl
                                                                 vari.m_v
                                                                )
                                                       in
                                                       Hashtbl.add sub_hashtbl v trm
                                                      with _ ->()
                                                     end; v
                                                   )                           (* !!!!! tut  *)
                                          | _ -> raise impossible
                                        )
                   }
      )
   )

(* returnes the complete term for substitution
 * and stores it in  multeq_hashtbl  as a side effect
 *)

let multieq2term meq consts var_hashtbl multeq_hashtbl =
       ( match meq.m with
          [] -> (let trm = mk_var_term (match meq.s with
                                           hd::tl ->(match hd.name with
                                                       (V v) -> v
                                                     | Vinit -> (match (List.hd tl).name with
                                                                   (V v) -> v
                                                                 | _ -> raise impossible
                                                                )
                                                    )
                                         | [] -> raise impossible
                                        )
                  in
                  Hashtbl_multeq.add multeq_hashtbl meq trm;
                  trm
                 )
        | [t] ->(let sub_hashtbl = Hashtbl.create 23
                 in
                 let trm= multiterm_list2term meq.m consts var_hashtbl sub_hashtbl multeq_hashtbl
                 and sub_hashtbl2sub tab=(let subref = ref [] in
                                          Hashtbl.iter
                                          (function x ->
                                           (function y ->
                                            subref:=(x,y)::(!subref)
                                          )) tab ;
                                          !subref
                                         )
                 in
                 let sub = (sub_hashtbl2sub sub_hashtbl)
                 in
                 let trm_w_sub = match sub with
                                   [] -> trm
                                 | _  -> apply_subst trm sub
                 in
                 Hashtbl_multeq.add multeq_hashtbl meq trm_w_sub;
                 trm_w_sub
                )
        | _ -> raise impossible
       )

let rec upd_subst varstringl trm sigma =
   match varstringl with
      [] -> sigma
    | x::tl ->
         if is_var_term trm then
            let y = dest_var trm in
               if (x=y) then ( upd_subst tl trm sigma)
               else (x,trm)::( upd_subst tl trm sigma)
         else
            (x,trm)::( upd_subst tl trm sigma)

let solvedpart2subst slvdpt consts var_hashtbl =
   let multeq_hashtbl = (Hashtbl_multeq.create 23)
   in
   (
   let rec sp2s l sigma consts var_hashtbl multeq_hashtbl =
    ( match l with
       [] -> sigma
     | hd::tl -> (let varstringl= (mulieq2varstringl hd)
                  and trm = (multieq2term hd consts var_hashtbl multeq_hashtbl)
                  in
                   sp2s
                    tl
                    (upd_subst varstringl trm sigma)
                    consts
                    var_hashtbl
                    multeq_hashtbl
                 )
    )
   in
   sp2s slvdpt [] consts var_hashtbl multeq_hashtbl
   )

let unify t0 t1 consts=
   if is_var_term t0 then
      let x = dest_var t0 in
      if is_var_term t1 then
         let y = dest_var t1 in
         if SymbolSet.mem consts x then
            if SymbolSet.mem consts y then
               if(x=y) then [] else raise Clash
            else
               [y,t0]
         else
            [x,t1]
      else
         if SymbolSet.mem consts x then raise Clash
         else if (is_free_var x t1) then raise Cycle
         else [x,t1]
   else
      if is_var_term t1 then
         let y = dest_var t1 in
         if SymbolSet.mem consts y then raise Clash
         else if (is_free_var y t0) then raise Cycle
         else [y,t0]
      else
         let t_0 = dest_term t0 in
         let t_1 = dest_term t1 in
         let var_hashtbl = (Hashtbl.create 23) in
           solvedpart2subst
             (mm_unify (cterms2system t_0 t_1 consts var_hashtbl)).t
             consts
             var_hashtbl

let unify_eqnl l1 consts =
   let l = List.rev l1 in
   let opL = mk_op (Opname.make_opname ["L"]) [] in
      unify (mk_term opL (fofeqnlist l) ) (mk_term opL (sofeqnlist l)) consts

(**********************
 * EQNLIST MANAGEMENT *
 **********************)

let update_subst varstringl terml sigma =
   match terml, varstringl with
      [], [] | [_], [] -> sigma
    | [], v::h ->
         (List.map (function x -> x, mk_var_term v) h) @ sigma
    | [t], li ->
         (List.map (function x -> x, apply_subst t sigma) li) @ sigma
    | _ -> raise impossible

let rec multiterm_list2term m consts var_hashtbl=
 ( match m with
       [multit] ->
      ( match multit.fsymb with
          Op op_w_b ->( make_term {term_op = op_w_b.opsymb ;
                                  term_terms = List.map2
                                               (function x -> function y ->
                                                (temp_multieq2bterm x y consts var_hashtbl)
                                               )
                                               multit.args
                                               (List.map Array.to_list (Array.to_list op_w_b.opbinding))
                                 }
                      )
       | Bvar bv -> (match (pick_name bv consts var_hashtbl) with
                       (V v) -> mk_var_term v
                      | _ -> raise impossible
                     )
       | Cnst x -> (match x with
                          (V v) -> mk_var_term v
                         | _ -> raise impossible
                     )
      )
     | _ -> raise impossible
 )

(* given temp_multeq and external bound_variable list constructs bterm *)

and temp_multieq2bterm t_meq b_v_list consts var_hashtbl =
   ( make_bterm
      (
           if ( (Queue.length t_meq.s_t) = 0 ) then
                   {bvars=(List.map
                           (function l ->(
                            match
                            (pick_name (List.hd l) consts var_hashtbl)
                            with
                               (V v) -> v
                              | _ -> raise impossible
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
                              | _ -> raise impossible
                                          )
                           )
                           b_v_list
                           );
                    bterm = mk_var_term (match (Queue.peek t_meq.s_t).name with
                                           (V v) -> v                                 (* !!!!! tut  *)
                                          | _ -> raise impossible
                                        )
                   }
      )
   )

let multieq2terms meq consts var_hashtbl =
       ( match meq.m with
          []  -> []
        | [t] -> [multiterm_list2term meq.m consts var_hashtbl]
        | _   -> raise impossible
       )

let update_eqnlist varstringl terml eqnl =
       match terml with
         [] -> ( match varstringl with
                  []   -> eqnl
                | v::h -> (let vt = mk_var_term v
                            in
                            List.map
                            (function x -> ((mk_var_term x), vt) )
                            h
                           )@ eqnl
               )
       | [t] ->( match varstringl with
                  [] -> eqnl
                | li -> (List.map
                        (function x -> ((mk_var_term x), t ) )
                        li
                        )@ eqnl
                )
       | _ -> raise impossible

let solvedpart2eqnlist slvdpt consts var_hashtbl =
   let rec sp2el l consts var_hashtbl =
    ( match l with
       [] -> []
     | hd::tl -> ( update_eqnlist
                     (mulieq2varstringl hd)
                     (multieq2terms hd consts var_hashtbl)
                     (sp2el
                      tl
                      consts
                      var_hashtbl
                     )
                 )
    )
   in
   sp2el slvdpt consts var_hashtbl

let opL = mk_op (Opname.make_opname ["L"]) []

let unify_eqnl_eqnl l1 consts =
        let l = List.rev l1
        in
        let t_0={term_op = opL; term_terms =(fofeqnlist l)}
        and t_1={term_op = opL; term_terms =(sofeqnlist l)}
        in
         let var_hashtbl = (Hashtbl.create 23)
         in
            solvedpart2eqnlist
             (mm_unify (cterms2system t_0 t_1 consts var_hashtbl)).t
             consts
             var_hashtbl

(*********************
 * WRAPPER FUNCTIONS *
 *********************)

let re_bug = Invalid_argument "Bug: Uncaught RefineError exception in unify_mm"
let unif_error = RefineError("unify_mm", StringError "Mm_unif_error")
let clash_error = RefineError("unify_mm", StringError "Unification failed - clash")
let cycle_error = RefineError("unify_mm", StringError "Unification failed - cycle")

let unifiable term0 term1 const =
   try unifiable term0 term1 const with
      RefineError _ -> raise re_bug
    | Mm_unif_error -> raise unif_error

let unifiable_eqnl l consts =
   try unifiable_eqnl l consts with
      RefineError _ -> raise re_bug
    | Mm_unif_error -> raise unif_error

let unify term0 term1 consts =
   try unify term0 term1 consts with
      Clash -> raise clash_error
    | Cycle -> raise cycle_error
    | RefineError _ -> raise re_bug
    | Mm_unif_error -> raise unif_error

let unify_eqnl l consts =
   try unify_eqnl l consts with
      Clash -> raise clash_error
    | Cycle -> raise cycle_error
    | RefineError _ -> raise re_bug
    | Mm_unif_error -> raise unif_error

let unify_eqnl_eqnl l consts =
   try unify_eqnl_eqnl l consts with
      Clash -> raise clash_error
    | Cycle -> raise cycle_error
    | RefineError _ -> raise re_bug
    | Mm_unif_error -> raise unif_error

let alpha_equal_my term0 term1 =
   try alpha_equal_my term0 term1 with
      RefineError _ -> raise re_bug
    | Mm_unif_error -> raise unif_error
