open Lm_debug
open Lm_symbol
open Lm_printf
open Lm_string_set

open Term_sig
open Refiner.Refiner
(*open Term*)
open TermType
(*open TermOp*)
(*open TermSubst*)
(*open TermMan*)
open RefineError
(*open Opname*)

open Jlogic_sig
open Jtypes

let dest_bterm = Term.dest_bterm
let dest_term = Term.dest_term
let dest_op = Term.dest_op
let dest_param = Term.dest_param
let print_term = Term.print_term

exception Not_unifiable
exception Failed

let rec kind_to_string = function
 | Root ->
      "w"
 | Atom ->
      "a"
 | Const ->
      "c"
 | Dummy ->
      "d"
 | EigenVar ->
      "e"
 | NewVar ->
      "n"
 | Var ->
      "v"
 | EmptyVar ->
      ""
 | NewVarQ ->
      "q"
 | GammaPos k ->
      (kind_to_string k)^"_jprover"

let rec string_to_kind s =
   let len = String.length s in
   if len = 0 then EmptyVar
   else
      if len > 1 then
         let sub = String.sub s 0 (len - 8) in
         GammaPos (string_to_kind sub)
      else
         match String.get s 0 with
            'a' -> Atom
          | 'c' -> Const
          | 'd' -> Dummy
          | 'e' -> EigenVar
          | 'n' -> NewVar
          | 'q' -> NewVarQ
          | 'v' -> Var
          | 'w' -> Root
          |  _  ->
               raise (Invalid_argument ("Unknown kind of position: "^s))

let safe_a2i x =
   try int_of_string x with
      e ->
         raise (Invalid_argument ("Can't extract index from "^x))

let rec extract_gamma k s =
   let len = String.length s in
   if len > 8 then
      extract_gamma (GammaPos k) (String.sub s 8 (len - 8))
   else
      k, safe_a2i s

let string_to_pos s =
   let len = String.length s in
   if len = 0 then EmptyVar,0
   else
      if len > 1 then
         match String.get s 1 with
            '0'..'9' ->
               let n = safe_a2i (String.sub s 1 (len-1)) in
               begin match String.get s 0 with
                  'a' -> Atom,n
                | 'c' -> Const,n
                | 'd' -> Dummy,n
                | 'e' -> EigenVar,n
                | 'n' -> NewVar,n
                | 'q' -> NewVarQ,n
                | 'v' -> Var,n
                |  _  ->
                     raise (Invalid_argument ("Unknown kind of position: "^s))
               end
          | '_' ->
               let sub = String.sub s 1 (len-1) in
               let k =
                  match String.get s 0 with
                     'a' -> Atom
                   | 'c' -> Const
                   | 'd' -> Dummy
                   | 'e' -> EigenVar
                   | 'n' -> NewVar
                   | 'q' -> NewVarQ
                   | 'v' -> Var
                   |  _  ->
                     raise (Invalid_argument ("Unexpected kind of position: "^s))
               in
               extract_gamma k sub
          |  _  ->
               raise (Invalid_argument ("Unknown kind of position: "^s))
      else
         if s="w" then
            Root, 0
         else
            raise (Invalid_argument ("Unknown kind of position: "^s))

let pos_to_symbol (k,i) =
   Lm_symbol.make (kind_to_string k) i

let symbol_to_pos sym =
   let i = to_int sym in
   let s = to_string sym in
   let k = string_to_kind s in
   match k with
      Atom | Const | Dummy | Var | NewVar | NewVarQ | EigenVar | GammaPos _ ->
         k, to_int sym
    | Root | EmptyVar ->
         k, 0

let rec pos_to_string (kind,i) =
   let si = string_of_int i in
   let sk = kind_to_string kind in
   match kind with
      Atom
    | Const
    | Dummy
    | EigenVar
    | NewVar
    | Var
    | NewVarQ
    | GammaPos _ ->
         sk^si
    | EmptyVar | Root ->
         sk

let string_to_symbol s = pos_to_symbol (string_to_pos s)

let gamma_to_simple p =
   let k, i = p in
   match k with
      GammaPos k ->
         k, i
    | EmptyVar|Atom|Const|Dummy|EigenVar|
      Var|NewVar|NewVarQ|Root ->
         let s = pos_to_string p in
         raise (Invalid_argument ("GammaPos was expected instead of: "^s))

let simple_to_gamma (k,i) =
   GammaPos k, i

let string_to_gamma s =
   pos_to_string (simple_to_gamma (string_to_pos s))

let empty_pos = (EmptyVar, 0)
let empty_sym = pos_to_symbol empty_pos
let dummy_pos _ = (Dummy, Lm_symbol.new_number ())

module PosOrdering =
struct

   type t = position

   let rec compare (a,(i:int)) (b,j) =
      match a,b with
       | GammaPos a, GammaPos b ->
            compare (a,i) (b,j)
       | GammaPos _, (EmptyVar|Atom|Const|Dummy|EigenVar|Var|NewVar|NewVarQ|Root) ->
            1
       | (EmptyVar|Atom|Const|Dummy|EigenVar|Var|NewVar|NewVarQ|Root), GammaPos _ ->
            -1
       | EmptyVar,EmptyVar -> Pervasives.compare i j
       | EmptyVar, _ -> -1
       | Atom,EmptyVar -> 1
       | Atom,Atom -> Pervasives.compare i j
       | Atom,_ -> -1
       | Const,(EmptyVar|Atom) -> 1
       | Const,Const -> Pervasives.compare i j
       | Const,_ -> -1
       | Dummy,(EmptyVar|Atom|Const) -> 1
       | Dummy, Dummy -> Pervasives.compare i j
       | Dummy, _ -> -1
       | EigenVar,(EmptyVar|Atom|Const|Dummy) -> 1
       | EigenVar, EigenVar -> Pervasives.compare i j
       | EigenVar, _ -> -1
       | Var,(Atom|Const|Dummy|EmptyVar|EigenVar) -> 1
       | Var,Var -> Pervasives.compare i j
       | Var,_ -> -1
       | NewVar,(Atom|Const|Dummy|EmptyVar|EigenVar|Var) -> 1
       | NewVar,NewVar -> Pervasives.compare i j
       | NewVar,_ -> -1
       | NewVarQ,
           (Atom|Const|Dummy|EmptyVar|EigenVar|Var|NewVar) -> 1
       | NewVarQ,NewVarQ -> Pervasives.compare i j
       | NewVarQ,_ -> -1
       | Root,
          (Atom|Const|Dummy|EmptyVar|EigenVar
          |NewVar|NewVarQ|Var) -> 1
       | Root,Root -> Pervasives.compare i j

end

module Set = Lm_set.LmMake(PosOrdering)

let list_pos_to_string = List.map pos_to_string
let list_string_to_pos = List.map string_to_pos
let set_pos_to_string set =
   StringSet.of_list (List.map pos_to_string (Set.to_list set))
let set_string_to_pos set =
   Set.of_list (List.map string_to_pos (StringSet.to_list set))

module JOrdering (JLogic : JLogicSig) =
struct

   module JTy = JTypes(JLogic)
   open JTy

(* make a term list out of a bterm list *)

   let rec collect_subterms tail = function
      [] -> tail
    | bt::r ->
         ((dest_bterm bt).bterm)::(collect_subterms tail r)

   let rec collect_delta_terms = function
      [] -> []
    | t::r ->
         let dt = dest_term t in
            let {op_name=opname; op_params=params} = dest_op dt.term_op in
               if Opname.eq opname jprover_op then
                  match params with
                     [hd] ->
                        begin match dest_param hd with
                           Term_sig.Var sym ->
                              sym::(collect_delta_terms r)
                         | _ ->
                              raise
                              (Invalid_argument
                               "Unexpected type of parameter of jprover_op")
                        end
                   | _ ->
                        raise
                        (Invalid_argument
                         "Unexpected number of parameters of jprover_op")
               else
                  collect_delta_terms (collect_subterms r dt.term_terms)

(* ***************** REDUCTION ORDERING -- both types **************************** *)

   exception Reflexive

   let rec transitive_irreflexive_closure addset const ordering =
      match ordering with
         [] ->
            []
       | ((pos,fset) as pos_fset)::r ->
            if (pos = const) or (Set.mem fset const) then
(* check reflexsivity during transitive closure wrt. addset ONLY!!! *)
               if Set.mem addset pos then
                  raise Reflexive
               else
                  (pos,(Set.union fset addset))::(transitive_irreflexive_closure addset const r)
            else
               pos_fset::(transitive_irreflexive_closure addset const r)

   let rec search_set var = function
      [] ->
         raise (Invalid_argument ("Jprover: element in ordering missing: "^(pos_to_string var)))
    | (pos,fset)::r ->
         if pos = var then
            Set.add fset pos
         else
            search_set var r

   let add_sets var const ordering =
      let addset =  search_set var ordering in
      transitive_irreflexive_closure addset const ordering

(* ************* J ordering ********************************************** *)

   let rec add_arrowsJ v ordering = function
      [] -> ordering
    | ((Const,i) as f)::r ->
         let new_ordering = add_sets v f ordering in
            add_arrowsJ v new_ordering r
    | _::r ->
         add_arrowsJ v ordering r

   let rec add_substJ replace_vars replace_string ordering atom_rel =
      match replace_vars with
         [] -> ordering
       | ((NewVar | NewVarQ),_)::r -> (* don't integrate new variables *)
            add_substJ r replace_string ordering atom_rel
       | v::r (* no reduction ordering at atoms *)
            when List.exists (fun (x,_,_) -> (x = v)) atom_rel ->
               add_substJ r replace_string ordering atom_rel
       | v::r ->
            let next_ordering = add_arrowsJ v ordering replace_string in
               add_substJ r replace_string next_ordering atom_rel

   let build_orderingJ replace_vars replace_string ordering atom_rel =
      try
         add_substJ replace_vars replace_string ordering atom_rel
      with Reflexive ->        (* only possible in the FO case *)
         raise Not_unifiable    (*search for alternative string unifiers *)

   let rec build_orderingJ_list substJ ordering atom_rel =
      match substJ with
         [] -> ordering
       | (v,vlist)::r ->
            let next_ordering = build_orderingJ [v] vlist ordering atom_rel in
            build_orderingJ_list r next_ordering atom_rel

(* ************* J ordering  END ********************************************** *)

(* ************* quantifier ordering ********************************************** *)

   let rec add_arrowsQ v clist ordering =
      match clist with
         [] -> ordering
       | f::r ->
            let new_ordering = add_sets v f ordering in
            add_arrowsQ v r new_ordering

   let rec print_sigmaQ sigmaQ =
      match sigmaQ with
         [] ->
            print_endline "."
       | (v,term)::r ->
            begin
               open_box 0;
               print_endline " ";
               print_string (v^" = ");
               print_term stdout term;
               force_newline ();
               print_flush ();
               print_sigmaQ r
            end

   let rec print_term_list tlist =
      match tlist with
         [] -> print_string "."
       | t::r ->
            begin
               print_term stdout t;
               print_string "   ";
               print_term_list r
            end

   let rec add_sigmaQ new_elements ordering =
      match new_elements with
         [] -> ([],ordering)
       | (v,termlist)::r ->
            let dterms = collect_delta_terms termlist in
            let dterms = List.map symbol_to_pos dterms in
            begin
(*        open_box 0;
   print_endline " ";
   print_endline "sigmaQ: ";
   print_string (v^" = ");
   print_term_list termlist;
   force_newline ();
   print_stringlist dterms;
   force_newline ();
   print_flush ();
*)
               let new_ordering = add_arrowsQ v dterms ordering in
               let (rest_pairs,rest_ordering) = add_sigmaQ r new_ordering in
               ((v,dterms)::rest_pairs),rest_ordering
            end

   let build_orderingQ new_elements ordering =
(* new_elements is of type (string * term list) list, since one variable can receive more than *)
(* a single term due to substitution multiplication *)
      try
(*   print_endline "build orderingQ in"; *)
         add_sigmaQ new_elements ordering;
      with Reflexive ->
         raise Failed                (* new connection, please *)

(* ************* quantifier ordering  END ********************************************** *)

end
