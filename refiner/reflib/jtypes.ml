open Refiner.Refiner.TermType
open Opname

open Jlogic_sig

let free_var_op = make_opname ["free_variable";"Jprover"]
let jprover_op = make_opname ["string";"Jprover"]

type pos_kind =
   EmptyVar
 | Atom
 | Const of int
 | Dummy
 | EigenVar
 | Var of int
 | NewVar of int
 | NewVarQ of int
 | GammaPos of pos_kind
 | Root

type position = pos_kind * int

module JTypes (JLogic : JLogicSig) =
struct
   type polarity = Zero | One

   type connective = And | Or | Neg | Imp | All | Ex | At | Null | Box of int

   type ptype = Alpha | Beta | Gamma | Delta | Phi | Psi | PNull | Pi of int | Nu of int

   type stype =
      Alpha_1 | Alpha_2 | Beta_1 | Beta_2 | Gamma_0 | Delta_0 | Phi_0 | Psi_0 |
      PNull_0 | Pi_0 of int | Nu_0 of int

   type direction =
      Left | Right

   type pos = {address : int list;
               pospos : position;
               op :  connective;
               pol : polarity;
               pt : ptype;
               st : stype;
               label : term}

   type ftree =
      Empty
    | NodeAt of pos
    | NodeA of pos * ftree list

   type atom = {aaddress : int list;
                apos : position;
                aposprefix : position list;
                apredicate : opname;
                apol : polarity;
                ast : stype;
                alabel : term}

   type atom_relations = atom * atom list * atom list

(* all atoms except atom occur in alpha_set and beta_set of atom*)

(* beta proofs *)

   type bproof = BEmpty
    | RNode of position list * bproof
    | CNode of (position * position)
    | BNode of position * (position list * bproof) * (position list * bproof)
    | AtNode of position * (position * position)

(* Assume only constants for instantiations, not adapted to terms yet *)
   type inf = position * rule * term * term

(* proof tree for pretty print and permutation *)
   type ptree =
      PEmpty
    | PNodeAx of inf
    | PNodeA of inf * ptree
    | PNodeB of inf * ptree * ptree

end
