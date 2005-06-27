open Refiner.Refiner.TermType
open Opname

open Jlogic_sig

let free_var_op = make_opname ["free_variable";"Jprover"]
let jprover_op = make_opname ["string";"Jprover"]

type pos_kind =
   EmptyVar
 | Atom
 | Const
 | Dummy
 | EigenVar
 | Var
 | NewVar
 | NewVarQ
 | GammaPos of pos_kind
 | Root

type position = pos_kind * int

let empty_pos = EmptyVar, 0

module JTypes (JLogic : JLogicSig) =
struct
   type polarity = I | O

   type connective = And | Or | Neg | Imp | All | Ex | At | Null

   type ptype = Alpha | Beta | Gamma | Delta | Phi | Psi | PNull

   type stype =
      Alpha_1 | Alpha_2 | Beta_1 | Beta_2 | Gamma_0 | Delta_0 | Phi_0 | Psi_0 | PNull_0

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

   type atom = {aname : string;
                aaddress : int list;
                apos : position;
                aprefix : string list;
                aposprefix : position list;
                apredicate :  operator;
                apol : polarity;
                ast : stype;
                alabel : term}

   type atom_relations = atom * atom list * atom list

(* all atoms except atom occur in alpha_set and beta_set of atom*)

(* beta proofs *)

   type bproof = BEmpty
    | RNode of string list * bproof
    | CNode of (string * string)
    | BNode of string * (string list * bproof) * (string list * bproof)
    | AtNode of string * (string * string)

(* Assume only constants for instantiations, not adapted to terms yet *)
   type inf = string * rule * term * term

(* proof tree for pretty print and permutation *)
   type ptree =
      PEmpty
    | PNodeAx of inf
    | PNodeA of inf * ptree
    | PNodeB of inf * ptree * ptree

end
