module type HypsSig =
  sig
    type var
    and cmp = var * var * int
    and hyps
    and addr
    val get_cmp : hyps -> addr -> cmp
    val get_v1 : hyps -> addr -> var
    val get_v2 : hyps -> addr -> var
    val get_const : hyps -> addr -> int
    val iter : hyps -> (addr -> cmp -> unit) -> unit
  end
module SimpleHyps :
  sig
    type var = string
    and cmp = var * var * int
    and hyps = cmp array
    and addr = int
    val get_cmp : 'a array -> int -> 'a
    val get_v1 : ('a * 'b * 'c) array -> int -> 'a
    val get_v2 : ('a * 'b * 'c) array -> int -> 'b
    val get_const : ('a * 'b * 'c) array -> int -> 'c
    val iter : 'a array -> (int -> 'a -> unit) -> unit
  end
module ArrayTools :
  sig
    val d2_1 : int * int * int -> int
    val d1_2 : int -> int -> int * int * int
    val get : 'a array -> int * int * int -> 'a
    val set : 'a array -> int * int * int -> 'a -> unit
    val init : int -> int -> (int * int * int -> 'a) -> 'a array
    val find : 'a array -> 'a -> int
  end
module Graph :
  functor(Hyps : HypsSig) ->
    sig
      type result =
        | Example of (Hyps.var * int) list
        | Cycle of Hyps.addr list
      and dist = | Disconnected | Int of int * Hyps.addr list
      val maxd : dist -> dist -> dist
      val pos_dist : dist -> bool
      val add_dist : dist array -> int -> int -> int -> int -> dist
      val init_c : Hyps.hyps -> Hyps.var array -> dist array
      val compute : Hyps.hyps -> Hyps.var array -> dist * dist array
      val vars_of_hyps : Hyps.hyps -> Hyps.var array
      val solve : Hyps.hyps -> dist * dist array
    end

open Refiner.Refiner

module TermHyps :
(*   functor(TermType : TermSig) ->
   functor(Term : TermBaseSig with type term=TermType.term 
                                             and type term'=TermType.term') ->
   functor(TermMan : TermManSig with type term=TermType.term) ->
*)
   sig
       type var = Term.term
       and cmp = var * var * int
       and hyps = Term.term * int array
       and addr = int

       val get_cmp :
          TermMan.term * int array -> int -> Term.term * Term.term * int
       val get_v1 : TermMan.term * int array -> int -> Term.term
       val get_v2 : TermMan.term * int array -> int -> Term.term
       val get_const : TermMan.term * int array -> int -> int
       val iter :
           (TermMan.term * int array) ->
           (int -> Term.term * Term.term * int -> unit) -> unit
       val collect : (Term.term -> bool) -> Term.term -> int list
   end

module Test :
  sig
    module SG :
      sig
        type result =
          Graph(SimpleHyps).result =
          | Example of (SimpleHyps.var * int) list
          | Cycle of SimpleHyps.addr list
        and dist =
          Graph(SimpleHyps).dist =
          | Disconnected
          | Int of int * SimpleHyps.addr list
        val maxd : dist -> dist -> dist
        val pos_dist : dist -> bool
        val add_dist : dist array -> int -> int -> int -> int -> dist
        val init_c : SimpleHyps.hyps -> SimpleHyps.var array -> dist array
        val compute :
          SimpleHyps.hyps -> SimpleHyps.var array -> dist * dist array
        val vars_of_hyps : SimpleHyps.hyps -> SimpleHyps.var array
        val solve : SimpleHyps.hyps -> dist * dist array
      end
    val h : SimpleHyps.hyps
    val v : SG.dist * SG.dist array
  end

module TG :
   sig
      type result =
         Graph(TermHyps).result =
         | Example of (TermHyps.var * int) list
         | Cycle of TermHyps.addr list
      and dist =
         Graph(TermHyps).dist =
         | Disconnected
         | Int of int * TermHyps.addr list
      val maxd : dist -> dist -> dist
      val pos_dist : dist -> bool
      val add_dist : dist array -> int -> int -> int -> int -> dist
      val init_c : TermHyps.hyps -> TermHyps.var array -> dist array
      val compute :
         TermHyps.hyps -> TermHyps.var array -> dist * dist array
      val vars_of_hyps : TermHyps.hyps -> TermHyps.var array
      val solve : TermHyps.hyps -> dist * dist array
   end







