open Jlogic_sig
open Jtypes
open Jordering

(* Debugging *)

val print_equations :
   (position list * (position list * position list)) list -> unit

val print_tunify : int * (position * position list) list -> unit

val shorten : position list -> position list -> (position list * position list)

module type JQuantifierSig =
sig

   val build_ordering :
      calculus ->
      position list ->
      position list ->
      Set.t PMap.t ->
      Set.t ->
      Set.t PMap.t

   type equation = position list * (position list * position list)
   val add_fo_eqlist : equation list -> equation list -> equation list

   val result_qmax : int -> int
end

module JQuantifier (JLogic : JLogicSig) : JQuantifierSig

module JTUnifyQ (JLogic : JLogicSig) :
sig

	val do_stringunify :
      calculus ->
		position list ->
		position list ->
		position ->
		position ->
		(position list * (position list * position list)) list ->
		(position list * (position list * position list)) list ->
		Set.t PMap.t ->
		Set.t ->
		int ->
		(int * (position * position list) list) * (* unifier *)
		(int * (position list * (position list * position list)) list) *
      (* applied new eqlist *)
		Set.t PMap.t

end

module JTUnifyProp (JLogic : JLogicSig) :
sig

   val do_stringunify :
      calculus ->
      position list ->
      position list ->
      position ->
      position ->
      (position list * (position list * position list)) list ->
      (position list * (position list * position list)) list ->
      Set.t PMap.t ->
      Set.t ->
      int ->
      (int * (position * position list) list) * (* unifier *)
      (int * (position list * (position list * position list)) list) *
      (* applied new eqlist *)
      Set.t PMap.t

end
