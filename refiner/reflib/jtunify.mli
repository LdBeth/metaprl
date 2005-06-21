open Jlogic_sig
open Jtypes
open Jordering

(* Debugging *)

val print_equations :
   (string list * (string list * string list)) list -> unit

val print_tunify : int * (string * string list) list -> unit

module type JQuantifierSig =
sig

   val build_ordering :
      position list ->
      position list ->
      (position * Set.t) list ->
      (position * 'a * 'b) list ->
      (position * Set.t) list

   val shorten : 'a list -> 'a list -> ('a list * 'a list)

   val add_fo_eqlist : 'a list -> 'a list -> 'a list

   val result_qmax : int -> int
end

module JQuantifier (JLogic : JLogicSig) : JQuantifierSig

module JTUnifyQ (JLogic : JLogicSig) :
sig

	val do_stringunify :
		position list ->
		position list ->
		position ->
		position ->
		(position list * (position list * position list)) list ->
		(position list * (position list * position list)) list ->
		(position * Set.t) list ->
		(position * 'a * 'b) list ->
		int ->
		(int * (position * position list) list) * (* unifier *)
		(int * (position list * (position list * position list)) list) *
      (* applied new eqlist *)
		(position * Set.t) list

end

module JTUnifyProp (JLogic : JLogicSig) :
sig

   val do_stringunify :
      position list ->
      position list ->
      position ->
      position ->
      (position list * (position list * position list)) list ->
      (position list * (position list * position list)) list ->
      (position * Set.t) list ->
      (position * 'a * 'b) list ->
      int ->
      (int * (position * position list) list) * (* unifier *)
      (int * (position list * (position list * position list)) list) *
      (* applied new eqlist *)
      (position * Set.t) list

end
