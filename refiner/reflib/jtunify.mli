open Lm_string_set

open Jlogic_sig

(* Utilities *)

val is_const : string -> bool
val is_var : string -> bool
val r_1 : 'a list -> 'b list -> 'c list -> bool
val r_2 : 'a list -> 'b list -> 'c list -> bool
val r_3 : 'a list -> 'b list -> 'a list -> bool
val r_4 : string list -> 'a list -> string list -> bool
val r_5 : string list -> 'b list -> bool
val r_6 : string list -> 'a list -> string list -> bool
val r_7 : string list -> string list -> bool
val r_8 : string list -> 'a list -> string list -> bool
val r_9 : string list -> 'a list -> string list -> bool
val r_10 : string list -> string list -> bool
val com_subst : 'a -> 'a list -> 'a list -> 'a list
val apply_element : 'a -> 'a list -> 'a list -> 'a list -> 'a list * 'a list

(* Debugging *)

val print_equations : (string list * (string list * string list)) list -> unit

val print_tunify : int * (string * string list) list -> unit

module type JQuantifierSig =
sig

   type atom

   val build_ordering :
      string list ->
      string list ->
      (string * StringSet.t) list ->
                (atom * 'a * 'b) list ->
      (string * StringSet.t) list

   val shorten : 'a list -> 'a list -> ('a list * 'a list)

   val add_fo_eqlist : 'a list -> 'a list -> 'a list

   val result_qmax : int -> int
end

module JQuantifier (JLogic : JLogicSig) : JQuantifierSig
   with type atom = Jtypes.JTypes(JLogic).atom

module JTUnifyQ (JLogic : JLogicSig) :
sig

	val do_stringunify :
		string list ->
		string list ->
		string ->
		string ->
		(string list * (string list * string list)) list ->
		(string list * (string list * string list)) list ->
		(string * Lm_string_set.StringSet.t) list ->
		(Jtypes.JTypes(JLogic).atom * 'a * 'b) list ->
		int ->
		(int * (string * string list) list) * (* unifier *)
		(int * (string list * (string list * string list)) list) * (* applied new eqlist *)
		(string * Lm_string_set.StringSet.t) list

end

module JTUnifyProp (JLogic : JLogicSig) :
sig

	val do_stringunify :
		string list ->
		string list ->
		string ->
		string ->
		(string list * (string list * string list)) list ->
		(string list * (string list * string list)) list ->
		(string * Lm_string_set.StringSet.t) list ->
		(Jtypes.JTypes(JLogic).atom * 'a * 'b) list ->
		int ->
		(int * (string * string list) list) * (* unifier *)
		(int * (string list * (string list * string list)) list) * (* applied new eqlist *)
		(string * Lm_string_set.StringSet.t) list

end
