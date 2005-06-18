open Jlogic_sig

module JTUnify_Q (JLogic : JLogicSig) :
sig

val shorten : 'a list -> 'a list -> 'a list * 'a list

(* Main function *)

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
