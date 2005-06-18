open Lm_string_set

open Refiner.Refiner.Term.TermTypes

open Jlogic_sig
open Jtypes

module JOrdering (JLogic : JLogicSig) :
sig

   val collect_subterms :
    term list ->
    bound_term list ->
    term list

   val collect_delta_terms : term list -> string list

   val print_sigmaQ : (string * term) list -> unit

   val build_orderingJ :
    string list ->
    string list ->
    (string * StringSet.t) list ->
    (JTypes(JLogic).atom * 'a * 'b) list ->
    (string * StringSet.t) list

   val build_orderingJ_list :
    (string * StringSet.elt list) list ->
    (string * StringSet.t) list ->
    (JTypes(JLogic).atom * 'a * 'b) list ->
    (string * StringSet.t) list

   val build_orderingQ :
    (string * term list) list ->
    (string * StringSet.t) list ->
    (string * string list) list *
    (string * StringSet.t) list

end
