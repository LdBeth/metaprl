(*conversion between nuprl-light terms and mathbus terms*)

open Refiner.Refiner.Term
open Opname
open MathBus


val term_of_mbterm: mbterm -> term
val mbterm_of_term: term -> mbterm

val print_param: param -> unit
val print_term: term -> unit

(*
val param_of_opname: opname ->  param
val opname_of_param: param -> opname
val op_of_params: param list -> operator

val mbparameter_of_param: param -> mbterm
val mbbinding_of_binding: string -> mbterm

val param_of_mbparameter: mbterm -> param
val bvars_of_mbbindings: mbterm -> string list
*)


val write_node_to_file: mbterm -> string -> unit
val read_node_from_file: string -> mbterm
