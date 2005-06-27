open Refiner.Refiner.TermType

(* definition: rules, inferences for LJ, LJmc, and LK *)

type rule =
   Ax | Andr | Andl | Orr | Orr1 | Orr2 | Orl | Impr | Impl | Negr | Negl
 | Allr | Alll| Exr | Exl | Fail

type intuit_calc =
   SingleConcl
 | MultiConcl

type calculus =
   Classical
 | Intuit of intuit_calc

module type JLogicSig =
sig
   (* understanding the input *)
	val is_all_term	: term -> bool
	val dest_all : term -> Lm_symbol.var * term * term
	val is_exists_term : term -> bool
	val dest_exists : term -> Lm_symbol.var * term * term
	val is_and_term : term -> bool
	val dest_and : term -> term * term
	val is_or_term : term -> bool
	val dest_or : term -> term * term
	val is_implies_term : term -> bool
	val dest_implies : term -> term * term
	val is_not_term : term -> bool
	val dest_not : term -> term

   (* processing the output *)
   type inference
   val empty_inf : inference
   val append_inf : inference -> term -> term -> rule -> inference
end
