open Refiner.Refiner.TermType

val collect : (term -> bool) -> term -> int list

module TG :
sig
   type dist =
      Disconnected
    | Int of int * int list
   val solve : (term * int array) -> dist * dist array
end
