(*
 * Include file to load tptp.
 *)

load "tptp_prove";;
cd "tptp_prove";;
create_tptp "GEN";;
cd "GEN";;
refine testT;;

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
