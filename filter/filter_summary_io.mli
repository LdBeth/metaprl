(*
 * This module defines an interface for saving information about
 * modules.  We record information about each module interface,
 * to be used in the definition of the module and in submodules.
 *
 *)

open Refiner.Refiner.Term

open File_base_type

open Filter_summary
open Filter_summary_type

(*
 * Create a summary base for a specific format.
 *)
module MakeSummaryBase
   (Address : AddressSig)
   (FileBase : FileBaseSig with type cooked = Address.t) :
      (SummaryBaseSig
       with type cooked = FileBase.cooked
       with type select = FileBase.select)

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
