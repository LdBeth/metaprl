(*
 * Debugging operations.
 *)

module type RewriteDebugSig =
sig
   type rwterm
   type rstack
   type stack
   type varname

   val print_varname : out_channel -> varname -> unit
   val print_prog : out_channel -> rwterm -> unit
   val print_rstack : out_channel -> rstack array -> unit
   val print_stack : out_channel -> stack array -> unit
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
