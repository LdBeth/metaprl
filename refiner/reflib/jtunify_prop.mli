(* Main function *)

val do_stringunify : string list ->
                     string list ->
                     string ->
                     string ->
                     (string list * (string list * string list)) list ->
                     (int * (string * string list) list) *                 (* unifier *)
                     (int * ((string list * (string list * string list)) list))    (* applied new eqlist *)

