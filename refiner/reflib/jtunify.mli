


val print_equations : (string list * (string list * string list)) list -> unit



val print_tunify : int * (string * string list) list -> unit 



val do_stringunify : string list ->
                     string list ->
                     string ->
                     string ->
                     (string list * (string list * string list)) list ->
                     int -> 
                     (int * (string * string list) list) *                 (* unifier *)
                     (int * ((string list * (string list * string list)) list))    (* applied new eqlist *)










