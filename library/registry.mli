open BigInt

type tb
type regtbl
type regval =  bigint option
type regid = string option

val global_registry: regtbl 
val local_registry: regtbl 


val registry_types: string list


val registry_file: string

val define_registry_type: string -> bool -> unit

val clear_registry: bool -> bool -> unit

val registry_lookup_value: string -> string -> regval

val registry_lookup_identifier: string ->bigint -> regid

val registry_store_local: string -> string -> bigint -> unit

val read_string: in_channel -> string
 
val read_number: in_channel -> bigint
 
val read_registry: unit

 (*val default_registry_files: unit ->

val registry_header_text: unit -> string

val generate_registry_declarations &optional ofile file =
*)
           
