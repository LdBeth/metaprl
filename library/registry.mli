open Int32


type regtb

val global_registry: regtb
val local_registry: regtb

val registry_types: string list ref

val registry_file: string

val define_registry_type: string -> bool -> unit

val clear_registry: bool -> bool -> unit

val registry_lookup_value: string -> string -> int32
val registry_lookup_identifier: string -> int32 -> string
val registry_store_local: string -> string -> int32 -> unit

val read_string: in_channel -> string
 (*val read_number: in_channel -> num*)
val read_int32 : in_channel -> int32

val read_registry: unit

 (*val default_registry_files: unit ->

val registry_header_text: unit -> string

val generate_registry_declarations &optional ofile file =
*)

