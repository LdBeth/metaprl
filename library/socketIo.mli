
val open_client: int -> string -> in_channel* out_channel
val close_client: in_channel* out_channel -> unit
val make_socket: int -> Unix.file_descr
  
val destroy_socket: Unix.file_descr -> unit
val accept_client: Unix.file_descr -> Unix.file_descr
