
 open Basic
 open Refiner.Refiner.Term

 type 'a oidtable

 val make_oidtable	: unit -> 'a oidtable

 val insert	: 'a oidtable -> stamp -> object_id -> int -> 'a -> unit
 val delete	: 'a oidtable -> stamp -> object_id -> int -> unit

 val commit	: 'a oidtable -> stamp -> object_id -> int -> unit
 val undo	: 'a oidtable -> stamp -> object_id -> int -> unit

 val lookup	: 'a oidtable -> stamp -> object_id -> 'a

 val oidtable_unit_map	: 'a oidtable -> stamp -> (object_id -> 'a -> unit) -> unit
 val oidtable_map	: 'a oidtable -> stamp -> (object_id -> 'a -> 'b option) -> 'b list


