
open Term
open Basic
open Filename
open MathBus
open Mbterm
open BigInt
open Unix

type dbtable = (stamp * string, term) Hashtbl.t

let db_cache = (Hashtbl.create 7:dbtable)
let master_pathname = ref ""
let process_pathname = ref ""

let db_init stamp master =
 master_pathname := master;
 let {process_id = pid} = dest_stamp stamp in
 process_pathname := String.concat "" [master; "/"; pid];
 mkdir !process_pathname 999

(*let db_query string =*)

let db_read stamp object_type =
 let {process_id = pid; seq = seq}  = dest_stamp stamp in
 let filename = String.concat "" [!master_pathname; "/"; pid; (string_of_int seq); "."; object_type] in
 let in_channel = try open_in filename with
 Sys_error e -> error ["db_read"; "file"; "not"; "exist"] [] [] in
 let term = term_of_mbterm (read_node in_channel) in
 close_in in_channel;
 Hashtbl.add db_cache (stamp, object_type) term;
 term

let db_write stamp object_type term =
 let {process_id = pid; seq = seq}  = dest_stamp stamp in
 let filename = String.concat "" [!master_pathname; "/"; pid; (string_of_int seq); "."; object_type] in
 let descr = openfile filename [O_EXCL; O_WRONLY; O_CREAT] 999 in
  (write_node (mbterm_of_term term) (out_channel_of_descr descr));
 close descr

