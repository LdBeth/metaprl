open Printf
open Nl_debug


open Basic

let _ =
   if !debug_load then
      eprintf "Loading Object_id%t" eflush

type object_id = param list


let make_object_id object_id  = object_id
let dest_object_id object_id  = object_id

let equal_object_ids oida oidb =
 (length oida) = (length oidb)
 & try  for_all2 (fun a b -> (eq_parms_p a b))
   with _ -> false
