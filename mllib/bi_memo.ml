(* This file is part of Nuprl-Light, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/index.html for information on Nuprl,
 * OCaml, and more information about this system.
 *
 * Copyright (C) 1998 Yegor Bryukhov, Moscow State University
 * 
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 * 
 * Author: Yegor Bryukhov
 *)

open Mp_debug
open Printf
open Simplehash_sig

let debug_memo =
   create_debug (**)
      { debug_name = "memo";
        debug_description = "Display bi-memo operations";
        debug_value = false
      }

module ExtBiMemo (Hash: SimpleHashSig) =
struct

type ('params, 'domain, 'd2i, 'i2d, 'image) t =
   { memo_domain_convert : 'params -> 'domain -> 'd2i;
     memo_apply : 'params -> 'd2i -> 'image;
(*     memo_domain_compare : 'domain -> 'domain -> bool;
*)
     memo_image_convert : 'params -> 'image -> 'i2d;
     memo_invert : 'params -> 'i2d -> 'domain;
(*     memo_image_compare : 'image -> 'image -> bool;
*)

     mutable memo_d2i_ref : ('domain, 'image) Hash.t;

     mutable memo_d2i_value : ('d2i, 'image) Hash.t;

     mutable memo_i2d_ref : ('image, 'domain) Hash.t;

     mutable memo_i2d_value : ('i2d, 'domain) Hash.t;
   }

let create fwd_conv fwd_fun dom_comp back_conv back_fun img_comp =
   { memo_domain_convert = fwd_conv;
     memo_apply = fwd_fun;
(*     memo_domain_compare = dom_comp; *)
     memo_image_convert = back_conv;
     memo_invert = back_fun;
(*     memo_image_compare = img_comp; *)

     memo_d2i_ref = Hash.create 17 (==);

     memo_d2i_value = Hash.create 17 dom_comp;

     memo_i2d_ref = Hash.create 17 (==);

     memo_i2d_value  = Hash.create 17 img_comp;
   }

let apply info par arg =
   let table = info.memo_d2i_ref in
   let hash = Hashtbl.hash arg in
   match Hash.seek table hash arg with
      Hash.Some value ->
         (*
          * We have seen this exact application before.
          *)
(*         if !debug_memo then
            eprintf "forward_object F: success%t" eflush;
*)
         value
    | Hash.None ->
         let table = info.memo_d2i_value in
         let d2i_arg = info.memo_domain_convert par arg in
         let d2i_hash = Hashtbl.hash d2i_arg in
            match Hash.seek table d2i_hash d2i_arg with
               Hash.Some value ->
                  (*
                   * This happens when g finds an approximate match,
                   * but the exact pointer equality has not been seen.
                   * Remember this in ref_table.
                   *)
(*                  if !debug_memo then
                     eprintf "forward_arg G: success%t" eflush;
*)
                  Hash.insert info.memo_d2i_ref hash arg value;
                     value
            | Hash.None ->
                 (*
                  * We have never seen the value before.
                  * Remember it in both ref_table and value_table.
                  *)
                 let value = info.memo_apply par d2i_arg in
                 let back_hash = Hashtbl.hash value in
(* ??? *)        let i2d_value = info.memo_image_convert par value in
(* ??? *)        let i2d_hash = Hashtbl.hash i2d_value in
(*                    if !debug_memo then
                       eprintf "forward_value G: failed%t" eflush;
*)
                    Hash.insert info.memo_d2i_ref hash arg value;
                    Hash.insert info.memo_d2i_value d2i_hash d2i_arg value;
                    Hash.insert info.memo_i2d_ref back_hash value arg;
(* ??? *)           Hash.insert info.memo_i2d_value i2d_hash i2d_value arg;
                    value

let invert info par arg =
   let table = info.memo_i2d_ref in
   let hash = Hashtbl.hash arg in
   match Hash.seek table hash arg with
      Hash.Some value ->
         (*
          * We have seen this exact application before.
          *)
(*         if !debug_memo then
            eprintf "forward_object F: success%t" eflush;
*)
         value
    | Hash.None ->
         let table = info.memo_i2d_value in
         let i2d_arg = info.memo_image_convert par arg in
         let i2d_hash = Hashtbl.hash i2d_arg in
            match Hash.seek table i2d_hash i2d_arg with
               Hash.Some value ->
                  (*
                   * This happens when g finds an approximate match,
                   * but the exact pointer equality has not been seen.
                   * Remember this in ref_table.
                   *)
(*                  if !debug_memo then
                     eprintf "forward_arg G: success%t" eflush;
*)
                  Hash.insert info.memo_i2d_ref hash arg value;
                     value
            | Hash.None ->
                 (*
                  * We have never seen the value before.
                  * Remember it in both ref_table and value_table.
                  *)
                 let value = info.memo_invert par i2d_arg in
                 let domain_hash = Hashtbl.hash value in
                 let d2i_value = info.memo_domain_convert par value in
                 let d2i_hash = Hashtbl.hash d2i_value in
(*                    if !debug_memo then
                       eprintf "forward_value G: failed%t" eflush;
*)
                    Hash.insert info.memo_i2d_ref hash arg value;
                    Hash.insert info.memo_i2d_value i2d_hash i2d_arg value;
                    Hash.insert info.memo_d2i_ref domain_hash value arg;
(* ??? *)                    Hash.insert info.memo_d2i_value d2i_hash d2i_value arg;
                    value

let revert {
              memo_domain_convert = from_domain_convert;
              memo_apply = from_apply;
              memo_image_convert = from_image_convert;
              memo_invert = from_invert;
              memo_d2i_ref = from_d2i_ref;
              memo_d2i_value = from_d2i_value;
              memo_i2d_ref = from_i2d_ref;
              memo_i2d_value = from_i2d_value;
           } =
   { 
     memo_domain_convert = from_image_convert;
     memo_apply = from_invert;
(*     memo_domain_compare = dom_comp; *)
     memo_image_convert = from_domain_convert;
     memo_invert = from_apply;
(*     memo_image_compare = img_comp; *)

     memo_d2i_ref = from_i2d_ref;

     memo_d2i_value = from_i2d_value;

     memo_i2d_ref = from_d2i_ref;

     memo_i2d_value  = from_d2i_value;
   }

let extr tt = (Hash.extr tt.memo_d2i_ref, 
               Hash.extr tt.memo_d2i_value, 
               Hash.extr tt.memo_i2d_ref, 
               Hash.extr tt.memo_i2d_value)

end




