open Lm_pervasives

(* Previously used somewhere: 0x63ac6be3, 0x63ac6be7, 0x63ac6be9, 0x73ac6be2, 0x73ac6be4, 0x73ac6be5, 0x73ac6be7 *)

(*
 * Magic numbers for interactive files.
 *)
let int_term_sig_magic = 0x63ac6be1
let int_term_str_magic = 0x63ac6be3
let int_lib_sig_magic  = 0x63ac6be5
let int_lib_str_magic  = 0x63ac6be6
let int_raw_sig_magic  = 0x63ac6be8
let int_raw_str_magic  = 0x63ac6bea
let interactive_magics =
   [int_term_sig_magic;
    int_raw_sig_magic;
    int_term_str_magic;
    int_raw_str_magic;
    int_lib_sig_magic;
    int_lib_str_magic]

(*
 * See if the file is interactive by checking the magic number.
 *)
let file_interactive file =
   try
      let inx = open_in_bin file in
         try
            let magic = input_binary_int inx in
               close_in inx;
               List.mem magic interactive_magics
         with
            _ ->
               close_in inx;
               false
   with
      Sys_error _ ->
         false

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
