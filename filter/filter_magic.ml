(*
 * Magic numbers for interactive files.
 *)
let int_term_sig_magic = 0x63ac6be1
let int_raw_sig_magic  = 0x63ac6be2
let int_term_str_magic = 0x63ac6be3
let int_raw_str_magic  = 0x63ac6be4
let int_lib_sig_magic  = 0x63ac6be5
let int_lib_str_magic  = 0x63ac6be6
let interactive_magics =
   [int_term_sig_magic;
    int_raw_sig_magic;
    int_term_str_magic;
    int_raw_str_magic;
    int_lib_sig_magic;
    int_lib_str_magic]

(*
 * $Log$
 * Revision 1.1  1998/06/16 16:28:18  jyh
 * Added magic numbers.
 *
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
