open Printf
open Mp_debug

open Refiner.Refiner
open Refiner.Refiner.Term
open Refiner.Refiner.TermAddr
open Refiner.Refiner.TermMan
open Refiner.Refiner.Refine
open Simple_print.SimplePrint

let string_of_exn exn = Rformat.line_format 120 (fun buf ->  Refine_exn.format_exn Dform.null_base buf exn)
let string_of_term t = Rformat.line_format 120 (fun buf ->  format_simple_term buf t)

let addr_term_str af afa tf t =
   try begin
      let addr = af afa in
      "addr = " ^ (string_of_address addr) ^
      try
         "; term = " ^ string_of_term (tf t addr)
      with
         exn -> "; term exception: " ^ (string_of_exn exn)
   end with
      exn -> "addr exception: " ^ (string_of_exn exn)

try
   let t = <<sequent['arg]{'H; x: 1 + 2; 'J['x]; y: 3 + 4 >- 'C['x;'y]}>> in
   eprintf "t = %s%tnum_hyps t = %i%t" (string_of_term t) eflush (num_hyps t) eflush;
   eprintf "2nd hyp = %s; 2nd binding = %s%t" (string_of_term (TermMan.nth_hyp t 2)) (nth_binding t 2) eflush
with
   exn -> Refine_exn.print_exn Dform_print.null_mode_base stderr "Uncaught exception when testing: " exn; raise exn

