(*
 * There are three types of packages.  This is an interactive
 * package that contains interactive proofs, theorem statements, etc.
 * A package has a name, and a magic number.
 * The binary files is laid out as follows:
 *
 *   1. magic_number of version
 *   2. magic number of file
 *   3. Proofs
 *   4. IO list
 *)

open Printf
open Debug

include Package_type

(*
 * Show that the file is loading.
 *)
let _ =
   if !debug_load then
      eprintf "Loading Package_int%t" eflush

(************************************************************************
 * TYPES                                                                *
 ************************************************************************)

(*
 * The package type.
 *)
type t =
   { mutable pack_name : string option;
     pack_magic : int;
     
     (* Contents *)
     mutable pack_items : item list
   }

(*
 * This is the representation stored in the files.
 *)
magic_block magic_number =
struct
   type io_item =
      IORewrite of io_rewrite_info
    | IOCondRewrite of io_cond_rewrite_info
    | IOAxiom of io_axiom_info
    | IORule of io_rule_info
    | IOOpname of opname_info
    | IOMLTerm of mlterm_info
    | IOParent of module_path
    | IODForm of dform_info
    | IOPrec of prec_info
    | IOInfix of string

   and io_rewrite_info =
      { io_rw_name : string;
        io_rw_redex : term;
        io_rw_contractum : term;
        io_rw_proof : Proof.handle option
      }

   and io_cond_rewrite_info =
      { io_crw_name : string;
        io_crw_params : param list;
        io_crw_args : term list;
        io_crw_redex : term;
        io_crw_contractum : term;
        io_crw_proof : Proof.handle option
      }

   and io_axiom_info =
      { io_axiom_name : string;
        io_axiom_stmt : term;
        io_axiom_proof : Proof.handle option
      }

   and io_rule_info =
      { io_rule_name : string;
        io_rule_params : param list;
        io_rule_stmt : meta_term;
        io_rule_proof : Proof.handle option
      }
end

(************************************************************************
 * IMPLEMENTATION                                                       *
 ************************************************************************)

(*
 * Constructors.
 *)
let create () =
   { pack_name = None;
     pack_magic = Random.bits ();
     pack_items = []
   }

(*
 * Rename the package.
 *)
let rename pack name =
   pack.pack_name <- Some name

(*
 * Add an item to the package.
 *)
let add pack item =
   pack.items <- item :: pack.items 

(*
 * Listing the package.
 *)
let items_of_package { pack_items = items } = items

(************************************************************************
 * IO OPERATIONS                                                        *
 ************************************************************************)

(*
 * Convert an item list to an io_item list.
 *)
let io_item_of_item base = function
   Rewrite { rw_name = name;
             rw_redex = redex;
             rw_contractum = contractum;
             rw_proof = pf
   } ->
      { io_rw_name = name;
        io_rw_redex = redex;
        io_rw_contractum = contractum;
        io_rw_proof = save_proof_option base pf
      }
 | CondRewrite { crw_name = name;
                 crw_params = params;
                 crw_args = args;
                 crw_redex = redex;
                 crw_contractum = contractum;
                 crw_proof = pf
   } ->
      { io_crw_name = name;
        io_crw_params = params;
        io_crw_args = args;
        io_crw_redex = redex;
        io_crw_contractum = contractum;
        io_crw_proof = save_proof_option base pf
      }
 | Axiom { axiom_name = name;
           axiom_stmt = stmt;
           axiom_proof = pf
   } ->
      { io_axiom_name = name;
        io_axiom_stmt = stmt;
        io_axiom_proof = save_proof_option base pf
      }
 | Rule { rule_name = name;
          rule_params = params;
          rule_stmt = stmt;
          rule_proof = pf
   } ->
      { io_rule_name = name;
          io_rule_params = params;
          io_rule_stmt = stmt;
          io_rule_proof = save_proof_option base pf
      }
 | Opname t ->
      IOOpname t
 | MLTerm t ->
      IOMLTerm t
 | Parent p ->
      IOParent p
 | DForm d ->
      IODform d
 | Prec p ->
      IOPrec p
 | Infix s ->
      IOInfix s

(*
 * Convert an item list to an io_item list.
 *)
let item_of_io_item base = function
   IORewrite { io_rw_name = name;
               io_rw_redex = redex;
               io_rw_contractum = contractum;
               io_rw_proof = pf
   } ->
      { rw_name = name;
        rw_redex = redex;
        rw_contractum = contractum;
        rw_proof = load_proof_option base pf
      }
 | IOCondRewrite { io_crw_name = name;
                   io_crw_params = params;
                   io_crw_args = args;
                   io_crw_redex = redex;
                   io_crw_contractum = contractum;
                   io_crw_proof = pf
   } ->
      { crw_name = name;
        crw_params = params;
        crw_args = args;
        crw_redex = redex;
        crw_contractum = contractum;
        crw_proof = load_proof_option base pf
      }
 | IOAxiom { io_axiom_name = name;
             io_axiom_stmt = stmt;
             io_axiom_proof = pf
   } ->
      { axiom_name = name;
        axiom_stmt = stmt;
        axiom_proof = load_proof_option base pf
      }
 | IORule { io_rule_name = name;
            io_rule_params = params;
            io_rule_stmt = stmt;
            io_rule_proof = pf
   } ->
      { rule_name = name;
        rule_params = params;
        rule_stmt = stmt;
        rule_proof = load_proof_option base pf
      }
 | IOOpname t ->
      Opname t
 | IOMLTerm t ->
      MLTerm t
 | IOParent p ->
      Parent p
 | IODForm d ->
      Dform d
 | IOPrec p ->
      Prec p
 | IOInfix s ->
      Infix s

(*
 * Save the package into a file.
 * Save the two magic numbers, then the proofs, then the items
 * in the package.
 *)
let save pack out =
   let { pack_magic = magic_number'; pack_items = items } = pack in
   let base = create_out_base () in
   let items' = List.map (io_item_of_item base) items in
      output_binary_int out magic_number;
      output_binary_int out magic_number';
      save_base base;
      output_value out items'

(*
 * Just get the tactics from the database.
 *)
let restore_tactics inx magic =
   try
      let magic_number' = input_binary_int inx in
         if magic_number = magic_number' then
            let magic' = input_binary_int inx in
               if magic' = magic then
                  Proof.restore_tactics inx
               else
                  raise (Failure "Package_int.restore_tactics: package numbers do not match")
         else
            raise (Failure "Package_int.restore_tactics: bad magic number")
   with
      End_of_file ->
         raise (Failure "Package_int.restore_tactics: premature end of file")

(*
 * Loading the file.
 *)
let restore inx name magic resources tacs =
   try
      let magic_number' = input_binary_int inx in
         if magic_number = magic_number' then
            let magic' = input_binary_int inx in
               if magic' = magic then
                  let base = Proof.restore_base inx in
                  let items = (input_value inx : io_item list) in
                  let items' = restore_items base items resources tacs in
                     { pack_name = name;
                       pack_magic = magic;
                       pack_items = items'
                     }
               else
                  raise (Failure "Package_int.restore: package numbers do not match")
         else
            raise (Failure "Package_int.restore: bad magic number")
   with
      End_of_file ->
         raise (Failure "Package.restore: premature end of file")

(*
 * $Log$
 * Revision 1.2  1998/04/24 02:41:27  jyh
 * Added more extensive debugging capabilities.
 *
 * Revision 1.1  1997/08/06 16:17:17  jyh
 * This is an ocaml version with subtyping, type inference,
 * d and eqcd tactics.  It is a basic system, but not debugged.
 *
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
