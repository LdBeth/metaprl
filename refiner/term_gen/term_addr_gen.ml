(************************************************************************
 * Subterm addressing                                                   *
 ************************************************************************)

open Term_sig
open Term_op_sig

module TermAddr (**)
   (Term : TermSig)
   (TermOp : TermOpSig
    with type term = Term.term) =
struct
   open Term
   open TermOp

   type term = Term.term

   (*
    * Address of a subterm.
    *)
   type address =
      Path of int list
    | NthClause of int * bool
    | Compose of address * address

   exception IncorrectAddress of address * term

   (*
    * Constructor.
    *)
   let make_address l =
      Path l

   let nth_hd_address i =
      NthClause (i, true)

   let nth_tl_address i =
      NthClause (i, false)

   let compose_address path1 path2 =
      Compose (path1, path2)

   (*
    * Get a subterm.
    *)
   let rec getnth a term terms i =
      match (terms, i) with
         (hd::_, 0) ->
            hd
       | (hd::tl, _) ->
            getnth a term tl (pred i)
       | ([], _) ->
            raise (IncorrectAddress (a, term))

   (*
    * Follow an explicit path.
    *)
   let rec term_subterm_path a term t = function
      [] ->
         t
    | i::tl ->
         term_subterm_path a term (dest_bterm (getnth a term (dest_term t).term_terms i)).bterm tl

   (*
    * Follow a sequent path to a clause.
    *)
   let rec term_subterm_nthpath a term flag t = function
      0 ->
         if flag then
            match dest_term t with
               { term_op = op; term_terms = bterm :: _ } ->
                  if (dest_op op).op_name == context_opname then
                     t
                  else
                     (dest_bterm bterm).bterm
             | _ ->
                  raise (IncorrectAddress (a, term))
         else
            t
    | i ->
         let { term_op = op; term_terms = bterms } = dest_term t in
            match (dest_term t).term_terms with
               [bterm] ->
                  term_subterm_nthpath a term flag (dest_bterm bterm).bterm (i - 1)
             | ((_ :: bterm2 :: _) as bterms) ->
                  if (dest_op op).op_name == context_opname then
                     term_subterm_nthpath a term flag (dest_bterm (List_util.last bterms)).bterm (i - 1)
                  else
                     term_subterm_nthpath a term flag (dest_bterm bterm2).bterm (i - 1)
             | _ ->
               raise (IncorrectAddress (a, term))

   (*
    * Get the subterm for any type of path.
    *)
   let rec term_subterm term a =
      match a with
         Path addr ->
            term_subterm_path a term term addr
       | NthClause (addr, flag) ->
            term_subterm_nthpath a term flag term addr
       | Compose (addr1, addr2) ->
            term_subterm (term_subterm term addr1) addr2

   (*
    * Replace a subterm at the specified address.
    * Capture is not taken into account.  This function
    * allows the replacement to compute an extra value.
    *)
   let rec path_replace_term a term f t = function
      i::tl ->
         let { term_op = op; term_terms = bterms } = dest_term t in
         let bterms, arg = path_replace_bterm a term f tl i bterms in
            mk_term op bterms, arg
    | [] ->
         f t

   and path_replace_bterm a term f tl i btrms = match (i,btrms) with
      (0, bterm::bterms) ->
         let { bvars = vars; bterm = trm } = dest_bterm bterm in
         let term, arg = path_replace_term a term f trm tl in
            mk_bterm vars term :: bterms, arg
    | (_, bterm::bterms) ->
         let bterms, arg = path_replace_bterm a term f tl (pred i) bterms in
            bterm :: bterms, arg
    | _ ->
         raise (IncorrectAddress (a,term))

   let rec nthpath_replace_term a term flag f t i =
      if i = 0 then
         if flag then
            match dest_term t with
               { term_op = op; term_terms = bterm :: bterms } ->
                  if (dest_op op).op_name == context_opname then
                     f t
                  else
                     let { bvars = vars; bterm = term } = dest_bterm bterm in
                     let term, arg = f term in
                     let bterm = mk_bterm vars term in
                        mk_term op (bterm :: bterms), arg
             | _ ->
                  raise (IncorrectAddress (a, term))
         else
            f t
      else
         match dest_term t with
            { term_op = op; term_terms = [bterm] } ->
               (* Always take subterm if there is only one *)
               let { bvars = vars; bterm = trm } = dest_bterm bterm in
               let term, arg = nthpath_replace_term a term flag f trm (i - 1) in
               let bterm = mk_bterm vars term in
                  mk_term op [bterm], arg
          | { term_op = op; term_terms = ((bterm1 :: bterm2 :: bterms) as bterms') } ->
               if (dest_op op).op_name == context_opname then
                  let args, bterm = List_util.split_last bterms' in
                  let { bvars = vars; bterm = trm } = dest_bterm bterm in
                  let term, arg = nthpath_replace_term a term flag f trm (i - 1) in
                  let bterm = mk_bterm vars term in
                     mk_term op (args @ [bterm]), arg
               else
                  let { bvars = vars; bterm = term' } = dest_bterm bterm2 in
                  let term, arg = nthpath_replace_term a term flag f term' (i - 1) in
                  let bterm = mk_bterm vars term in
                     mk_term op (bterm1 :: bterm :: bterms), arg
          | _ ->
               raise (IncorrectAddress (a, term))

   let rec apply_fun_arg_at_addr f a term =
      match a with
         Path addr ->
            path_replace_term a term f term addr
       | NthClause (addr, flag) ->
            nthpath_replace_term a term flag f term addr
       | Compose (addr1, addr2) ->
            apply_fun_arg_at_addr (apply_fun_arg_at_addr f addr2) addr1 term

   let add_unit_arg f t =
      f t, ()

   let apply_fun_at_addr f a term =
      fst (apply_fun_arg_at_addr (add_unit_arg f) a term)

   let replace_subterm_aux subterm term = subterm

   let replace_subterm term a subterm =
         apply_fun_at_addr (replace_subterm_aux subterm) a term

   (*
    * Print address as a string.
    *)
   let rec collect_string_of_path_address = function
      [] ->
         ""
    | [h] ->
         string_of_int h
    | h::t ->
         (string_of_int h) ^ "; " ^ (collect_string_of_path_address t)

   let rec collect_string_of_nthpath_address_true = function
      0 ->
         "0"
    | i ->
         "@; " ^ (collect_string_of_nthpath_address_true (i - 1))

   let rec collect_string_of_nthpath_address_false = function
      0 ->
         ""
    | 1 ->
         "@"
    | i ->
         "@; " ^ (collect_string_of_nthpath_address_false (i - 1))

   let rec collect_string_of_address = function
      Path addr ->
         collect_string_of_path_address addr
    | NthClause (addr, flag) ->
         (if flag then
            collect_string_of_nthpath_address_true
         else
            collect_string_of_nthpath_address_false) addr
    | Compose (addr1, addr2) ->
         let addr1 = collect_string_of_address addr1 in
         let addr2 = collect_string_of_address addr2 in
            if addr1 = "" then
               addr2
            else if addr2 = "" then
               addr2
            else
               addr1 ^ "; " ^ addr2

   let string_of_address addr =
      "[" ^ collect_string_of_address addr ^ "]"
end
