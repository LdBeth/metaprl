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
    | NthPath of int * bool
    | Compose of address * address

   exception IncorrectAddress of address * term

   (*
    * Constructor.
    *)
   let make_address l =
      Path l

   let nth_address i flag =
      NthPath (i, flag)

   let compose_address path1 path2 =
      Compose (path1, path2)
   
   (*
    * Get a subterm.
    *)
   let rec term_subterm term a =
      match a with
         Path addr ->
            begin
               let rec aux t = function
                  [] ->
                     t
                | i::tl ->
                     aux (dest_bterm (List.nth (dest_term t).term_terms i)).bterm tl
               in
                  try aux term addr with
                     Not_found ->
                        raise (IncorrectAddress (a, term))
            end
       | NthPath (addr, flag) ->
            begin
               let rec aux t = function
                  0 ->
                     if flag then
                        match dest_term t with
                           { term_terms = bterm::_ } -> (dest_bterm bterm).bterm
                         | _ -> raise (IncorrectAddress (a, term))
                     else
                        t
                | i ->
                     begin
                        match (dest_term t).term_terms with
                           [bterm] ->
                              aux (dest_bterm bterm).bterm (i - 1)
                         | _::bterm::_ ->
                              aux (dest_bterm bterm).bterm (i - 1)
                         | _ ->
                              raise (IncorrectAddress (a, term))
                     end
               in
                  aux term addr
            end
       | Compose (addr1, addr2) ->
            term_subterm (term_subterm term addr1) addr2

   (*
    * Replace a subterm at the specified address.
    * Capture is not taken into account.  This function
    * allows the replacement to compute an extra value.
    *)
   let rec apply_fun_arg_at_addr f a term =
      match a with
         Path addr ->
            begin
               let rec replace t = function
                  i::tl ->
                     let { term_op = op; term_terms = bterms } = dest_term t in
                     let bterms, arg = replace_bterm bterms i tl in
                        mk_term op bterms, arg
                | [] ->
                     f t
               and replace_bterm bterms i tl =
                  if i = 0 then
                     match bterms with
                        bterm :: t ->
                           let { bvars = vars; bterm = term } = dest_bterm bterm in
                           let term, arg = replace term tl in
                              mk_bterm vars term :: t, arg
                      | [] ->
                           raise Not_found
                  else
                     match bterms with
                        bterm :: bterms ->
                           let bterms, arg = replace_bterm bterms (i - 1) tl in
                              bterm :: bterms, arg
                      | [] ->
                           raise Not_found
               in
                  try replace term addr with
                     Not_found ->
                        raise (IncorrectAddress (a, term))
            end
       | NthPath (addr, flag) ->
            begin
               let rec replace t i =
                  if i = 0 then
                     if flag then
                        match dest_term t with
                           { term_op = op; term_terms = bterm :: bterms } ->
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
                           let { bvars = vars; bterm = term } = dest_bterm bterm in
                           let term, arg = replace term (i - 1) in
                           let bterm = mk_bterm vars term in
                              mk_term op [bterm], arg
                      | { term_op = op; term_terms = h :: bterm :: bterms } ->
                           let { bvars = vars; bterm = term } = dest_bterm bterm in
                           let term, arg = replace term (i - 1) in
                           let bterm = mk_bterm vars term in
                              mk_term op (h :: bterm :: bterms), arg
                      | _ ->
                           raise (IncorrectAddress (a, term))
               in
                  replace term addr
            end
       | Compose (addr1, addr2) ->
            let f term =
               apply_fun_arg_at_addr f addr2 term
            in
               apply_fun_arg_at_addr f addr1 term

   let apply_fun_at_addr f a term =
      let f t =
         f t, ()
      in
         fst (apply_fun_arg_at_addr f a term)

   let replace_subterm term a subterm =
      let aux _ = subterm in
         apply_fun_at_addr aux a term

   (*
    * Print a string.
    *)
   let string_of_address addr =
      let rec collect = function
         Path addr ->
            let rec aux = function
               [] ->
                  ""
             | [h] ->
                  string_of_int h
             | h::t ->
                  (string_of_int h) ^ "; " ^ (aux t)
            in
               aux addr
       | NthPath (addr, flag) ->
            let rec aux = function
               0 ->
                  if flag then
                     "0"
                  else
                     ""
             | i ->
                  "@; " ^ (aux (i - 1))
            in
               aux addr
       | Compose (addr1, addr2) ->
            let addr1 = collect addr1 in
            let addr2 = collect addr2 in
               if addr1 = "" then
                  addr2
               else if addr2 = "" then
                  addr2
               else
                  addr1 ^ "; " ^ addr2
      in
         "[" ^ collect addr ^ "]"
end
