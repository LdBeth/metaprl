(************************************************************************
 * Subterm addressing                                                   *
 ************************************************************************)

open Term_ds
open Term_op_ds

module TermAddr =
struct
   open Term

   type term = Term.term

   (*
    * Address of a subterm.
    *)
   type address =
      Path of int list
    | NthPath of int * bool

   exception IncorrectAddress of address * term
   exception BadAddressPrefix of address * address

   (*
    * Constructor.
    *)
   let make_address l = Path l
   
   let make_seq_address i = NthPath (i + 1, true)
   
   let nth_cdr_addr i = NthPath (i, false)
   
   (*
    * Compute arities of subterms.
    *)
   let subterm_arities term =
      let aux bterm = List.length (dest_bterm bterm).bvars in
         List.map aux (dest_term term).term_terms
   
   (*
    * Get a subterm.
    *)
   let term_subterm term =
      function
         (Path addr) as a ->
            begin
               let rec aux t = function
                  [] -> t
                | i::tl -> aux (dest_bterm (List.nth (dest_term t).term_terms i)).bterm tl
               in
                  try aux term addr with
                     Not_found -> raise (IncorrectAddress (a, term))
            end
       | (NthPath (addr, flag)) as a ->
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
   
   let apply_fun_at_addr f a term =
      match a with
         Path addr ->
            begin
               let rec aux t = function
                  [] -> f t
                | i::tl ->
                     match dest_term t with
                        { term_op = op; term_terms = bterms } ->
                           let f bt =
                              let bterm = dest_bterm bt in
                              mk_bterm bterm.bvars (aux bterm.bterm tl)
                           in
                           mk_term op (List_util.replacef_nth i f bterms)
               in
                  try aux term addr with
                     Not_found -> raise (IncorrectAddress (a, term))
            end
       | NthPath (addr, flag) ->
            begin
               let rec aux t = function
                  0 ->
                     if flag then
                        match dest_term t with
                           { term_op = op;
                             term_terms = btrm::bterms
                           } ->
                              let bt = dest_bterm btrm in
                              mk_term op ((mk_bterm bt.bvars (f bt.bterm))::bterms)
                         | _ ->
                              raise (IncorrectAddress (a, term))
                     else
                        f t
                | i ->
                     begin
                        match dest_term t with
                           { term_op = op; term_terms = [btrm] } ->
                              let bt = dest_bterm btrm in
                              mk_term op [mk_bterm bt.bvars (aux bt.bterm (pred i))]
                         | { term_op = op; term_terms = h::btrm::bterms } ->
                              let bt = dest_bterm btrm in
                              mk_term op (h::(mk_bterm bt.bvars (aux bt.bterm (pred i)))::bterms)
                         | _ ->
                              raise (IncorrectAddress (a, term))
                     end
               in
                  aux term addr
            end
   
   let replace_subterm term a subterm =
      let aux _ = subterm in
         apply_fun_at_addr aux a term
   
   (*
    * Subtract two addresses.
    * addr1 must be a prefix of addr2, and it is removed from addr2.
    *)
   let remove_addr_prefix addr1 addr2 =
      match addr1 with
         NthPath (i, flag1) ->
            begin
               match addr2 with
                  NthPath (j, flag2) ->
                     if flag1 then
                        if flag2 & i = j then
                           NthPath (0, false)
                        else
                           raise (BadAddressPrefix (addr1, addr2))
                     else if j >= i then
                        NthPath (j - i - 1, flag2)
                     else
                        raise (BadAddressPrefix (addr1, addr2))
   
                | Path path ->
                     (*
                      * Check prefix of addr2 is a cdr path
                      * and remove a head for each component of the path.
                      *)
                     let rec aux i' path' =
                        if i' = 0 then
                           if flag1 then
                              match path' with
                                 0::path'' -> Path path''
                               | _ -> raise (BadAddressPrefix (addr1, addr2))
                           else
                              Path path'
                        else
                           match path' with
                              1::path'' -> aux (i' - 1) path''
                            | _ -> raise (BadAddressPrefix (addr1, addr2))
                     in
                        aux i path
            end
   
       | Path path1 ->
            begin
               match addr2 with
                  NthPath (j, flag) ->
                     (* addr1 must be a cdr path *)
                     let rec aux path' j' =
                        match path' with
                           1::path'' -> aux path'' (j' - 1)
                         | [] -> NthPath (j', flag)
                         | _ -> raise (BadAddressPrefix (addr1, addr2))
                     in
                        aux path1 j
                | Path path2 ->
                     let rec aux path1' path2' =
                        match path1' with
                           x::path1'' ->
                              begin
                                 match path2' with
                                    y::path2'' when x = y -> aux path1'' path2''
                                  | _ -> raise (BadAddressPrefix (addr1, addr2))
                              end
                         | [] ->
                              Path path2'
                     in
                        aux path1 path2
            end
   
   (*
    * Print a string.
    *)
   let string_of_address = function
      Path addr ->
         let rec aux = function
            [] -> ""
          | [h] -> string_of_int h
          | h::t -> (string_of_int h) ^ "; " ^ (aux t)
         in
            "[" ^ (aux addr) ^ "]"
    | NthPath (addr, flag) ->
         let rec aux = function
            0 -> if flag then "1" else "0"
          | i -> "2; " ^ (aux i)
         in
            "[" ^ (aux addr) ^ "]"

end
