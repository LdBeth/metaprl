(*
 * Addressed operations on terms.
 *)

open Printf
open Debug

open Term_std
open Term_op_std

module TermAddr =
struct
   open Term

   type term = Term.term

   (*
    * An address of a subterm is an int list describing the path.
    * In NthPath, the flag is true if the car should be taken.
    *)
   type address =
      Path of int list
    | NthPath of int * bool

   exception IncorrectAddress of address * term
   exception BadAddressPrefix of address * address

   (************************************************************************
    * Subterm addressing                                                   *
    ************************************************************************)

   (*
    * Constructor.
    *)
   let make_address l = Path l

   let make_seq_address i = NthPath (i + 1, false)

   let nth_cdr_addr i = NthPath (i, false)

   (*
    * Compute arities of subterms.
    *)
   let subterm_arities { term_terms = terms } =
      let aux { bvars = vars } = List.length vars in
         List.map aux terms

   (*
    * Get a subterm.
    *)
   let term_subterm term = function
      (Path addr) as a ->
         begin
            let rec aux t = function
               [] -> t
             | i::tl -> aux (List.nth t.term_terms i).bterm tl
            in
               try aux term addr with
                  Not_found -> raise (IncorrectAddress (a, term))
         end
    | (NthPath (addr, flag)) as a ->
         begin
            let rec aux t = function
               0 ->
                  if flag then
                     match t with
                        { term_terms = { bterm = h }::_ } ->
                           if !debug_rewrite then
                              eprintf "Term_addr_std.term_subterm: got subterm%t" eflush;
                           h
                      | _ ->
                           raise (IncorrectAddress (a, term))
                  else
                     begin
                        if !debug_rewrite then
                           eprintf "Term_addr_std.term_subterm: got subterm%t" eflush;
                        t
                     end
             | i ->
                  begin
                     if !debug_rewrite then
                        eprintf "Term_addr_std.term_subterm: %d%t" i eflush;
                     match t.term_terms with
                        [{ bterm = bterm }] ->
                           aux bterm (i - 1)
                      | _::{ bterm = bterm }::_ ->
                           aux bterm (i - 1)
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
                  [] ->
                     f t
                | i::tl ->
                     match t with
                        { term_op = op; term_terms = bterms } ->
                           { term_op = op;
                             term_terms =
                                let f { bvars = vars; bterm = term } =
                                   { bvars = vars; bterm = aux term tl }
                                in
                                   List_util.replacef_nth i f bterms
                           }
               in
                  try aux term addr with
                     Not_found
                   | Failure "replacef_nth" ->
                        raise (IncorrectAddress (a, term))
            end
       | NthPath (addr, flag) ->
            begin
               let rec aux t = function
                  0 ->
                     if flag then
                        match t with
                           { term_op = op;
                             term_terms = { bvars = vars; bterm = term }::bterms
                           } ->
                              { term_op = op;
                                term_terms = { bvars = vars; bterm = f term }::bterms
                              }
                         | _ ->
                              raise (IncorrectAddress (a, term))
                     else
                        f t
                | i ->
                     begin
                        match t with
                           { term_op = op; term_terms = [{ bvars = vars; bterm = term }] } ->
                              { term_op = op;
                                term_terms = [{ bvars = vars; bterm = aux term (i - 1) }]
                              }
                         | { term_op = op; term_terms = h::{ bvars = vars; bterm = term }::bterms } ->
                              { term_op = op;
                                term_terms = h::{ bvars = vars; bterm = aux term (i - 1) }::bterms
                              }
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

(*
 * $Log$
 * Revision 1.4  1998/06/01 19:53:49  jyh
 * Working addition proof.  Removing polymorphism from refiner(?)
 *
 * Revision 1.3  1998/06/01 13:55:28  jyh
 * Proving twice one is two.
 *
 * Revision 1.2  1998/05/30 19:18:46  nogin
 * Eliminated white space in empty lines.
 *
 * Revision 1.1  1998/05/28 15:02:22  jyh
 * Partitioned refiner into subdirectories.
 *
 * Revision 1.1  1998/05/27 15:14:20  jyh
 * Functorized the refiner over the Term module.
 *
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
