(*
 * This module is used to provide an association between
 * terms in an ML file, and their comments.
 *
 * The algorithm is best match.  We parse the comments from
 * the file, the iterate through all the terms in the
 * program.  The closest, largest program block after the comment
 * is associated with the comment through a table.
 * The comments can then be queried through the table.
 *)

open Printf
open Debug

open MLast
open MLast_util

(*
 * Show the file loading.
 *)
let _ =
   if !debug_load then
      eprintf "Loading Filter_comment%t" eflush


(************************************************************************
 * TYPES                                                                *
 ************************************************************************)

(*
 * The assocation is a hastable that maps MLast.loc to comments.
 *)
type t = (MLast.loc, int * string) Hashtbl.t

(************************************************************************
 * PARSING                                                              *
 ************************************************************************)

(*
 * Read the comments from the file.
 * No fancy stuff, just use stdio.
 *
 * raises Sys_error if file can't be opened.
 *)
let parse name =
   (*
    * Output:
    *   buf: buffer for collecting chars in a comment
    *   comments: list of all the comments.
    *   store: put another comment in the list.
    *)
   let buf = Buffer.create () in
   let comments = ref [] in
   let store loc s =
      comments := (loc, s) :: !comments
   in

   (*
    * Input:
    *    inx: the input file
    *    getc: get a char from the file (may raise End_of_file)
    *    index: the position of the next char getc will return.
    *)
   let inx = open_in name in
   let index = ref 0 in
   let getc () =
      let c = input_char inx in
         incr index;
         c
   in
   let rec read = function
      '(' ->
         maybe_comment1 (getc ())
    | _ ->
         read (getc ())
   and maybe_comment1 = function
      '*' ->
         (* This might be a comment or it may be Nuprl *)
         maybe_comment2 (getc ())
    | _ ->
         (* Wasn't a comment after all *)
         read (getc ())
   and maybe_comment2 = function
      '*' ->
         (* It is a comment, but it may also be a terminator *)
         Buffer.puts buf "(**";
         maybe_comment_end 1 (getc ())
    | ':' ->
         (* Nuprl comment leader is ignored *)
         read (getc ())
    | c ->
         (* It is certainly a comment *)
         Buffer.puts buf "(*";
         Buffer.putc buf c;
         comment 0 (getc ())
   and comment level c =
      Buffer.putc buf c;
      match c with
         '*' ->
            maybe_comment_end level (getc ())
       | '(' ->
            maybe_nested_comment level (getc ())
       | _ ->
            comment level (getc ())
   and maybe_nested_comment level c =
      Buffer.putc buf c;
      match c with
         '*' ->
            (* Comment is nested *)
            comment (level + 1) (getc ())
       | _ ->
            comment level (getc ())
   and maybe_comment_end level c =
      Buffer.putc buf c;
      match c with
         ')' ->
            if level = 1 then
               (* Comment is terminated *)
               let s = Buffer.gets buf in
                  store (!index - (String.length s)) s;
                  Buffer.clear buf;
                  read (getc ())
            else
               comment (level - 1) (getc ())
       | _ ->
            comment level (getc ())
   in

   (* Execution will always produce End_of_file *)
   let _ =
      try read (getc ()) with
         End_of_file -> ()
   in
      close_in inx;
      List.rev !comments

(************************************************************************
 * ASSOCIATIONS                                                         *
 ************************************************************************)

(*
 * Program interators.
 *)
let fold_expr locs expr =
   loc_of_expr expr :: locs

let fold_patt locs patt =
   loc_of_patt patt :: locs

let fold_type locs ctyp =
   loc_of_ctyp ctyp :: locs

let fold_sig_item locs item =
   loc_of_sig_item item :: locs

let fold_str_item locs item =
   loc_of_str_item item :: locs

let fold_module_expr locs me =
   loc_of_module_expr me :: locs

let fold_module_type locs mt =
   loc_of_module_type mt :: locs

let fold_with_constr locs = function
   WcTyp (loc, _, _, _) ->
      loc :: locs
 | WcMod (loc, _, _) ->
      loc :: locs

let fold_class_decl locs { cdLoc = loc } =
   loc :: locs

let fold_class_field locs field =
   let loc =
      match field with
         CfCtr (loc, _, _) -> loc
       | CfInh (loc, _, _, _) -> loc
       | CfMth (loc, _, _) -> loc
       | CfVal (loc, _, _, _, _) -> loc
       | CfVir (loc, _, _) -> loc
   in
      loc :: locs

let fold_class_type locs { ctLoc = loc } =
   loc :: locs

let fold_class_type_field locs field =
   let loc =
      match field with
         CtCtr (loc, _, _) -> loc
       | CtInh (loc, _) -> loc
       | CtMth (loc, _, _) -> loc
       | CtVal (loc, _, _, _, _) -> loc
       | CtVir (loc, _, _) -> loc
   in
      loc :: locs

(*
 * Fold record.
 *)
let folder =
   { fold_expr = fold_expr;
     fold_patt = fold_patt;
     fold_type = fold_type;
     fold_sig_item = fold_sig_item;
     fold_str_item = fold_str_item;
     fold_module_expr = fold_module_expr;
     fold_module_type = fold_module_type;
     fold_with_constr = fold_with_constr;
     fold_class = fold_class_decl;
     fold_class_field = fold_class_field;
     fold_class_type = fold_class_type;
     fold_class_type_field = fold_class_type_field
   }

(*
 * Create an association.
 * Each comment gets associated with the closest,
 * largest block following the comment, except for the final
 * comment, which is associated with the last block.
 *
 * We assume the comments are sorted by location.
 * We have to sort the locations.
 *)
let create comments locs =
   let table = Hashtbl.create 97 in
   let rec insert locs = function
      comment :: comments ->
         let start, contents = comment in
         let rec search locs =
            match locs with
               [loc] ->
                  Hashtbl.add table loc comment;
                  insert locs comments
             | loc :: locs' ->
                  let start', _ = loc in
                     if start' >= start then
                        begin
                           (* This is the closest term *)
                           Hashtbl.add table loc comment;
                           insert locs comments
                        end
                     else
                        search locs'
             | [] ->
               (* No associations are possible *)
                  ()
         in
            search locs
    | [] ->
         ()
   in

   (* Sort the list by starting position, longest length going first *)
   let compare (start1, len1) (start2, len2) =
      (start1 < start2) || (start1 = start2 && len1 < len2)
   in
   let locs = Sort.list compare locs in
      insert (Sort.list compare locs) comments;
      table

(*
 * Type specific creators.
 *)
let create_sig comments interf =
   create comments (List.fold_left (MLast_util.fold_sig_item folder) [] interf)

let create_str comments implem =
   create comments (List.fold_left (MLast_util.fold_str_item folder) [] implem)

(*
 * Get an association from the table.
 * The location has to match exactly.
 *)
let get = Hashtbl.find

(*
 * $Log$
 * Revision 1.5  1998/06/01 13:52:53  jyh
 * Proving twice one is two.
 *
 * Revision 1.4  1998/04/24 19:38:19  jyh
 * Updated debugging.
 *
 * Revision 1.3  1998/04/24 02:41:49  jyh
 * Added more extensive debugging capabilities.
 *
 * Revision 1.2  1998/04/06 19:50:37  jyh
 * Fixed match error in mLast_util.ml
 *
 * Revision 1.1  1998/02/19 17:13:56  jyh
 * Splitting filter_parse.
 *
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
