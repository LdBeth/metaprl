(*
 * Module for printing terms to an ML module.
 *
 *)

open Printf

open Debug
open Opname
open Term
open Term_util
open Ml_format_sig
open Ml_format
open Ml_print_sig

(*
 * Show the file loading.
 *)
let _ =
   if !debug_load then
      eprintf "Loading ML_print%t" eflush

module MakePrinter (File : FileSig) =
struct
   (************************************************************************
    * ML PRINTING                                                          *
    ************************************************************************)
   
   (*
    * Print an expression as a string.
    *)
   let print_ml_expr ofile expr =
      let rec print_var = function
         [h] ->
            File.put ofile h;
       | h::t ->
            File.put ofile h;
            File.put ofile ".";
            print_var t
       | [] ->
            raise (Invalid_argument "print_var")
      in
      let rec print = function
         ML_Var v ->
            File.put ofile (sprintf " %s" v)
       | ML_Int i ->
            File.put ofile (sprintf " %d" i)
       | ML_Num n ->
            File.put ofile
                     ("(Num.num_of_string \"" ^ (Num.string_of_num n) ^ "\")")
       | ML_String s ->
            File.put ofile (sprintf " \"%s\"" (String.escaped s))
       | ML_List l ->
            File.put ofile " [";
            print_list ";" l;
            File.put ofile "]"
       | ML_Let (v, expr, body) ->
            File.put ofile (sprintf "\nlet %s =" v);
            print expr;
            File.put ofile " in ";
            print body
       | ML_Apply l ->
            File.put ofile " (";
            print_list " " l;
            File.put ofile ")"
       | ML_Tuple l ->
            File.put ofile " (";
            print_list ", " l;
            File.put ofile ")"
       | ML_Record l ->
            File.put ofile " {";
            print_record l;
            File.put ofile "}"
       | ML_Module_Var l ->
            File.put ofile " ";
            print_var l
      and print_list sep = function
         [h] ->
            print h
       | h::t ->
            print h;
            File.put ofile sep;
            print_list sep t
       | [] ->
            ()
      and print_record = function
         [name, expr] ->
            print name;
            File.put ofile " =";
            print expr
       | (name, expr)::t ->
            print name;
            File.put ofile " =";
            print expr;
            File.put ofile "; ";
            print_record t
       | [] ->
            ()
      in
         print expr
         
   (************************************************************************
    * TYPES                                                                *
    ************************************************************************)
   
   (* Use the file types *)
   type t = File.t
   type out = File.out

   (************************************************************************
    * FILE INTERFACE                                                       *
    ************************************************************************)
   
   (*
    * Interface.
    *)
   let print_named_term ofile name term =
      (* Interface *)
      File.puti ofile (sprintf "val %s : term\n" name);

      (* Definition *)
      let expr = FormatTerm.format_term term in
         File.put ofile (sprintf "let %s =\n" name);
         print_ml_expr ofile expr

   (*
    * Interface.
    *)
   let print_term ofile term =
      print_ml_expr ofile (FormatTerm.format_term term)
   
   (*
    * Meta terms.
    *)
   let print_mterm ofile mterm =
      (* Print output *)
      print_ml_expr ofile (FormatTerm.format_mterm mterm)
   
   (*
    * List of terms.
    *)
   let print_term_list ofile l =
      print_ml_expr ofile (FormatTerm.format_term_list l)
end
   
(*
 * $Log$
 * Revision 1.3  1998/04/24 02:42:41  jyh
 * Added more extensive debugging capabilities.
 *
 * Revision 1.2  1998/03/20 22:16:18  eli
 * Eli: Changed integer parameters to Num.num's.
 *
 * Revision 1.1  1997/04/28 15:51:25  jyh
 * This is the initial checkin of Nuprl-Light.
 * I am porting the editor, so it is not included
 * in this checkin.
 *
 * Directories:
 *     refiner: logic engine
 *     filter: front end to the Ocaml compiler
 *     editor: Emacs proof editor
 *     util: utilities
 *     mk: Makefile templates
 *
 * Revision 1.8  1996/05/21 02:13:56  jyh
 * This is a semi-working version before Wisconsin vacation.
 *
 * Revision 1.7  1996/04/07 18:24:48  jyh
 * This is an intermediate commit while adjusting the dforms.
 * We intend that dform printers just return a list of terms.
 *
 * Revision 1.6  1996/03/25 20:50:41  jyh
 * Intermediate commit while modifying grammer.  Restricting
 * ML hooks to condition terms.
 *
 * Revision 1.5  1996/03/05 19:48:30  jyh
 * Preliminary version with logical framework.
 *
 * Revision 1.4  1996/02/25 15:16:15  jyh
 * This is a partial checkin as filterModule is being developed.
 * After the meta-logical framework is developed, sequent.* will go away.
 *
 * Revision 1.3  1996/02/18 23:32:28  jyh
 * Changin Format module to more Nuprl-like format.
 *
 * Revision 1.2  1996/02/13 21:32:22  jyh
 * This is an intermediate checkin while matching is being added to the rewriter.
 *
 * Revision 1.1  1996/02/10 20:19:54  jyh
 * Initial checkin of filter (prlcomp).
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner.run"
 * End:
 * -*-
 *)
