(*
 * This file just exists to extract the code for adding and infix
 * expression.
 *)

open Pcaml;;

(*
 * Make an infix expression.
 *)
let make_infix loc op e1 e2 =
  let lop = "prefix_" ^ op in
  MLast.ExApp (loc, MLast.ExApp (loc, MLast.ExLid (loc, lop), e1), e2)
;;
      
(*
 * Add an infix keyword.
 * This is computed in infix.ml.
 *)
let add_infix (keyword : string) =
  Grammar.extend
    (let _ = (expr : 'expr Grammar.Entry.e) in
     [Grammar.Entry.obj (expr : 'expr Grammar.Entry.e),
      Some (Gramext.Before "top"),
      [None, None,
       [[Gramext.Sself; Gramext.Stoken (Token.P_TERM keyword);
         Gramext.Sself],
        Gramext.action
          (fun (e2 : 'expr) (op : string) (e1 : 'expr) (loc : int * int) ->
             (make_infix loc op e1 e2 : 'expr))]]])
;;

(*
 * $Log$
 * Revision 1.3  1998/05/29 14:53:05  jyh
 * Better Makefiles.
 *
 * Revision 1.1  1997/04/28 15:51:04  jyh
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
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
