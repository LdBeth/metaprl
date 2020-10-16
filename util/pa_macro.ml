(* camlp4r *)
(* $Id$ *)

(*
Added statements:

  At toplevel (structure item):

     DEFINE <uident>
     DEFINE <uident> = <expression>
     DEFINE <uident> (<parameters>) = <expression>
     IFDEF <uident> THEN <structure_items> (END | ENDIF)
     IFDEF <uident> THEN <structure_items> ELSE <structure_items> (END | ENDIF)
     IFNDEF <uident> THEN <structure_items> (END | ENDIF)
     IFNDEF <uident> THEN <structure_items> ELSE <structure_items> (END | ENDIF)
     INCLUDE <string>

  In expressions:

     IFDEF <uident> THEN <expression> [ ELSE <expression> ] (END | ENDIF)
     IFNDEF <uident> THEN <expression> [ ELSE <expression> ] (END | ENDIF)
     DEFINE <lident> = <expression> IN <expression>
     __FILE__
     __LOCATION__

  In patterns:

     IFDEF <uident> THEN <pattern> ELSE <pattern> (END | ENDIF)
     IFNDEF <uident> THEN <pattern> ELSE <pattern> (END | ENDIF)

  As Camlp4 options:

     -D<uident>                      define <uident>
     -U<uident>                      undefine it
     -I<dir>                         add <dir> to the search path for INCLUDE'd files

  After having used a DEFINE <uident> followed by "= <expression>", you
  can use it in expressions *and* in patterns. If the expression defining
  the macro cannot be used as a pattern, there is an error message if
  it is used in a pattern.

  You can also define a local macro in an expression usigng the DEFINE ... IN form.
  Note that local macros have lowercase names and can not take parameters.

  The toplevel statement INCLUDE <string> can be used to include a
  file containing macro definitions; note that files included in such
  a way can not have any non-macro toplevel items.  The included files
  are looked up in directories passed in via the -I option, falling
  back to the current directory.

  The expression __FILE__ returns the current compiled file name.
  The expression __LOCATION__ returns the current location of itself.

  If a macro is define to = NOTHING, and then used as an argument to a function,
  this will be equivalent to function taking one less argument. Similarly,
  passing NOTHING as an argument to a macro is equivalent to "erasing" the
  corresponding parameter from the macro body.

*)

#load "pa_extend.cmo";
#load "q_MLast.cmo";

open Pcaml;
open Printf;

type macro_value =
  [ MvExpr of list string and MLast.expr
  | MvType of list string and MLast.ctyp
  | MvNone ]
;

type item_or_def 'a =
  [ SdStr of 'a
  | SdDef of string and macro_value
  | SdUnd of string
  | SdList of list (item_or_def 'a)
  | SdInc of string ]
;

value rec list_remove x =
  fun
  [ [(y, _) :: l] when y = x -> l
  | [d :: l] -> [d :: list_remove x l]
  | [] -> [] ]
;

value defined =
  ref
    [("CAMLP5", MvNone); ("CAMLP5_4_02", MvNone); ("CAMLP5_5_00", MvNone);
     ("CAMLP5_6_00", MvNone); ("CAMLP5_6_02_1", MvNone);
     ("CAMLP5_6_09", MvNone)]
;

(*
 XXX[jyh]: fix this.

value oversion = do {
  let v = Bytes.of_string Pconfig.ocaml_version in
  for i = 0 to Bytes.length v - 1 do {
    match Bytes.get v i with
    [ '0'..'9' | 'a'..'z' | 'A'..'Z' -> ()
    | _ -> Bytes.set v i '_' ];
  };
  Bytes.to_string v
};

value oname =
  if Pconfig.ocaml_name = "ocaml" then []
  else [(string_uppercase Pconfig.ocaml_name, MvNone)]
;

value defined_version loc =
  let s = "OCAML_" in
  loop (List.rev defined.val) where rec loop =
    fun
    [ [(d, _) :: l] ->
        if String.length d > String.length s &&
           String.sub d 0 (String.length s) = s
        then d
        else loop l
    | [] -> Ploc.raise loc (Failure "no defined version") ]
;
*)

value is_defined i = List.mem_assoc i defined.val;

value _loc = Ploc.dummy;

value rec no_nothing =
  fun
    [ [] -> []
    | [<:expr< NOTHING >> :: tl] -> no_nothing tl
    | [hd :: tl] -> [hd :: no_nothing tl] ]
;

value rec no_nothingp =
  fun
    [ [] -> []
    | [<:patt< NOTHING >> :: tl] -> no_nothingp tl
    | [hd :: tl] -> [hd :: no_nothingp tl] ]
;

value print_defined () = do {
  let deflist =
    if Pcaml.strict_mode.val then [("STRICT", MvNone) :: defined.val]
    else defined.val
  in
  List.iter
    (fun (d, v) -> do {
       print_string d;
       match v with
       [ MvExpr _ _ -> print_string " = <expr>"
       | MvType _ _ -> print_string " = <type>"
       | MvNone -> () ];
       print_newline ()
     })
    deflist;
  if Sys.interactive.val then () else exit 0
};

value subst mloc env =
  let rec loop =
    fun
    [ <:expr< let $opt:rf$ $list:pel$ in $e$ >> ->
        let pel = List.map (fun (p, e) -> (p, loop e)) pel in
        <:expr< let $opt:rf$ $list:pel$ in $loop e$ >>
    | <:expr< if $e1$ then $e2$ else $e3$ >> ->
        <:expr< if $loop e1$ then $loop e2$ else $loop e3$ >>
    | <:expr< fun $args$ -> $e$ >> ->
        match loopp args with
        [ <:patt< NOTHING >> -> loop e
        | p -> <:expr< fun $p$ -> $loop e$ >> ]
    | <:expr< fun [ $list: peoel$ ] >> -> <:expr< fun [ $list: (List.map loop_peoel peoel)$ ] >>
    | <:expr< $e1$ $e2$ >> ->
        match loop e2 with
        [ <:expr< NOTHING >> -> loop e1
        | e2 -> <:expr< $loop e1$ $e2$ >> ]
    | <:expr< $lid:x$ >> | <:expr< $uid:x$ >> as e ->
        try List.assoc x env with [ Not_found -> e ]
    | <:expr< ($list:x$) >> -> <:expr< ($list:no_nothing (List.map loop x)$) >>
    | <:expr< do {$list:x$} >> -> <:expr< do {$list:List.map loop x$} >>
    | <:expr< { $list:pel$ } >> ->
        let pel = List.map (fun (p, e) -> (loopp p, loop e)) pel in
        <:expr< { $list:pel$ } >>
    | <:expr< match $e$ with [ $list:peoel$ ] >> ->
        <:expr< match $loop e$ with [ $list: (List.map loop_peoel peoel)$ ] >>
    | <:expr< try $e$ with [ $list:peoel$ ] >> ->
        <:expr< try $loop e$ with [ $list: (List.map loop_peoel peoel)$ ] >>
    | e -> e ]
  and loop_peoel =
    fun
      [ (p, Ploc.VaVal (Some e1), e2) -> (loopp p, Ploc.VaVal (Some (loop e1)), loop e2)
      | (p, e, e2) -> (loopp p, e, loop e2) ]
  and loopp =
    fun
    [ <:patt< $p1$ $p2$ >> ->
        match loopp p2 with
          [ <:patt< NOTHING >> -> loopp p1
          | p2 -> <:patt< $loopp p1$ $p2$ >> ]
    | <:patt< $lid:x$ >> ->
        try to_patt (List.assoc x env) with
        [ Not_found -> <:patt< $lid:x$ >> ]
    | <:patt< $uid:x$ >> ->
        try to_patt (List.assoc x env) with
        [ Not_found -> <:patt< $uid:x$ >> ]
    | <:patt< ($list:x$) >> -> <:patt< ($list:no_nothingp (List.map loopp x)$) >>
    | <:patt< { $list:ppl$ } >> ->
        let ppl = List.map (fun (p1, p2) -> (p1, loopp p2)) ppl in
        <:patt< { $list:ppl$ } >>
    | p -> p ]
  and to_patt =
    fun
      [ <:expr< $e1$ $e2$ >> ->
         match to_patt e2 with
          [ <:patt< NOTHING >> -> to_patt e1
          | e2 -> <:patt< $to_patt e1$ $e2$ >> ]
      | <:expr< $lid:x$ >> -> <:patt< $lid:x$ >>
      | <:expr< $uid:x$ >> -> <:patt< $uid:x$ >>
      | <:expr< ($list:x$) >> -> <:patt< ($list:no_nothingp(List.map to_patt x)$) >>
      | _ -> raise Not_found (* Will be caught by loopp *) ]
  in loop
;

value substp mloc env =
  loop where rec loop =
    fun
    [ <:expr< $e1$ $e2$ >> ->
        match loop e2 with
          [ <:patt< NOTHING >> -> loop e1
          | p2 -> <:patt< $loop e1$ $p2$ >> ]
    | <:expr< $lid:x$ >> ->
        try List.assoc x env with
        [ Not_found -> <:patt< $lid:x$ >> ]
    | <:expr< $uid:x$ >> ->
        try List.assoc x env with
        [ Not_found -> <:patt< $uid:x$ >> ]
    | <:expr< $int:x$ >> -> <:patt< $int:x$ >>
    | <:expr< $str:s$ >> -> <:patt< $str:s$ >>
    | <:expr< ($list:x$) >> -> <:patt< ($list:List.map loop x$) >>
    | <:expr< { $list:pel$ } >> ->
        let ppl = List.map (fun (p, e) -> (p, loop e)) pel in
        <:patt< { $list:ppl$ } >>
    | _ ->
        Ploc.raise mloc
          (Failure
             "this macro cannot be used in a pattern (see its definition)") ]
;

value substt mloc env =
  loop where rec loop =
    fun
    [ <:ctyp< $t1$ -> $t2$ >> -> <:ctyp< $loop t1$ -> $loop t2$ >>
    | <:ctyp< $t1$ $t2$ >> -> <:ctyp< $loop t1$ $loop t2$ >>
    | <:ctyp< ($list:tl$) >> -> <:ctyp< ($list:List.map loop tl$) >>
    | <:ctyp< $lid:x$ >> | <:ctyp< $uid:x$ >> as t ->
        try List.assoc x env with [ Not_found -> t ]
    | t -> t ]
;

value cannot_eval e =
  let loc = MLast.loc_of_expr e in
  Ploc.raise loc (Stream.Error "can't eval")
;

value rec eval =
  fun
  [ <:expr< Char.chr $e$ >> ->
      match eval e with
      [ <:expr< $int:i$ >> ->
          let c = Char.escaped (Char.chr (int_of_string i)) in
         <:expr< $chr:c$ >>
      | e -> cannot_eval e ]
  | <:expr< Char.code $e$ >> ->
      match eval e with
      [ <:expr< $chr:c$ >> ->
          let i = string_of_int (Char.code (Plexing.eval_char c)) in
          <:expr< $int:i$ >>
      | e ->
          cannot_eval e ]
  | <:expr< $op$ $x$ $y$ >> ->
      let f = eval op in
      let x = eval x in
      let y = eval y in
      match (x, y) with
      [ (<:expr< $int:x$ >>, <:expr< $int:y$ >>) ->
          let x = int_of_string x in
          let y = int_of_string y in
          match f with
          [ <:expr< $lid:"+"$ >> -> <:expr< $int:string_of_int (x + y)$ >>
          | <:expr< $lid:"-"$ >> -> <:expr< $int:string_of_int (x - y)$ >>
          | <:expr< $lid:"lor"$ >> ->
              let s = sprintf "0o%o" (x lor y) in
              <:expr< $int:s$ >>
          | _ -> cannot_eval op ]
      | _ -> cannot_eval op ]
  | <:expr< $uid:x$ >> as e ->
      try
        match List.assoc x defined.val with
        [ _ -> e ]
      with
      [ Not_found -> e ]
  | <:expr< $chr:_$ >> | <:expr< $int:_$ >> | <:expr< $lid:_$ >> as e -> e
  | e -> cannot_eval e ]
;

value may_eval =
  fun
  [ <:expr< EVAL $e$ >> -> eval e
  | e -> e ]
;

value incorrect_number loc l1 l2 =
  Ploc.raise loc
    (Failure
       (Printf.sprintf "expected %d parameters; found %d"
          (List.length l2) (List.length l1)))
;

value define eo x = do {
  match eo with
  [ MvExpr [] e ->
      EXTEND
        expr: LEVEL "simple"
          [ [ UIDENT $x$ -> may_eval (Reloc.expr (fun _ -> _loc) 0 e) ] ]
        ;
        patt: LEVEL "simple"
          [ [ UIDENT $x$ ->
                let p = substp _loc [] e in
                Reloc.patt (fun _ -> _loc) 0 p ] ]
        ;
      END
  | MvExpr sl e ->
      EXTEND
        expr: LEVEL "apply"
          [ [ UIDENT $x$; param = SELF ->
                let el =
                  match param with
                  [ <:expr< ($list:el$) >> -> el
                  | e -> [e] ]
                in
                if List.length el = List.length sl then
                  let env = List.combine sl el in
                  let e = subst _loc env e in
                  may_eval (Reloc.expr (fun _ -> _loc) 0 e)
                else
                  incorrect_number _loc el sl ] ]
        ;
        patt: LEVEL "simple"
          [ [ UIDENT $x$; param = SELF ->
                let pl =
                  match param with
                  [ <:patt< ($list:pl$) >> -> pl
                  | p -> [p] ]
                in
                if List.length pl = List.length sl then
                  let e = may_eval (Reloc.expr (fun _ -> _loc) 0 e) in
                  let env = List.combine sl pl in
                  let p = substp _loc env e in
                  Reloc.patt (fun _ -> _loc) 0 p
                else
                  incorrect_number _loc pl sl ] ]
        ;
      END
  | MvType [] t ->
      EXTEND
        ctyp: LEVEL "simple"
          [ [ UIDENT $x$ -> t ] ]
        ;
      END
  | MvType sl t ->
      EXTEND
        ctyp: LEVEL "apply"
          [ [ UIDENT $x$; param = SELF ->
                let tl = [param] in
                if List.length tl = List.length sl then
                  let env = List.combine sl tl in
                  let t = substt _loc env t in
                  t
                else
                  incorrect_number _loc tl sl ] ]
        ;
      END
  | MvNone -> () ];
  defined.val := [(x, eo) :: defined.val]
};

value undef x =
  try do {
    let eo = List.assoc x defined.val in
    match eo with
    [ MvExpr [] _ -> do {
        DELETE_RULE expr: UIDENT $x$ END;
        DELETE_RULE patt: UIDENT $x$ END;
      }
    | MvExpr _ _ -> do {
        DELETE_RULE expr: UIDENT $x$; SELF END;
        DELETE_RULE patt: UIDENT $x$; SELF END;
      }
    | MvType [] _ -> do {
        DELETE_RULE ctyp: UIDENT $x$ END;
      }
    | MvType _ _ -> do {
        DELETE_RULE ctyp: UIDENT $x$; SELF END;
      }
    | MvNone -> () ];
    defined.val := list_remove x defined.val
  }
  with
  [ Not_found -> () ]
;

(* This is a list of directories to search for INCLUDE statements. *)
value include_dirs = ref []
;

(* Add something to the above, make sure it ends with a slash. *)
value add_include_dir str =
  if str <> "" then
    let str =
      if String.get str ((String.length str)-1) = '/'
      then str else str ^ "/"
    in include_dirs.val := include_dirs.val @ [str]
  else ()
;

value structure_or_macro = Grammar.Entry.create Pcaml.gram "structure_or_macro";
value signature_or_macro = Grammar.Entry.create Pcaml.gram "signature_or_macro";

value parse_include_file =
  let dir_ok file dir = Sys.file_exists (dir ^ file) in
  fun file ->
    let file =
      try (List.find (dir_ok file) (include_dirs.val @ ["./"])) ^ file
      with [ Not_found -> file ]
    in
    let ch = open_in file in
    let st = Stream.of_channel ch in
    let old_input = Pcaml.input_file.val in
    let (bol_ref, lnum_ref, name_ref) = (Plexing.bol_pos, Plexing.line_nb, Plexing.input_file) in
    let (old_bol, old_lnum, old_name) = (bol_ref.val, lnum_ref.val, name_ref.val) in
    let restore () =
      do {
        close_in ch;
        bol_ref.val := old_bol;
        lnum_ref.val := old_lnum;
        name_ref.val := old_name;
        Pcaml.input_file.val := old_input;
      }
    in
    do {
      bol_ref.val := ref 0;
      lnum_ref.val := ref 1;
      name_ref.val := file;
      Pcaml.input_file.val := file;
      try
        let items = Grammar.Entry.parse structure_or_macro st in
        do { restore (); items }
      with [ exn -> do { restore (); raise exn } ] }
;

value apply_directive loc n dp =
  let n = Pcaml.unvala n in
  match
    try Some (Pcaml.find_directive n) with
    [ Not_found -> None ]
  with
  [ Some f -> f (Pcaml.unvala dp)
  | None ->
      let msg = sprintf "unknown directive #%s" n in
      Ploc.raise loc (Stream.Error msg) ]
;

value rec str_execute_macro = fun
[ SdStr i -> [i] (* do {
    let sil = Pcaml.unvala sil in
    List.iter
      (fun
       [ MLast.StDir loc n dp -> apply_directive loc n dp
       | _ -> () ])
      sil;
    sil
  } *)
| SdDef x eo -> do { define eo x; [] }
| SdUnd x -> do { undef x; [] }
| SdList l -> str_execute_macro_list l
| SdInc f -> str_execute_macro_list (parse_include_file f) ]

and str_execute_macro_list = fun
[ [] -> []
| [hd::tl] -> (* The evaluation order is important here *)
  let il1 = str_execute_macro hd in
  let il2 = str_execute_macro_list tl in
    il1 @ il2 ]
;

value rec sig_execute_macro = fun
[ SdStr i  -> [i] (* do {
    let sil = Pcaml.unvala sil in
    List.iter
      (fun
       [ MLast.SgDir loc n dp -> apply_directive loc n dp
       | _ -> () ])
      sil;
    sil
  } *)
| SdDef x eo -> do { define eo x; [] }
| SdUnd x -> do { undef x; [] }
| SdList l -> sig_execute_macro_list l
| SdInc f -> raise (Invalid_argument "include is not supported in .mli files") ]

and sig_execute_macro_list = fun
[ [] -> []
| [hd::tl] -> (* The eveluation order is important here *)
  let il1 = sig_execute_macro hd in
  let il2 = sig_execute_macro_list tl in
    il1 @ il2 ]
;


EXTEND
  GLOBAL: expr patt str_item sig_item constructor_declaration match_case
    label_declaration structure_or_macro signature_or_macro;
  str_item: FIRST
    [ [ x = str_macro_def ->
         match str_execute_macro x with
         [ [si] -> si
         | sil -> <:str_item< declare $list:sil$ end >> ] ] ]
  ;
  sig_item: FIRST
    [ [ x = sig_macro_def ->
          match sig_execute_macro x with
            [ [si] -> si
            | sil -> <:sig_item< declare $list:sil$ end >> ] ] ]
  ;
  str_macro_def:
    [ [ "DEFINE"; i = uident; ome = opt_macro_expr -> SdDef i ome
      | "DEFINE_TYPE"; i = uident; ome = opt_macro_type -> SdDef i ome
      | "UNDEF"; i = uident -> SdUnd i
      | "IFDEF"; e = dexpr; "THEN"; d1 = structure_or_macro;
        d2 = else_str; "END" ->
          SdList (if e then d1 else d2)
      | "IFNDEF"; e = dexpr; "THEN"; d1 = structure_or_macro;
        d2 = else_str; "END" ->
          SdList (if not e then d1 else d2)
      | "INCLUDE"; fname = STRING -> SdInc fname ] ]
  ;
  else_str:
    [ [ "ELSIFDEF"; e = dexpr; "THEN"; d1 = structure_or_macro;
        d2 = else_str -> if e then d1 else d2
      | "ELSIFNDEF"; e = dexpr; "THEN"; d1 = structure_or_macro;
        d2 = else_str -> if not e then d1 else d2
      | "ELSE"; d1 = structure_or_macro -> d1
      | -> [] ] ]
  ;
  sig_macro_def:
    [ [ "DEFINE"; i = uident; omt = opt_macro_type -> SdDef i omt
      | "DEFINE_TYPE"; i = uident; omt = opt_macro_type -> SdDef i omt
      | "UNDEF"; i = uident -> SdUnd i
      | "IFDEF"; e = dexpr; "THEN"; d1 = signature_or_macro;
        d2 = else_sig; "END" ->
          SdList (if e then d1 else d2)
      | "IFNDEF"; e = dexpr; "THEN"; d1 = signature_or_macro;
        d2 = else_sig; "END" ->
          SdList (if not e then d1 else d2) ] ]
  ;
  else_sig:
    [ [ "ELSIFDEF"; e = dexpr; "THEN"; d1 = signature_or_macro;
        d2 = else_sig -> if e then d1 else d2
      | "ELSIFNDEF"; e = dexpr; "THEN"; d1 = signature_or_macro;
        d2 = else_sig -> if not e then d1 else d2
      | "ELSE"; d1 = signature_or_macro -> d1
      | -> [] ] ]
  ;
  structure_or_macro:
    [ [ sml = LIST1 str_item_or_macro -> sml ] ]
  ;
  str_item_or_macro:
    [ [ d = str_macro_def -> d
      | si = str_item -> SdStr si ] ]
  ;
  signature_or_macro:
    [ [ sml = LIST1 sig_item_or_macro -> sml ] ]
  ;
  sig_item_or_macro:
    [ [ d = sig_macro_def -> d
      | si = sig_item -> SdStr si ] ]
  ;
  opt_macro_expr:
    [ [ pl = macro_param; "="; e = expr -> MvExpr pl e
      | "="; e = expr -> MvExpr [] e
      | -> MvNone ] ]
  ;
  opt_macro_type:
    [ [ pl = LIST1 LIDENT; "="; t = ctyp -> MvType pl t
      | "="; t = ctyp -> MvType [] t
      | -> MvNone ] ]
  ;
  macro_param:
    [ [ sl = LIST1 LIDENT -> sl
      | "("; sl = LIST1 LIDENT SEP ","; ")" -> sl ] ]
  ;
  expr: LEVEL "top"
    [ [ "IFDEF"; e = dexpr; "THEN"; e1 = SELF; e2 = else_expr; "END" ->
          if e then e1 else e2
      | "IFNDEF"; e = dexpr; "THEN"; e1 = SELF; e2 = else_expr; "END" ->
          if not e then e1 else e2
     | "DEFINE"; i = LIDENT; "="; def = expr; "IN"; body = expr ->
          subst _loc [(i, def)] body ] ]
  ;
  else_expr:
    [ [ "ELSIFDEF"; e = dexpr; "THEN"; e1 = expr; e2 = else_expr ->
          if e then e1 else e2
      | "ELSIFNDEF"; e = dexpr; "THEN"; e1 = expr; e2 = else_expr ->
          if not e then e1 else e2
      | "ELSE"; e = expr -> e
      | -> <:expr< () >> ] ]
  ;
  expr: LEVEL "simple"
    [ [ LIDENT "__FILE__" -> <:expr< $str:Ploc.file_name _loc$ >>
      | LIDENT "__LOCATION__" ->
          let bp = string_of_int (Ploc.first_pos _loc) in
          let ep = string_of_int (Ploc.last_pos _loc) in
          <:expr< ($int:bp$, $int:ep$) >> ] ]
  ;
  patt:
    [ [ "IFDEF"; e = dexpr; "THEN"; p1 = SELF; p2 = else_patt; "END" ->
          if e then p1 else p2
      | "IFNDEF"; e = dexpr; "THEN"; p1 = SELF; p2 = else_patt; "END" ->
          if e then p2 else p1 ] ]
  ;
  else_patt:
    [ [ "ELSIFDEF"; e = dexpr; "THEN"; p1 = patt; p2 = else_patt ->
          if e then p1 else p2
      | "ELSIFNDEF"; e = dexpr; "THEN"; p1 = patt; p2 = else_patt ->
          if not e then p1 else p2
      | "ELSE"; p = patt -> p ] ]
  ;
  constructor_declaration: FIRST
    [ [ "IFDEF"; e = dexpr; "THEN"; x = SELF; "END" ->
          if e then x else Grammar.skip_item x
      | "IFDEF"; e = dexpr; "THEN"; x = SELF; "ELSE"; y = SELF; "END" ->
          if e then x else y
      | "IFNDEF"; e = dexpr; "THEN"; x = SELF; "END" ->
          if e then Grammar.skip_item x else x
      | "IFNDEF"; e = dexpr; "THEN"; x = SELF; "ELSE"; y = SELF; "END" ->
          if e then y else x ] ]
  ;
  label_declaration: FIRST
    [ [ "IFDEF"; e = dexpr; "THEN"; x = SELF; "END" ->
          if e then x else Grammar.skip_item x
      | "IFDEF"; e = dexpr; "THEN"; x = SELF; "ELSE"; y = SELF; "END" ->
          if e then x else y
      | "IFNDEF"; e = dexpr; "THEN"; x = SELF; "END" ->
          if e then Grammar.skip_item x else x
      | "IFNDEF"; e = dexpr; "THEN"; x = SELF; "ELSE"; y = SELF; "END" ->
          if e then y else x ] ]
  ;
  match_case: FIRST
    [ [ "IFDEF"; e = dexpr; "THEN"; x = SELF; y = else_match_case; "END" ->
          if e then x else y
      | "IFDEF"; e = dexpr; "THEN"; x = SELF; "END" ->
          if e then x else Grammar.skip_item x
      | "IFNDEF"; e = dexpr; "THEN"; x = SELF; y = else_match_case; "END" ->
          if not e then x else y
      | "IFNDEF"; e = dexpr; "THEN"; x = SELF; "END" ->
          if not e then x else Grammar.skip_item x ] ]
  ;
  else_match_case:
    [ RIGHTA
      [ "ELSIFDEF"; e = dexpr; "THEN"; x = match_case; y = else_match_case ->
          if e then x else y
      | "ELSIFDEF"; e = dexpr; "THEN"; x = match_case ->
          if e then x else Grammar.skip_item x
      | "ELSIFNDEF"; e = dexpr; "THEN"; x = match_case; y = else_match_case ->
          if not e then x else y
      | "ELSIFNDEF"; e = dexpr; "THEN"; x = match_case ->
          if not e then x else Grammar.skip_item x
      | "ELSE"; x = match_case -> x ] ]
  ;
  dexpr:
    [ [ x = SELF; "OR"; y = SELF -> x || y ]
    | [ x = SELF; "AND"; y = SELF -> x && y ]
    (* | [ "OCAML_VERSION"; f = op; y = uident -> f (defined_version _loc) y ] *)
    | [ "NOT"; x = SELF -> not x ]
    | [ i = uident -> is_defined i
      | "("; x = SELF; ")" -> x ] ]
  ;
(*
  op:
    [ [ "<=" -> \<=
      | "<" -> \<
      | "=" -> \=
      | "<>" -> \<>
      | ">" -> \>
      | ">=" -> \>= ] ]
  ;
*)
  uident:
    [ [ i = UIDENT -> i ] ]
  ;
END;


Pcaml.add_option "-D" (Arg.String (define MvNone))
  "<string> Define for IFDEF instruction.";

Pcaml.add_option "-U" (Arg.String undef)
  "<string> Undefine for IFDEF instruction.";

Pcaml.add_option "-I" (Arg.String add_include_dir)
  "<string> Add a directory to INCLUDE search path.";

Pcaml.add_option "-defined" (Arg.Unit print_defined)
  " Print the defined macros and exit.";
