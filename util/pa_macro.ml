(* camlp4r *)
(* $Id$ *)

(*
Added statements:

  At toplevel (structure item):

     DEFINE <uident>
     DEFINE <uident> = <expression>
     DEFINE <uident> (<parameters>) = <expression>
     IFDEF <uident> THEN <structure_items> ENDIF
     IFDEF <uident> THEN <structure_items> ELSE <structure_items> ENDIF
     IFNDEF <uident> THEN <structure_items> ENDIF
     IFNDEF <uident> THEN <structure_items> ELSE <structure_items> ENDIF
     INCLUDE <string>

  In expressions:

     IFDEF <uident> THEN <expression> [ ELSE <expression> ] ENDIF
     IFNDEF <uident> THEN <expression> [ ELSE <expression> ] ENDIF
     DEFINE <lident> = <expression> in <expression>
     __FILE__
     __LOCATION__

  In patterns:

     IFDEF <uident> THEN <pattern> ELSE <pattern> ENDIF
     IFNDEF <uident> THEN <pattern> ELSE <pattern> ENDIF

  As Camlp4 options:

     -D<uident>
     -U<uident>
     -I<dir>

  After having used a DEFINE <uident> followed by "= <expression>", you
  can use it in expressions *and* in patterns. If the expression defining
  the macro cannot be used as a pattern, there is an error message if
  it is used in a pattern.

  You can also define a local macro in an expression usigng the DEFINE ... IN form.
  Note that local macros have lowercase names and can not take parameters.

  INCLUDE can be used to include a file with a number of macro definitions;
  however it can not have any non-macro toplevel items. The INCLUDE files
  are looked up in directories passed in via the -I option, falling back to
  current directory.

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

type item_or_def 'a =
  [ SdStr of 'a
  | SdDef of string and option (list string * MLast.expr)
  | SdUnd of string
  | SdITE of string and list (item_or_def 'a) and list (item_or_def 'a)
  | SdInc of string ]
;

value rec list_remove x =
  fun
  [ [(y, _) :: l] when y = x -> l
  | [d :: l] -> [d :: list_remove x l]
  | [] -> [] ]
;

value defined = ref [];

value is_defined i = List.mem_assoc i defined.val;

value _loc =
   let nowhere =
      { (Lexing.dummy_pos) with Lexing.pos_lnum = 1; Lexing.pos_cnum = 0 }
   in
      (nowhere, nowhere);

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

value subst env =
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
      [ (p, Some e1, e2) -> (loopp p, Some (loop e1), loop e2)
      | (p, None, e2) -> (loopp p, None, loop e2) ]
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
    | <:expr< ($list:x$) >> -> <:patt< ($list:List.map loop x$) >>
    | <:expr< { $list:pel$ } >> ->
        let ppl = List.map (fun (p, e) -> (p, loop e)) pel in
        <:patt< { $list:ppl$ } >>
    | _ ->
        Stdpp.raise_with_loc mloc
          (Failure
             "this macro cannot be used in a pattern (see its definition)") ]
;

value incorrect_number loc l1 l2 =
  Stdpp.raise_with_loc loc
    (Failure
       (Printf.sprintf "expected %d parameters; found %d"
          (List.length l2) (List.length l1)))
;

value define eo x =
  do {
    match eo with
    [ Some ([], <:expr< NOTHING >>) ->
        EXTEND
          expr: LEVEL "apply"
            [ [ e = SELF; UIDENT $x$ -> e ] ]
          ;
          patt: LEVEL "simple"
            [ [ p = SELF; UIDENT $x$ -> p
              | UIDENT $x$; p = SELF -> p ] ]
          ;
        END
    | Some ([], e) ->
        EXTEND
          expr: LEVEL "simple"
            [ [ UIDENT $x$ -> Pcaml.expr_reloc (fun _ -> _loc) (fst _loc) e ] ]
          ;
          patt: LEVEL "simple"
            [ [ UIDENT $x$ ->
                  let p = substp _loc [] e in
                  Pcaml.patt_reloc (fun _ -> _loc) (fst _loc) p ] ]
          ;
        END
    | Some (sl, e) ->
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
                    let e = subst env e in
                    Pcaml.expr_reloc (fun _ -> _loc) (fst _loc) e
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
                    let env = List.combine sl pl in
                    let p = substp _loc env e in
                    Pcaml.patt_reloc (fun _ -> _loc) (fst _loc) p
                  else
                    incorrect_number _loc pl sl ] ]
          ;
        END
    | None -> () ];
    defined.val := [(x, eo) :: defined.val];
  }
;

value undef x =
  try
    do {
      let eo = List.assoc x defined.val in
      match eo with
      [ Some ([], <:expr< NOTHING >>) ->
          do {
            DELETE_RULE expr: SELF; UIDENT $x$ END;
            DELETE_RULE patt: SELF; UIDENT $x$ END;
            DELETE_RULE patt: UIDENT $x$; SELF END;
          }
      | Some ([], _) ->
          do {
            DELETE_RULE expr: UIDENT $x$ END;
            DELETE_RULE patt: UIDENT $x$ END;
          }
      | Some (_, _) ->
          do {
            DELETE_RULE expr: UIDENT $x$; SELF END;
            DELETE_RULE patt: UIDENT $x$; SELF END;
          }
      | None -> () ];
      defined.val := list_remove x defined.val;
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

value smlist = Grammar.Entry.create Pcaml.gram "smlist"
;

value parse_include_file =
  let dir_ok file dir = Sys.file_exists (dir ^ file) in
  fun file ->
    let file =
      try (List.find (dir_ok file) (include_dirs.val @ ["./"])) ^ file
      with [ Not_found -> file ]
    in
    let st = Stream.of_channel (open_in file) in
    let old_input = Pcaml.input_file.val in
    do {
      Pcaml.input_file.val := file;
      let items = Grammar.Entry.parse smlist st in
      do { Pcaml.input_file.val := old_input; items } }
;

value rec execute_macro = fun
[ SdStr i -> [i]
| SdDef x eo -> do { define eo x; [] }
| SdUnd x -> do { undef x; [] }
| SdITE i l1 l2 ->
   execute_macro_list (if is_defined i then l1 else l2)
| SdInc f -> execute_macro_list (parse_include_file f) ]

and execute_macro_list = fun
[ [] -> []
| [hd::tl] -> (* The eveluation order is important here *)
  let il1 = execute_macro hd in
  let il2 = execute_macro_list tl in
    il1 @ il2 ]
;

EXTEND
  GLOBAL: expr patt str_item sig_item smlist;
  str_item: FIRST
    [ [ x = macro_def ->
         match execute_macro x with
         [ [si] -> si
         | sil -> <:str_item< declare $list:sil$ end >> ] ] ]
  ;
  macro_def:
    [ [ "DEFINE"; i = uident; def = opt_macro_value -> SdDef i def
      | "UNDEF"; i = uident -> SdUnd i
      | "IFDEF"; i = uident; "THEN"; dl = smlist; "ENDIF" ->
          SdITE i dl []
      | "IFDEF"; i = uident; "THEN"; dl1 = smlist; "ELSE";
        dl2 = smlist; "ENDIF" ->
          SdITE i dl1 dl2
      | "IFNDEF"; i = uident; "THEN"; dl = smlist; "ENDIF" ->
          SdITE i [] dl
      | "IFNDEF"; i = uident; "THEN"; dl1 = smlist; "ELSE";
        dl2 = smlist; "ENDIF" ->
          SdITE i dl2 dl1
      | "INCLUDE"; file = STRING -> SdInc file ] ]
  ;
  smlist:
    [ [ sml = LIST1 str_item_or_macro -> sml ] ]
  ;
  str_item_or_macro:
    [ [ d = macro_def -> d
      | si = str_item -> SdStr si ] ]
  ;
  opt_macro_value:
    [ [ "("; pl = LIST1 LIDENT SEP ","; ")"; "="; e = expr -> Some (pl, e)
      | "="; e = expr -> Some ([], e)
      | -> None ] ]
  ;
  else_expr:
    [ [ "ELSE"; e = expr; "ENDIF" -> e
      | "ENDIF" -> <:expr< () >> ]]
  ;
  expr: LEVEL "top"
    [ [ "IFDEF"; i = uident; "THEN"; e1 = expr; e2 = else_expr ->
          if is_defined i then e1 else e2
      | "IFNDEF"; i = uident; "THEN"; e1 = expr; e2 = else_expr ->
          if is_defined i then e2 else e1
      | "DEFINE"; i = LIDENT; "="; def = expr; "IN"; body = expr ->
          subst [(i, def)] body
      ]]
  ;
  expr: LEVEL "simple"
    [ [ LIDENT "__FILE__" -> <:expr< $str:Pcaml.input_file.val$ >>
      | LIDENT "__LOCATION__" ->
          let bp = string_of_int ((fst _loc).Lexing.pos_cnum) in
          let ep = string_of_int ((snd _loc).Lexing.pos_cnum) in
          <:expr< ($int:bp$, $int:ep$) >> ] ]
  ;
  patt:
    [ [ "IFDEF"; i = uident; "THEN"; p1 = patt; "ELSE"; p2 = patt; "ENDIF" ->
          if is_defined i then p1 else p2
      | "IFNDEF"; i = uident; "THEN"; p1 = patt; "ELSE"; p2 = patt; "ENDIF" ->
          if is_defined i then p2 else p1 ] ]
  ;
  uident:
    [ [ i = UIDENT -> i ] ]
  ;
END;

Pcaml.add_option "-D" (Arg.String (define None))
  "<string> Define for IFDEF instruction."
;
Pcaml.add_option "-U" (Arg.String undef)
  "<string> Undefine for IFDEF instruction."
;
Pcaml.add_option "-I" (Arg.String add_include_dir)
  "<string> Add a directory to INCLUDE search path."
;
