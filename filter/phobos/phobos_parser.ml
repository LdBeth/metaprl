type token =
    TokEof
  | TokAt of (Phobos_type.pos)
  | TokEq of (Phobos_type.pos)
  | TokRuleEq of (Phobos_type.pos)
  | TokArrow of (Phobos_type.pos)
  | TokDoubledArrow of (Phobos_type.pos)
  | TokPipe of (Phobos_type.pos)
  | TokSemi of (Phobos_type.pos)
  | TokColon of (Phobos_type.pos)
  | TokComma of (Phobos_type.pos)
  | TokLeftBrace of (Phobos_type.pos)
  | TokRightBrace of (Phobos_type.pos)
  | TokLeftBrack of (Phobos_type.pos)
  | TokRightBrack of (Phobos_type.pos)
  | TokIgnore of (Phobos_type.pos)
  | TokBang of (Phobos_type.pos)
  | TokDot of (Phobos_type.pos)
  | TokQuestionMark of (Phobos_type.pos)
  | TokLe of (Phobos_type.pos)
  | TokGe of (Phobos_type.pos)
  | TokStart of (Phobos_type.pos)
  | TokLongest of (Phobos_type.pos)
  | TokFirst of (Phobos_type.pos)
  | TokExtend of (Phobos_type.pos)
  | TokRemove of (Phobos_type.pos)
  | TokOverride of (Phobos_type.pos)
  | TokNonAssoc of (Phobos_type.pos)
  | TokLeftAssoc of (Phobos_type.pos)
  | TokRightAssoc of (Phobos_type.pos)
  | TokPrec of (Phobos_type.pos)
  | TokModule of (Phobos_type.pos)
  | TokInclude of (Phobos_type.pos)
  | TokTerms of (Phobos_type.pos)
  | TokTokens of (Phobos_type.pos)
  | TokGrammar of (Phobos_type.pos)
  | TokDeclare of (Phobos_type.pos)
  | TokRewrites of (Phobos_type.pos)
  | TokInline of (Phobos_type.pos)
  | TokOption of (string * Phobos_type.pos)
  | TokString of (string * Phobos_type.pos)
  | TokInt of (int * Phobos_type.pos)
  | TokFloat of (float * Phobos_type.pos)
  | TokId of (string * Phobos_type.pos)
  | TokQuotedId of (string * Phobos_type.pos)

open Parsing
# 26 "phobos_parser.mly"
open Phobos_debug
open Opname
open Mp_num
open Refiner.Refiner.TermType
open Refiner.Refiner.Term
open Phobos_type
open Phobos_constants
open Phobos_marshal
open Phobos_parse_state
open Phobos_exn
open Phobos_util
open Phobos_rewrite
open Phobos_builtin

(*
 * This is the name of the module.
 * It is only used in the opname of a product term.
 *)
let module_name = ref ""

(*
 * There are terms that are private to a module.
 * If they are ever show up outside of the parser,
 * something is wrong.
 *)
let privates_module = "@"

(*
 * Support for module names.
 *)
let term_names = ref StringTable.empty

(*
 * Add each built-in term to the private list.
 *)
let _ =
   List.iter (fun s ->
      term_names := StringTable.add !term_names s (privates_module, -1)) (snd (List.hd built_in_terms))

let new_terms mdl decls =
   List.iter (fun ((id, pos), subterm_count) ->
      if StringTable.mem !term_names id then begin
         let mod_name, stc = StringTable.find !term_names id in
         print_warning pos (string_format "%s exists as %s.%s{%d}" id mod_name id stc)
      end;
      debug_string (string_format "Adding term [%s]\n" id);
      term_names := StringTable.add !term_names id (mdl, subterm_count)) decls

let term_name_of (id, pos) =
   try
      let mod_name, subterm_count = StringTable.find !term_names id in
         if mod_name = privates_module then
            [id]
         else
            [id; mod_name]
   with
      Not_found ->
         raise (PhobosException (pos, Printf.sprintf "undefined term [%s]" id))

type param_type =
   TyString
 | TyNum
 | TyToken
 | TyLevel
 | TyVar

let simplify_module_name = function
   [(id, pos); ("", _)] ->
      [(id, pos)]
 | any ->
      any

let pho_make_term id_pos_list params bterms =
   let opname =
      if List.length id_pos_list > 1 then
         List.map fst (simplify_module_name id_pos_list)
      else
         term_name_of (List.hd id_pos_list)
   in
      mk_term (mk_op (make_opname opname) params) bterms

let pho_make_var_term (id, pos) =
   mk_var_term id

let pho_make_so_var_term (id, pos) terms =
   let terms = List.map fst terms in
      mk_so_var_term id terms

let pho_make_bterm id_pos_list pterm =
   let vars = List.map fst id_pos_list in
      mk_bterm vars (fst pterm)

let pho_make_unique_var_term = unique_var_term
let pho_make_token_term = token_term
let pho_make_prod_term = prod_term

let pho_make_number_term num =
   let param = make_param (Number (num_of_int (fst num))) in
      mk_term (mk_op (make_opname ["number"; "Itt_int_base"]) [param]) []

let rec make_rules (id, pos) = function
   head :: rest ->
      ((id, pos), head) :: make_rules (id, pos) rest
 | [] ->
      []

let make_rules head productions =
   List.rev (make_rules head productions)

let production_of_shorthand id_list opt_prec result =
   let from =
      List.map (fun id ->
         pho_make_unique_var_term id, snd id) id_list
   in
      id_list, opt_prec, [(from, result)]

let production_of_alt_syntax froms opt_prec to_term =
   let from_terms =
      List.map (fun (id, opt_term) ->
         match opt_term with
            Some (term, pos) ->
               term, pos
          | None ->
               pho_make_unique_var_term id, snd id) froms
   in
   let symbols = List.map (fun (id, _) -> id) froms in
      symbols, opt_prec, [(from_terms, to_term)]

let insert_rewrite_if_needed id_list = function
   [] ->
      let from_terms =
         List.map (fun (s, pos) ->
            pho_make_unique_var_term (s, pos)) id_list
      in
      let from = List.map2 (fun term (s, pos) -> term, pos) from_terms id_list in
      (match id_list with
         [s, pos] ->
               [from, List.hd from]
       | _ ->
            (* We use a bogus position for the result *)
            let result_pos = ("<prod>", 0, 0, 0, 0) in
            let result = pho_make_prod_term from_terms, result_pos in
               [from, result])
 | a ->
      a

let process_includes paths includes =
   List.iter (fun s ->
      if !debug_phobos then
         Format.print_string (Printf.sprintf "Loading %s..." s);
      let gst, _, _, _ = load_grammar (find_file paths s) in
      if !debug_phobos then
         Format.print_string "done\n";
      let termsets = gst.grammar_termsets in
         List.iter (fun term_option_list ->
            List.iter (fun term_option ->
               match term_option with
                  Term_extend (mdl, decls) ->
                     new_terms mdl decls) term_option_list) termsets) includes;
      if !debug_phobos then
         Format.print_string "\nDone processing includes\n"

let include_built_in (s, pos) =
   let new_terms, found =
      List.fold_left (fun (new_terms, found) (mdl, terms) ->
         if s = mdl then
            new_terms @ terms, true
         else
            new_terms, found) ([], false) built_in_terms
   in
   if not found then
      raise (PhobosException (pos, Printf.sprintf (**)
         "%s: No such module" s))
   else
      List.iter (fun term ->
         if !debug_phobos then
            print_string (Printf.sprintf "Built-in %s.%s\n" s term);
         term_names := StringTable.add !term_names term (s, -1)) new_terms

(* Line 183, file phobos_parser.ml *)
let yytransl_const = [|
  257 (* TokEof *);
    0|]

let yytransl_block = [|
  258 (* TokAt *);
  259 (* TokEq *);
  260 (* TokRuleEq *);
  261 (* TokArrow *);
  262 (* TokDoubledArrow *);
  263 (* TokPipe *);
  264 (* TokSemi *);
  265 (* TokColon *);
  266 (* TokComma *);
  267 (* TokLeftBrace *);
  268 (* TokRightBrace *);
  269 (* TokLeftBrack *);
  270 (* TokRightBrack *);
  271 (* TokIgnore *);
  272 (* TokBang *);
  273 (* TokDot *);
  274 (* TokQuestionMark *);
  275 (* TokLe *);
  276 (* TokGe *);
  277 (* TokStart *);
  278 (* TokLongest *);
  279 (* TokFirst *);
  280 (* TokExtend *);
  281 (* TokRemove *);
  282 (* TokOverride *);
  283 (* TokNonAssoc *);
  284 (* TokLeftAssoc *);
  285 (* TokRightAssoc *);
  286 (* TokPrec *);
  287 (* TokModule *);
  288 (* TokInclude *);
  289 (* TokTerms *);
  290 (* TokTokens *);
  291 (* TokGrammar *);
  292 (* TokDeclare *);
  293 (* TokRewrites *);
  294 (* TokInline *);
  295 (* TokOption *);
  296 (* TokString *);
  297 (* TokInt *);
  298 (* TokFloat *);
  299 (* TokId *);
  300 (* TokQuotedId *);
    0|]

let yylhs = "\255\255\
\001\000\009\000\010\000\010\000\018\000\018\000\019\000\019\000\
\020\000\021\000\021\000\011\000\011\000\022\000\022\000\023\000\
\024\000\025\000\025\000\026\000\026\000\012\000\012\000\028\000\
\013\000\013\000\030\000\030\000\032\000\032\000\033\000\033\000\
\033\000\031\000\034\000\034\000\035\000\035\000\036\000\038\000\
\038\000\039\000\040\000\040\000\041\000\041\000\041\000\041\000\
\043\000\044\000\044\000\045\000\046\000\046\000\042\000\047\000\
\047\000\017\000\014\000\014\000\048\000\048\000\049\000\049\000\
\049\000\008\000\008\000\050\000\050\000\051\000\051\000\052\000\
\007\000\006\000\006\000\005\000\053\000\053\000\056\000\056\000\
\055\000\057\000\057\000\058\000\054\000\054\000\059\000\060\000\
\060\000\061\000\061\000\037\000\004\000\029\000\029\000\064\000\
\064\000\065\000\062\000\062\000\063\000\063\000\003\000\003\000\
\067\000\068\000\069\000\069\000\066\000\066\000\070\000\071\000\
\071\000\072\000\072\000\002\000\002\000\002\000\002\000\002\000\
\002\000\073\000\077\000\077\000\077\000\075\000\078\000\078\000\
\079\000\080\000\080\000\081\000\081\000\074\000\074\000\076\000\
\082\000\082\000\084\000\083\000\083\000\083\000\083\000\083\000\
\083\000\027\000\027\000\085\000\085\000\086\000\087\000\088\000\
\088\000\089\000\089\000\090\000\090\000\091\000\091\000\092\000\
\093\000\093\000\094\000\094\000\095\000\095\000\015\000\015\000\
\096\000\096\000\097\000\016\000\016\000\098\000\098\000\099\000\
\000\000"

let yylen = "\002\000\
\010\000\002\000\000\000\001\000\002\000\001\000\002\000\002\000\
\001\000\002\000\001\000\000\000\001\000\001\000\002\000\002\000\
\001\000\001\000\002\000\005\000\001\000\000\000\001\000\003\000\
\000\000\005\000\000\000\001\000\002\000\001\000\001\000\001\000\
\001\000\001\000\002\000\001\000\004\000\005\000\002\000\000\000\
\001\000\001\000\001\000\002\000\002\000\001\000\002\000\002\000\
\001\000\001\000\003\000\001\000\001\000\002\000\001\000\001\000\
\003\000\001\000\000\000\001\000\002\000\001\000\002\000\002\000\
\002\000\000\000\005\000\000\000\001\000\002\000\001\000\002\000\
\001\000\002\000\001\000\003\000\002\000\001\000\000\000\002\000\
\001\000\002\000\001\000\002\000\003\000\004\000\001\000\001\000\
\002\000\004\000\001\000\003\000\003\000\000\000\001\000\002\000\
\001\000\003\000\000\000\001\000\000\000\001\000\003\000\001\000\
\003\000\001\000\002\000\001\000\001\000\001\000\001\000\000\000\
\001\000\001\000\003\000\003\000\002\000\001\000\004\000\001\000\
\001\000\001\000\001\000\003\000\003\000\003\000\000\000\001\000\
\001\000\003\000\001\000\001\000\003\000\000\000\001\000\003\000\
\003\000\001\000\001\000\003\000\003\000\003\000\001\000\001\000\
\001\000\000\000\001\000\001\000\002\000\002\000\001\000\001\000\
\003\000\001\000\004\000\001\000\004\000\000\000\001\000\001\000\
\001\000\003\000\001\000\003\000\001\000\004\000\000\000\001\000\
\001\000\002\000\004\000\000\000\001\000\001\000\002\000\002\000\
\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\177\000\000\000\058\000\002\000\000\000\
\000\000\000\000\006\000\011\000\008\000\000\000\009\000\000\000\
\000\000\000\000\014\000\005\000\010\000\000\000\021\000\016\000\
\000\000\018\000\000\000\000\000\023\000\015\000\000\000\019\000\
\000\000\121\000\120\000\111\000\109\000\000\000\000\000\000\000\
\097\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\024\000\096\000\000\000\000\000\000\000\000\000\000\000\
\000\000\031\000\032\000\033\000\000\000\000\000\030\000\000\000\
\000\000\000\000\000\000\000\000\062\000\000\000\000\000\000\000\
\148\000\125\000\098\000\123\000\114\000\000\000\000\000\000\000\
\000\000\000\000\000\000\138\000\000\000\116\000\124\000\000\000\
\029\000\053\000\063\000\000\000\064\000\065\000\000\000\000\000\
\061\000\000\000\150\000\000\000\152\000\000\000\020\000\149\000\
\119\000\000\000\000\000\000\000\000\000\000\000\136\000\132\000\
\000\000\000\000\000\000\000\000\128\000\000\000\131\000\000\000\
\000\000\000\000\000\000\036\000\000\000\054\000\000\000\000\000\
\000\000\071\000\000\000\000\000\000\000\169\000\000\000\000\000\
\000\000\115\000\139\000\142\000\141\000\140\000\137\000\000\000\
\000\000\126\000\000\000\000\000\000\000\000\000\000\000\039\000\
\041\000\000\000\043\000\026\000\035\000\000\000\072\000\000\000\
\070\000\000\000\000\000\000\000\000\000\174\000\170\000\000\000\
\153\000\000\000\000\000\000\000\159\000\000\000\161\000\163\000\
\133\000\057\000\130\000\000\000\056\000\045\000\050\000\047\000\
\000\000\048\000\044\000\000\000\000\000\075\000\000\000\000\000\
\000\000\176\000\001\000\175\000\157\000\000\000\000\000\155\000\
\000\000\000\000\000\000\000\000\037\000\000\000\074\000\067\000\
\171\000\164\000\000\000\162\000\038\000\051\000\000\000\000\000\
\108\000\104\000\000\000\000\000\000\000\076\000\000\000\000\000\
\000\000\088\000\166\000\000\000\092\000\000\000\107\000\000\000\
\000\000\077\000\000\000\083\000\000\000\000\000\089\000\103\000\
\105\000\000\000\084\000\082\000\080\000\000\000\000\000\085\000\
\090\000\086\000\000\000\000\000\093\000"

let yydgoto = "\002\000\
\004\000\037\000\215\000\248\000\190\000\191\000\192\000\096\000\
\005\000\009\000\017\000\028\000\047\000\067\000\132\000\164\000\
\076\000\010\000\011\000\014\000\015\000\018\000\019\000\024\000\
\025\000\026\000\071\000\029\000\039\000\061\000\122\000\062\000\
\063\000\123\000\124\000\125\000\205\000\152\000\153\000\154\000\
\155\000\114\000\184\000\185\000\091\000\092\000\115\000\068\000\
\069\000\128\000\129\000\130\000\222\000\223\000\234\000\238\000\
\235\000\236\000\224\000\225\000\226\000\216\000\252\000\040\000\
\041\000\042\000\218\000\219\000\220\000\043\000\078\000\079\000\
\044\000\055\000\086\000\056\000\045\000\116\000\117\000\118\000\
\119\000\083\000\084\000\140\000\072\000\073\000\099\000\100\000\
\101\000\102\000\172\000\173\000\174\000\175\000\176\000\133\000\
\134\000\165\000\166\000"

let yysindex = "\019\000\
\027\255\000\000\028\255\000\000\094\255\000\000\000\000\242\254\
\073\255\094\255\000\000\000\000\000\000\088\255\000\000\245\254\
\120\255\073\255\000\000\000\000\000\000\103\255\000\000\000\000\
\245\254\000\000\029\255\133\255\000\000\000\000\163\255\000\000\
\171\255\000\000\000\000\000\000\000\000\000\000\182\255\029\255\
\000\000\184\255\179\255\194\255\192\255\055\255\232\255\188\255\
\028\255\000\000\000\000\029\255\029\255\159\255\202\255\000\000\
\028\255\000\000\000\000\000\000\207\255\055\255\000\000\028\255\
\028\255\028\255\191\255\232\255\000\000\028\255\216\255\188\255\
\000\000\000\000\000\000\000\000\000\000\215\255\222\255\224\255\
\225\255\229\255\111\255\000\000\029\255\000\000\000\000\251\254\
\000\000\000\000\000\000\028\255\000\000\000\000\218\255\214\255\
\000\000\250\255\000\000\254\255\000\000\008\000\000\000\000\000\
\000\000\029\255\238\255\238\255\244\255\159\255\000\000\000\000\
\000\000\248\255\025\000\026\000\000\000\034\000\000\000\001\000\
\246\255\031\000\251\254\000\000\042\000\000\000\028\255\035\000\
\218\255\000\000\037\000\013\000\214\255\000\000\238\255\028\255\
\248\254\000\000\000\000\000\000\000\000\000\000\000\000\029\255\
\028\255\000\000\029\255\049\000\028\255\014\000\028\255\000\000\
\000\000\246\255\000\000\000\000\000\000\015\000\000\000\010\000\
\000\000\029\255\029\255\055\000\013\000\000\000\000\000\043\000\
\000\000\041\000\046\000\048\000\000\000\053\000\000\000\000\000\
\000\000\000\000\000\000\022\000\000\000\000\000\000\000\000\000\
\054\000\000\000\000\000\052\000\061\000\000\000\010\000\056\000\
\057\000\000\000\000\000\000\000\000\000\023\000\023\000\000\000\
\248\254\052\000\030\000\029\255\000\000\028\255\000\000\000\000\
\000\000\000\000\058\000\000\000\000\000\000\000\059\000\062\000\
\000\000\000\000\066\000\029\255\060\000\000\000\068\000\047\000\
\028\255\000\000\000\000\029\255\000\000\029\255\000\000\029\255\
\028\255\000\000\068\000\000\000\033\000\077\255\000\000\000\000\
\000\000\063\000\000\000\000\000\000\000\029\255\029\255\000\000\
\000\000\000\000\059\000\069\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\177\255\000\000\000\000\000\000\
\239\255\208\255\000\000\000\000\000\000\128\255\000\000\000\000\
\148\255\251\255\000\000\000\000\000\000\000\000\000\000\000\000\
\220\255\000\000\070\000\255\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\079\255\000\000\072\000\
\000\000\000\000\097\255\067\000\074\255\120\255\006\255\026\000\
\000\000\000\000\000\000\000\000\073\000\000\000\000\000\007\255\
\000\000\000\000\000\000\000\000\000\000\074\000\000\000\000\000\
\000\000\000\000\002\255\022\255\000\000\000\000\000\000\076\000\
\000\000\000\000\000\000\000\000\000\000\000\000\075\000\170\255\
\206\255\213\255\000\000\000\000\078\000\000\000\000\000\000\000\
\000\000\000\000\000\000\012\000\000\000\000\000\080\000\015\255\
\000\000\064\255\000\000\044\255\000\000\091\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\180\255\000\000\147\255\000\000\000\000\081\000\000\000\000\000\
\077\000\000\000\082\000\000\000\000\000\000\000\000\000\000\000\
\084\000\000\000\000\000\085\000\031\255\000\000\000\000\000\000\
\086\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\127\255\000\000\000\000\000\000\
\000\000\089\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\070\000\000\000\000\000\095\000\000\000\000\000\000\000\
\000\000\000\000\146\255\000\000\000\000\087\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\144\255\000\000\000\000\000\000\000\000\000\000\088\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\090\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\091\000\000\000\
\000\000\000\000\000\000\092\000\102\255\000\000\255\254\108\255\
\080\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\021\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\093\000\000\000\
\000\000\000\000\094\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\205\255\110\000\000\000\138\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\253\255\000\000\094\001\000\000\000\000\000\000\089\001\000\000\
\000\000\083\001\000\000\000\000\203\000\000\000\000\000\000\000\
\048\001\000\000\244\000\248\000\167\000\000\000\000\000\000\000\
\216\000\223\255\000\000\000\000\071\000\000\000\000\000\000\000\
\047\001\000\000\000\000\243\000\000\000\140\000\000\000\000\000\
\000\000\139\000\000\000\000\000\150\000\000\000\000\000\000\000\
\080\001\058\255\149\000\000\000\000\000\123\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\231\000\000\000\013\001\200\255\000\000\052\001\000\000\000\000\
\245\000\000\000\000\000\000\000\000\000\181\000\185\000\000\000\
\251\000\000\000\220\000"

let yytablesize = 385
let yytable = "\007\000\
\075\000\077\000\066\000\171\000\013\000\217\000\059\000\117\000\
\117\000\120\000\078\000\117\000\022\000\117\000\117\000\167\000\
\117\000\135\000\117\000\001\000\117\000\231\000\060\000\038\000\
\117\000\012\000\117\000\023\000\006\000\217\000\033\000\168\000\
\081\000\112\000\006\000\036\000\038\000\121\000\066\000\066\000\
\059\000\078\000\059\000\059\000\117\000\074\000\034\000\117\000\
\217\000\117\000\117\000\141\000\167\000\087\000\138\000\151\000\
\060\000\003\000\060\000\060\000\090\000\090\000\090\000\081\000\
\171\000\211\000\098\000\171\000\168\000\035\000\006\000\006\000\
\036\000\156\000\156\000\156\000\058\000\059\000\168\000\151\000\
\110\000\113\000\246\000\110\000\122\000\087\000\122\000\247\000\
\126\000\123\000\087\000\123\000\177\000\060\000\123\000\112\000\
\110\000\118\000\118\000\156\000\154\000\118\000\154\000\118\000\
\118\000\016\000\118\000\091\000\118\000\087\000\118\000\194\000\
\091\000\079\000\118\000\182\000\118\000\186\000\079\000\110\000\
\110\000\110\000\110\000\159\000\111\000\008\000\154\000\021\000\
\007\000\046\000\027\000\091\000\098\000\170\000\118\000\093\000\
\094\000\118\000\007\000\118\000\118\000\178\000\031\000\113\000\
\091\000\181\000\049\000\181\000\022\000\055\000\046\000\046\000\
\046\000\165\000\007\000\007\000\007\000\165\000\038\000\007\000\
\007\000\007\000\007\000\055\000\007\000\007\000\046\000\049\000\
\049\000\049\000\055\000\055\000\055\000\048\000\022\000\022\000\
\022\000\003\000\241\000\143\000\242\000\022\000\022\000\143\000\
\022\000\022\000\049\000\003\000\052\000\056\000\123\000\053\000\
\123\000\050\000\250\000\123\000\056\000\170\000\080\000\081\000\
\038\000\082\000\221\000\003\000\003\000\003\000\054\000\057\000\
\004\000\003\000\003\000\003\000\085\000\003\000\003\000\144\000\
\038\000\088\000\004\000\144\000\017\000\221\000\145\000\070\000\
\038\000\095\000\145\000\103\000\105\000\221\000\017\000\106\000\
\107\000\108\000\004\000\004\000\004\000\109\000\127\000\012\000\
\004\000\004\000\004\000\038\000\004\000\004\000\017\000\017\000\
\017\000\012\000\131\000\013\000\017\000\017\000\017\000\025\000\
\017\000\017\000\064\000\065\000\066\000\013\000\135\000\136\000\
\144\000\012\000\012\000\012\000\052\000\149\000\150\000\151\000\
\012\000\012\000\137\000\012\000\012\000\013\000\013\000\013\000\
\139\000\025\000\025\000\025\000\013\000\013\000\142\000\013\000\
\013\000\025\000\145\000\025\000\025\000\146\000\052\000\052\000\
\052\000\147\000\156\000\121\000\158\000\160\000\052\000\162\000\
\052\000\052\000\163\000\180\000\189\000\183\000\188\000\195\000\
\197\000\198\000\199\000\200\000\201\000\202\000\204\000\203\000\
\206\000\228\000\036\000\208\000\209\000\214\000\230\000\227\000\
\207\000\229\000\233\000\245\000\237\000\134\000\232\000\040\000\
\253\000\094\000\249\000\095\000\028\000\172\000\112\000\147\000\
\113\000\127\000\068\000\042\000\129\000\034\000\069\000\173\000\
\106\000\158\000\160\000\073\000\251\000\099\000\100\000\020\000\
\101\000\102\000\030\000\032\000\193\000\089\000\157\000\148\000\
\213\000\187\000\097\000\161\000\243\000\244\000\239\000\051\000\
\240\000\179\000\143\000\104\000\169\000\212\000\210\000\167\000\
\196\000"

let yycheck = "\003\000\
\052\000\053\000\001\001\137\000\008\000\204\000\001\001\001\001\
\002\001\015\001\012\001\005\001\024\001\007\001\008\001\001\001\
\010\001\011\001\012\001\001\000\014\001\220\000\001\001\027\000\
\018\001\040\001\020\001\039\001\043\001\228\000\002\001\001\001\
\012\001\085\000\043\001\044\001\040\000\043\001\037\001\038\001\
\035\001\043\001\037\001\038\001\038\001\049\000\018\001\041\001\
\247\000\043\001\044\001\108\000\038\001\057\000\106\000\012\001\
\035\001\031\001\037\001\038\001\064\000\065\000\066\000\043\001\
\198\000\199\000\070\000\201\000\038\001\041\001\043\001\043\001\
\044\001\010\001\011\001\012\001\022\001\023\001\135\000\036\001\
\002\001\085\000\006\001\005\001\011\001\006\001\013\001\011\001\
\092\000\011\001\011\001\013\001\144\000\039\001\016\001\147\000\
\018\001\001\001\002\001\036\001\010\001\005\001\012\001\007\001\
\008\001\033\001\010\001\006\001\012\001\030\001\014\001\163\000\
\011\001\006\001\018\001\149\000\020\001\151\000\011\001\041\001\
\010\001\043\001\044\001\127\000\014\001\032\001\036\001\040\001\
\001\001\003\001\011\001\030\001\136\000\137\000\038\001\065\000\
\066\000\041\001\011\001\043\001\044\001\145\000\040\001\147\000\
\043\001\149\000\003\001\151\000\001\001\003\001\024\001\025\001\
\026\001\008\001\027\001\028\001\029\001\012\001\162\000\032\001\
\033\001\034\001\035\001\017\001\037\001\038\001\034\001\024\001\
\025\001\026\001\024\001\025\001\026\001\011\001\027\001\028\001\
\029\001\001\001\230\000\010\001\232\000\034\001\035\001\014\001\
\037\001\038\001\016\001\011\001\005\001\010\001\011\001\013\001\
\013\001\012\001\246\000\016\001\017\001\201\000\040\001\041\001\
\204\000\043\001\206\000\027\001\028\001\029\001\013\001\016\001\
\001\001\033\001\034\001\035\001\011\001\037\001\038\001\010\001\
\220\000\011\001\011\001\014\001\001\001\225\000\010\001\036\001\
\228\000\035\001\014\001\012\001\014\001\233\000\011\001\010\001\
\009\001\009\001\027\001\028\001\029\001\009\001\021\001\001\001\
\033\001\034\001\035\001\247\000\037\001\038\001\027\001\028\001\
\029\001\011\001\037\001\001\001\033\001\034\001\035\001\001\001\
\037\001\038\001\027\001\028\001\029\001\011\001\013\001\010\001\
\017\001\027\001\028\001\029\001\001\001\024\001\025\001\026\001\
\034\001\035\001\011\001\037\001\038\001\027\001\028\001\029\001\
\043\001\027\001\028\001\029\001\034\001\035\001\043\001\037\001\
\038\001\035\001\010\001\037\001\038\001\012\001\027\001\028\001\
\029\001\008\001\012\001\043\001\003\001\011\001\035\001\011\001\
\037\001\038\001\038\001\003\001\043\001\040\001\040\001\001\001\
\014\001\017\001\013\001\012\001\008\001\040\001\011\001\010\001\
\004\001\007\001\044\001\012\001\012\001\040\001\005\001\014\001\
\191\000\012\001\007\001\043\001\030\001\011\001\019\001\003\001\
\012\001\012\001\020\001\012\001\011\001\001\001\014\001\012\001\
\014\001\012\001\011\001\003\001\012\001\012\001\011\001\001\001\
\005\001\012\001\012\001\012\001\247\000\012\001\012\001\010\000\
\012\001\012\001\018\000\025\000\162\000\062\000\123\000\120\000\
\202\000\154\000\068\000\129\000\233\000\235\000\225\000\040\000\
\228\000\147\000\110\000\072\000\136\000\201\000\198\000\133\000\
\165\000"

let yynames_const = "\
  TokEof\000\
  "

let yynames_block = "\
  TokAt\000\
  TokEq\000\
  TokRuleEq\000\
  TokArrow\000\
  TokDoubledArrow\000\
  TokPipe\000\
  TokSemi\000\
  TokColon\000\
  TokComma\000\
  TokLeftBrace\000\
  TokRightBrace\000\
  TokLeftBrack\000\
  TokRightBrack\000\
  TokIgnore\000\
  TokBang\000\
  TokDot\000\
  TokQuestionMark\000\
  TokLe\000\
  TokGe\000\
  TokStart\000\
  TokLongest\000\
  TokFirst\000\
  TokExtend\000\
  TokRemove\000\
  TokOverride\000\
  TokNonAssoc\000\
  TokLeftAssoc\000\
  TokRightAssoc\000\
  TokPrec\000\
  TokModule\000\
  TokInclude\000\
  TokTerms\000\
  TokTokens\000\
  TokGrammar\000\
  TokDeclare\000\
  TokRewrites\000\
  TokInline\000\
  TokOption\000\
  TokString\000\
  TokInt\000\
  TokFloat\000\
  TokId\000\
  TokQuotedId\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun parser_env ->
    let _1 = (peek_val parser_env 9 : 'module_name) in
    let _2 = (peek_val parser_env 8 : 'includes) in
    let _3 = (peek_val parser_env 7 : 'term_sections) in
    let _4 = (peek_val parser_env 6 : 'opt_preamble) in
    let _5 = (peek_val parser_env 5 : 'lexer) in
    let _6 = (peek_val parser_env 4 : 'opt_assocs) in
    let _7 = (peek_val parser_env 3 : Phobos_type.pre_rule list * Phobos_type.goption list) in
    let _8 = (peek_val parser_env 2 : 'opt_rewrites_section) in
    let _9 = (peek_val parser_env 1 : 'opt_inline_forms) in
    Obj.repr((
# 271 "phobos_parser.mly"
                                { phobos_module_name = _1;
                                  phobos_includes = _2;
                                  phobos_termsets = _3;
                                  phobos_local_rewrites = _4;
                                  phobos_lexer_info = _5;
                                  phobos_assoc_info = _6;
                                  phobos_grammar_info = _7;
                                  phobos_post_rewrites = _8;
                                  phobos_inline_forms = _9
                                }
                              ) : Phobos_type.phobos_parser_return_type))
; (fun parser_env ->
    let _1 = (peek_val parser_env 1 : Phobos_type.pos) in
    let _2 = (peek_val parser_env 0 : 'identifier) in
    Obj.repr((
# 284 "phobos_parser.mly"
                                module_name := fst _2; fst _2 ) : 'module_name))
; (fun parser_env ->
    Obj.repr((
# 290 "phobos_parser.mly"
                                [] ) : 'includes))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'include_list_rev) in
    Obj.repr((
# 291 "phobos_parser.mly"
                                let mdl_names = List.flatten (List.rev _1) in
                                   process_includes !Phobos_state.phobos_paths mdl_names;
                                   mdl_names
                              ) : 'includes))
; (fun parser_env ->
    let _1 = (peek_val parser_env 1 : 'include_list_rev) in
    let _2 = (peek_val parser_env 0 : 'include_item) in
    Obj.repr((
# 298 "phobos_parser.mly"
                                _2 :: _1 ) : 'include_list_rev))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'include_item) in
    Obj.repr((
# 299 "phobos_parser.mly"
                                [_1] ) : 'include_list_rev))
; (fun parser_env ->
    let _1 = (peek_val parser_env 1 : Phobos_type.pos) in
    let _2 = (peek_val parser_env 0 : 'string_list) in
    Obj.repr((
# 302 "phobos_parser.mly"
                                _2 ) : 'include_item))
; (fun parser_env ->
    let _1 = (peek_val parser_env 1 : Phobos_type.pos) in
    let _2 = (peek_val parser_env 0 : 'identifier) in
    Obj.repr((
# 303 "phobos_parser.mly"
                                include_built_in _2; [] ) : 'include_item))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'string_list_rev) in
    Obj.repr((
# 306 "phobos_parser.mly"
                                List.rev _1 ) : 'string_list))
; (fun parser_env ->
    let _1 = (peek_val parser_env 1 : 'string_list) in
    let _2 = (peek_val parser_env 0 : string * Phobos_type.pos) in
    Obj.repr((
# 309 "phobos_parser.mly"
                                (fst _2) :: _1 ) : 'string_list_rev))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : string * Phobos_type.pos) in
    Obj.repr((
# 310 "phobos_parser.mly"
                                [fst _1] ) : 'string_list_rev))
; (fun parser_env ->
    Obj.repr((
# 316 "phobos_parser.mly"
                                [] ) : 'term_sections))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'term_section_list) in
    Obj.repr((
# 317 "phobos_parser.mly"
                                List.rev _1 ) : 'term_sections))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'term_section) in
    Obj.repr((
# 320 "phobos_parser.mly"
                                [_1] ) : 'term_section_list))
; (fun parser_env ->
    let _1 = (peek_val parser_env 1 : 'term_section_list) in
    let _2 = (peek_val parser_env 0 : 'term_section) in
    Obj.repr((
# 322 "phobos_parser.mly"
                                _2 :: _1 ) : 'term_section_list))
; (fun parser_env ->
    let _1 = (peek_val parser_env 1 : Phobos_type.pos) in
    let _2 = (peek_val parser_env 0 : 'term_options) in
    Obj.repr((
# 325 "phobos_parser.mly"
                                _2 ) : 'term_section))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'term_option_list) in
    Obj.repr((
# 328 "phobos_parser.mly"
                                List.rev _1 ) : 'term_options))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'term_option) in
    Obj.repr((
# 331 "phobos_parser.mly"
                                [_1] ) : 'term_option_list))
; (fun parser_env ->
    let _1 = (peek_val parser_env 1 : 'term_option_list) in
    let _2 = (peek_val parser_env 0 : 'term_option) in
    Obj.repr((
# 333 "phobos_parser.mly"
                                _2 :: _1 ) : 'term_option_list))
; (fun parser_env ->
    let _1 = (peek_val parser_env 4 : Phobos_type.pos) in
    let _2 = (peek_val parser_env 3 : string * Phobos_type.pos) in
    let _3 = (peek_val parser_env 2 : Phobos_type.pos) in
    let _4 = (peek_val parser_env 1 : 'opt_term_declarations) in
    let _5 = (peek_val parser_env 0 : Phobos_type.pos) in
    Obj.repr((
# 337 "phobos_parser.mly"
                                new_terms (fst _2) _4;
                                Term_extend (fst _2, _4)
                              ) : 'term_option))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : string * Phobos_type.pos) in
    Obj.repr((
# 340 "phobos_parser.mly"
                                raise (ParseError (snd _1, string_add ["Invalid option \""; fst _1; "\""])) ) : 'term_option))
; (fun parser_env ->
    Obj.repr((
# 346 "phobos_parser.mly"
                                [] ) : 'opt_preamble))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'preamble) in
    Obj.repr((
# 347 "phobos_parser.mly"
                                _1 ) : 'opt_preamble))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : Phobos_type.pos) in
    let _2 = (peek_val parser_env 1 : 'new_rewrites) in
    let _3 = (peek_val parser_env 0 : Phobos_type.pos) in
    Obj.repr((
# 351 "phobos_parser.mly"
                                _2 ) : 'preamble))
; (fun parser_env ->
    Obj.repr((
# 357 "phobos_parser.mly"
                                [], [] ) : 'lexer))
; (fun parser_env ->
    let _1 = (peek_val parser_env 4 : Phobos_type.pos) in
    let _2 = (peek_val parser_env 3 : 'opt_lexer_options) in
    let _3 = (peek_val parser_env 2 : Phobos_type.pos) in
    let _4 = (peek_val parser_env 1 : 'tokens) in
    let _5 = (peek_val parser_env 0 : Phobos_type.pos) in
    Obj.repr((
# 359 "phobos_parser.mly"
                                _4, _2 ) : 'lexer))
; (fun parser_env ->
    Obj.repr((
# 362 "phobos_parser.mly"
                                [] ) : 'opt_lexer_options))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'lexer_option_list) in
    Obj.repr((
# 363 "phobos_parser.mly"
                                List.rev _1 ) : 'opt_lexer_options))
; (fun parser_env ->
    let _1 = (peek_val parser_env 1 : 'lexer_option_list) in
    let _2 = (peek_val parser_env 0 : 'lexer_option) in
    Obj.repr((
# 367 "phobos_parser.mly"
                                _2 :: _1 ) : 'lexer_option_list))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'lexer_option) in
    Obj.repr((
# 368 "phobos_parser.mly"
                                [_1] ) : 'lexer_option_list))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : Phobos_type.pos) in
    Obj.repr((
# 371 "phobos_parser.mly"
                                Lo_longest ) : 'lexer_option))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : Phobos_type.pos) in
    Obj.repr((
# 372 "phobos_parser.mly"
                                Lo_first ) : 'lexer_option))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : string * Phobos_type.pos) in
    Obj.repr((
# 373 "phobos_parser.mly"
                                raise (ParseError (snd _1, string_add ["Invalid option \""; fst _1; "\""])) ) : 'lexer_option))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'token_list) in
    Obj.repr((
# 376 "phobos_parser.mly"
                                List.rev _1 ) : 'tokens))
; (fun parser_env ->
    let _1 = (peek_val parser_env 1 : 'token_list) in
    let _2 = (peek_val parser_env 0 : 'token) in
    Obj.repr((
# 379 "phobos_parser.mly"
                                _2 :: _1 ) : 'token_list))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'token) in
    Obj.repr((
# 380 "phobos_parser.mly"
                                [_1] ) : 'token_list))
; (fun parser_env ->
    let _1 = (peek_val parser_env 3 : 'token_decl) in
    let _2 = (peek_val parser_env 2 : Phobos_type.pos) in
    let _3 = (peek_val parser_env 1 : string * Phobos_type.pos) in
    let _4 = (peek_val parser_env 0 : 'token_body) in
    Obj.repr((
# 384 "phobos_parser.mly"
                                false, _1, fst _3, fst _4 ) : 'token))
; (fun parser_env ->
    let _1 = (peek_val parser_env 4 : Phobos_type.pos) in
    let _2 = (peek_val parser_env 3 : 'token_decl) in
    let _3 = (peek_val parser_env 2 : Phobos_type.pos) in
    let _4 = (peek_val parser_env 1 : string * Phobos_type.pos) in
    let _5 = (peek_val parser_env 0 : 'token_body) in
    Obj.repr((
# 386 "phobos_parser.mly"
                                true, _2, fst _4, fst _5 ) : 'token))
; (fun parser_env ->
    let _1 = (peek_val parser_env 1 : string * Phobos_type.pos) in
    let _2 = (peek_val parser_env 0 : 'opt_token_options) in
    Obj.repr((
# 389 "phobos_parser.mly"
                                _1, _2 ) : 'token_decl))
; (fun parser_env ->
    Obj.repr((
# 392 "phobos_parser.mly"
                                [] ) : 'opt_token_options))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'token_option_list) in
    Obj.repr((
# 393 "phobos_parser.mly"
                                _1 ) : 'opt_token_options))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'token_options_rev) in
    Obj.repr((
# 396 "phobos_parser.mly"
                                List.rev _1 ) : 'token_option_list))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'token_option) in
    Obj.repr((
# 399 "phobos_parser.mly"
                                [_1] ) : 'token_options_rev))
; (fun parser_env ->
    let _1 = (peek_val parser_env 1 : 'token_options_rev) in
    let _2 = (peek_val parser_env 0 : 'token_option) in
    Obj.repr((
# 401 "phobos_parser.mly"
                                _2 :: _1 ) : 'token_options_rev))
; (fun parser_env ->
    let _1 = (peek_val parser_env 1 : Phobos_type.pos) in
    let _2 = (peek_val parser_env 0 : 'identifiers_with_comma) in
    Obj.repr((
# 405 "phobos_parser.mly"
                                Token_extend (Some _2) ) : 'token_option))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : Phobos_type.pos) in
    Obj.repr((
# 406 "phobos_parser.mly"
                                Token_extend None ) : 'token_option))
; (fun parser_env ->
    let _1 = (peek_val parser_env 1 : Phobos_type.pos) in
    let _2 = (peek_val parser_env 0 : 'strings_with_comma) in
    Obj.repr((
# 408 "phobos_parser.mly"
                                Token_remove _2 ) : 'token_option))
; (fun parser_env ->
    let _1 = (peek_val parser_env 1 : Phobos_type.pos) in
    let _2 = (peek_val parser_env 0 : 'identifiers_with_comma) in
    Obj.repr((
# 410 "phobos_parser.mly"
                                Token_override _2 ) : 'token_option))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'strings_rev_with_comma) in
    Obj.repr((
# 413 "phobos_parser.mly"
                                List.rev _1 ) : 'strings_with_comma))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : string * Phobos_type.pos) in
    Obj.repr((
# 416 "phobos_parser.mly"
                                [_1] ) : 'strings_rev_with_comma))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'strings_rev_with_comma) in
    let _2 = (peek_val parser_env 1 : Phobos_type.pos) in
    let _3 = (peek_val parser_env 0 : string * Phobos_type.pos) in
    Obj.repr((
# 418 "phobos_parser.mly"
                                _3 :: _1 ) : 'strings_rev_with_comma))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'rev_identifier_list) in
    Obj.repr((
# 421 "phobos_parser.mly"
                                List.rev _1 ) : 'identifiers))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'identifier) in
    Obj.repr((
# 424 "phobos_parser.mly"
                                [_1] ) : 'rev_identifier_list))
; (fun parser_env ->
    let _1 = (peek_val parser_env 1 : 'rev_identifier_list) in
    let _2 = (peek_val parser_env 0 : 'identifier) in
    Obj.repr((
# 426 "phobos_parser.mly"
                                _2 :: _1 ) : 'rev_identifier_list))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'rev_identifier_list_with_comma) in
    Obj.repr((
# 430 "phobos_parser.mly"
                                List.rev _1 ) : 'identifiers_with_comma))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'identifier) in
    Obj.repr((
# 433 "phobos_parser.mly"
                                [_1] ) : 'rev_identifier_list_with_comma))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'rev_identifier_list_with_comma) in
    let _2 = (peek_val parser_env 1 : Phobos_type.pos) in
    let _3 = (peek_val parser_env 0 : 'identifier) in
    Obj.repr((
# 435 "phobos_parser.mly"
                                _3 :: _1 ) : 'rev_identifier_list_with_comma))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : string * Phobos_type.pos) in
    Obj.repr((
# 438 "phobos_parser.mly"
                                _1 ) : 'identifier))
; (fun parser_env ->
    Obj.repr((
# 444 "phobos_parser.mly"
                                [] ) : 'opt_assocs))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'assoc_list) in
    Obj.repr((
# 445 "phobos_parser.mly"
                                List.rev _1 ) : 'opt_assocs))
; (fun parser_env ->
    let _1 = (peek_val parser_env 1 : 'assoc_list) in
    let _2 = (peek_val parser_env 0 : 'assoc) in
    Obj.repr((
# 448 "phobos_parser.mly"
                                _2 :: _1 ) : 'assoc_list))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'assoc) in
    Obj.repr((
# 449 "phobos_parser.mly"
                                [_1] ) : 'assoc_list))
; (fun parser_env ->
    let _1 = (peek_val parser_env 1 : Phobos_type.pos) in
    let _2 = (peek_val parser_env 0 : 'identifiers) in
    Obj.repr((
# 452 "phobos_parser.mly"
                                Dir_nonassoc _2 ) : 'assoc))
; (fun parser_env ->
    let _1 = (peek_val parser_env 1 : Phobos_type.pos) in
    let _2 = (peek_val parser_env 0 : 'identifiers) in
    Obj.repr((
# 453 "phobos_parser.mly"
                                Dir_leftassoc _2 ) : 'assoc))
; (fun parser_env ->
    let _1 = (peek_val parser_env 1 : Phobos_type.pos) in
    let _2 = (peek_val parser_env 0 : 'identifiers) in
    Obj.repr((
# 454 "phobos_parser.mly"
                                Dir_rightassoc _2 ) : 'assoc))
; (fun parser_env ->
    Obj.repr((
# 460 "phobos_parser.mly"
                                [], [] ) : Phobos_type.pre_rule list * Phobos_type.goption list))
; (fun parser_env ->
    let _1 = (peek_val parser_env 4 : Phobos_type.pos) in
    let _2 = (peek_val parser_env 3 : 'opt_grammar_options) in
    let _3 = (peek_val parser_env 2 : Phobos_type.pos) in
    let _4 = (peek_val parser_env 1 : Phobos_type.pre_rule list) in
    let _5 = (peek_val parser_env 0 : Phobos_type.pos) in
    Obj.repr((
# 462 "phobos_parser.mly"
                                _4, _2 ) : Phobos_type.pre_rule list * Phobos_type.goption list))
; (fun parser_env ->
    Obj.repr((
# 465 "phobos_parser.mly"
                                [] ) : 'opt_grammar_options))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'grammar_option_list) in
    Obj.repr((
# 466 "phobos_parser.mly"
                                List.rev _1 ) : 'opt_grammar_options))
; (fun parser_env ->
    let _1 = (peek_val parser_env 1 : 'grammar_option_list) in
    let _2 = (peek_val parser_env 0 : 'grammar_option) in
    Obj.repr((
# 470 "phobos_parser.mly"
                                _2 :: _1 ) : 'grammar_option_list))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'grammar_option) in
    Obj.repr((
# 471 "phobos_parser.mly"
                                [_1] ) : 'grammar_option_list))
; (fun parser_env ->
    let _1 = (peek_val parser_env 1 : Phobos_type.pos) in
    let _2 = (peek_val parser_env 0 : 'identifier) in
    Obj.repr((
# 474 "phobos_parser.mly"
                                Go_start (fst _2) ) : 'grammar_option))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : Phobos_type.pre_rule list) in
    Obj.repr((
# 477 "phobos_parser.mly"
                                _1 ) : Phobos_type.pre_rule list))
; (fun parser_env ->
    let _1 = (peek_val parser_env 1 : Phobos_type.pre_rule list) in
    let _2 = (peek_val parser_env 0 : Phobos_type.pre_rule list) in
    Obj.repr((
# 480 "phobos_parser.mly"
                                _1 @ _2 ) : Phobos_type.pre_rule list))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : Phobos_type.pre_rule list) in
    Obj.repr((
# 481 "phobos_parser.mly"
                                _1 ) : Phobos_type.pre_rule list))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : string * Phobos_type.pos) in
    let _2 = (peek_val parser_env 1 : Phobos_type.pos) in
    let _3 = (peek_val parser_env 0 : 'productions) in
    Obj.repr((
# 485 "phobos_parser.mly"
                                make_rules _1 _3 ) : Phobos_type.pre_rule list))
; (fun parser_env ->
    let _1 = (peek_val parser_env 1 : 'prod_body) in
    let _2 = (peek_val parser_env 0 : 'prods) in
    Obj.repr((
# 488 "phobos_parser.mly"
                                _1 :: _2 ) : 'productions))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'prod_body) in
    Obj.repr((
# 489 "phobos_parser.mly"
                                [_1] ) : 'productions))
; (fun parser_env ->
    Obj.repr((
# 492 "phobos_parser.mly"
                                None ) : 'opt_prec))
; (fun parser_env ->
    let _1 = (peek_val parser_env 1 : Phobos_type.pos) in
    let _2 = (peek_val parser_env 0 : string * Phobos_type.pos) in
    Obj.repr((
# 493 "phobos_parser.mly"
                                Some _2 ) : 'opt_prec))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'prod_list) in
    Obj.repr((
# 496 "phobos_parser.mly"
                                List.rev _1 ) : 'prods))
; (fun parser_env ->
    let _1 = (peek_val parser_env 1 : 'prod_list) in
    let _2 = (peek_val parser_env 0 : 'prod_list_prim) in
    Obj.repr((
# 499 "phobos_parser.mly"
                                _2 :: _1 ) : 'prod_list))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'prod_list_prim) in
    Obj.repr((
# 500 "phobos_parser.mly"
                                [_1] ) : 'prod_list))
; (fun parser_env ->
    let _1 = (peek_val parser_env 1 : Phobos_type.pos) in
    let _2 = (peek_val parser_env 0 : 'prod_body) in
    Obj.repr((
# 503 "phobos_parser.mly"
                                _2 ) : 'prod_list_prim))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'alt_prod_elements) in
    let _2 = (peek_val parser_env 1 : 'opt_prec) in
    let _3 = (peek_val parser_env 0 : Phobos_type.mp_pre_rewrite list * Phobos_type.pos) in
    Obj.repr((
# 507 "phobos_parser.mly"
                                let ids = List.map fst _1 in
                                   ids, _2, insert_rewrite_if_needed ids (fst _3)
                              ) : 'prod_body))
; (fun parser_env ->
    let _1 = (peek_val parser_env 3 : 'alt_prod_elements) in
    let _2 = (peek_val parser_env 2 : 'opt_prec) in
    let _3 = (peek_val parser_env 1 : Phobos_type.pos) in
    let _4 = (peek_val parser_env 0 : Phobos_type.mp_pre_term) in
    Obj.repr((
# 511 "phobos_parser.mly"
                                production_of_alt_syntax _1 _2 _4 ) : 'prod_body))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'alt_prod_element_list) in
    Obj.repr((
# 514 "phobos_parser.mly"
                                List.rev _1 ) : 'alt_prod_elements))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'alt_prod_element) in
    Obj.repr((
# 517 "phobos_parser.mly"
                                [_1] ) : 'alt_prod_element_list))
; (fun parser_env ->
    let _1 = (peek_val parser_env 1 : 'alt_prod_element_list) in
    let _2 = (peek_val parser_env 0 : 'alt_prod_element) in
    Obj.repr((
# 519 "phobos_parser.mly"
                                _2 :: _1 ) : 'alt_prod_element_list))
; (fun parser_env ->
    let _1 = (peek_val parser_env 3 : 'identifier) in
    let _2 = (peek_val parser_env 2 : Phobos_type.pos) in
    let _3 = (peek_val parser_env 1 : Phobos_type.mp_pre_term) in
    let _4 = (peek_val parser_env 0 : Phobos_type.pos) in
    Obj.repr((
# 523 "phobos_parser.mly"
                                _1, Some _3 ) : 'alt_prod_element))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'identifier) in
    Obj.repr((
# 524 "phobos_parser.mly"
                                _1, None ) : 'alt_prod_element))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : Phobos_type.pos) in
    let _2 = (peek_val parser_env 1 : 'token_term_matches) in
    let _3 = (peek_val parser_env 0 : Phobos_type.pos) in
    Obj.repr((
# 528 "phobos_parser.mly"
                                _2, union_pos _1 _3 ) : 'token_body))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : Phobos_type.pos) in
    let _2 = (peek_val parser_env 1 : 'opt_term_matches) in
    let _3 = (peek_val parser_env 0 : Phobos_type.pos) in
    Obj.repr((
# 532 "phobos_parser.mly"
                                _2, union_pos _1 _3 ) : Phobos_type.mp_pre_rewrite list * Phobos_type.pos))
; (fun parser_env ->
    Obj.repr((
# 535 "phobos_parser.mly"
                                [] ) : 'new_rewrites))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'new_rewrite_list) in
    Obj.repr((
# 536 "phobos_parser.mly"
                                List.rev _1 ) : 'new_rewrites))
; (fun parser_env ->
    let _1 = (peek_val parser_env 1 : 'new_rewrite_list) in
    let _2 = (peek_val parser_env 0 : 'new_rewrite) in
    Obj.repr((
# 540 "phobos_parser.mly"
                                _2 :: _1 ) : 'new_rewrite_list))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'new_rewrite) in
    Obj.repr((
# 541 "phobos_parser.mly"
                                [_1] ) : 'new_rewrite_list))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'from) in
    let _2 = (peek_val parser_env 1 : Phobos_type.pos) in
    let _3 = (peek_val parser_env 0 : Phobos_type.mp_pre_term) in
    Obj.repr((
# 544 "phobos_parser.mly"
                                _1, _3 ) : 'new_rewrite))
; (fun parser_env ->
    Obj.repr((
# 547 "phobos_parser.mly"
                                let term = pho_make_token_term () in
                                   [([(term, bogus_pos)], (term, bogus_pos))]
                              ) : 'token_term_matches))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : Phobos_type.mp_pre_rewrite list) in
    Obj.repr((
# 550 "phobos_parser.mly"
                                List.rev _1 ) : 'token_term_matches))
; (fun parser_env ->
    Obj.repr((
# 553 "phobos_parser.mly"
                                [] ) : 'opt_term_matches))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : Phobos_type.mp_pre_rewrite list) in
    Obj.repr((
# 554 "phobos_parser.mly"
                                List.rev _1 ) : 'opt_term_matches))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : Phobos_type.mp_pre_rewrite list) in
    let _2 = (peek_val parser_env 1 : Phobos_type.pos) in
    let _3 = (peek_val parser_env 0 : 'term_match) in
    Obj.repr((
# 558 "phobos_parser.mly"
                                _3 :: _1 ) : Phobos_type.mp_pre_rewrite list))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'term_match) in
    Obj.repr((
# 559 "phobos_parser.mly"
                                [_1] ) : Phobos_type.mp_pre_rewrite list))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'froms) in
    let _2 = (peek_val parser_env 1 : Phobos_type.pos) in
    let _3 = (peek_val parser_env 0 : Phobos_type.mp_pre_term) in
    Obj.repr((
# 562 "phobos_parser.mly"
                                _1, _3 ) : 'term_match))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'from_list) in
    Obj.repr((
# 565 "phobos_parser.mly"
                                List.rev _1 ) : 'froms))
; (fun parser_env ->
    let _1 = (peek_val parser_env 1 : 'from_list) in
    let _2 = (peek_val parser_env 0 : 'from) in
    Obj.repr((
# 568 "phobos_parser.mly"
                                _2 :: _1 ) : 'from_list))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'from) in
    Obj.repr((
# 569 "phobos_parser.mly"
                                [_1] ) : 'from_list))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : Phobos_type.mp_pre_term) in
    Obj.repr((
# 572 "phobos_parser.mly"
                                _1 ) : 'from))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'identifier) in
    Obj.repr((
# 573 "phobos_parser.mly"
                                pho_make_token_term (), snd _1 ) : 'from))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : string * Phobos_type.pos) in
    Obj.repr((
# 576 "phobos_parser.mly"
                                _1 ) : 'quoted_identifier))
; (fun parser_env ->
    Obj.repr((
# 579 "phobos_parser.mly"
                                [] ) : 'opt_simple_terms))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'simple_term_list_rev) in
    Obj.repr((
# 580 "phobos_parser.mly"
                                List.rev _1 ) : 'opt_simple_terms))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : Phobos_type.mp_pre_term) in
    Obj.repr((
# 583 "phobos_parser.mly"
                                [_1] ) : 'simple_term_list_rev))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'simple_term_list_rev) in
    let _2 = (peek_val parser_env 1 : Phobos_type.pos) in
    let _3 = (peek_val parser_env 0 : Phobos_type.mp_pre_term) in
    Obj.repr((
# 585 "phobos_parser.mly"
                                _3 :: _1 ) : 'simple_term_list_rev))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'module_identifier) in
    let _2 = (peek_val parser_env 1 : 'opt_term_params) in
    let _3 = (peek_val parser_env 0 : 'subterms) in
    Obj.repr((
# 589 "phobos_parser.mly"
                                pho_make_term (fst _1) _2 (fst _3), union_pos (snd _1) (snd _3) ) : Phobos_type.mp_pre_term))
; (fun parser_env ->
    let _1 = (peek_val parser_env 1 : 'module_identifier) in
    let _2 = (peek_val parser_env 0 : 'term_params) in
    Obj.repr((
# 591 "phobos_parser.mly"
                                pho_make_term (fst _1) (fst _2) [], union_pos (snd _1) (snd _2) ) : Phobos_type.mp_pre_term))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'quoted_identifier) in
    Obj.repr((
# 592 "phobos_parser.mly"
                                pho_make_var_term _1, snd _1 ) : Phobos_type.mp_pre_term))
; (fun parser_env ->
    let _1 = (peek_val parser_env 3 : 'quoted_identifier) in
    let _2 = (peek_val parser_env 2 : Phobos_type.pos) in
    let _3 = (peek_val parser_env 1 : 'opt_simple_terms) in
    let _4 = (peek_val parser_env 0 : Phobos_type.pos) in
    Obj.repr((
# 594 "phobos_parser.mly"
                                print_string "so_var["; print_string (fst _1); print_string "]\n"; pho_make_so_var_term _1 _3, union_pos (snd _1) _4 ) : Phobos_type.mp_pre_term))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : int * Phobos_type.pos) in
    Obj.repr((
# 595 "phobos_parser.mly"
                                pho_make_number_term _1, snd _1 ) : Phobos_type.mp_pre_term))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : Phobos_type.pos) in
    Obj.repr((
# 596 "phobos_parser.mly"
                                pho_make_unique_var_term ("?", _1), _1 ) : Phobos_type.mp_pre_term))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'modules_rev) in
    Obj.repr((
# 601 "phobos_parser.mly"
                                _1 ) : 'module_identifier))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'identifier) in
    Obj.repr((
# 604 "phobos_parser.mly"
                                [_1], snd _1 ) : 'modules_rev))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'modules_rev) in
    let _2 = (peek_val parser_env 1 : Phobos_type.pos) in
    let _3 = (peek_val parser_env 0 : 'identifier) in
    Obj.repr((
# 606 "phobos_parser.mly"
                                _3 :: fst _1, union_pos (snd _1) (snd _3) ) : 'modules_rev))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : Phobos_type.pos) in
    let _2 = (peek_val parser_env 1 : Phobos_type.pos) in
    let _3 = (peek_val parser_env 0 : 'identifier) in
    Obj.repr((
# 607 "phobos_parser.mly"
                                _3 :: [("", _1)], union_pos _1 (snd _3) ) : 'modules_rev))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : Phobos_type.pos) in
    let _2 = (peek_val parser_env 1 : 'opt_subterm_list_semi) in
    let _3 = (peek_val parser_env 0 : Phobos_type.pos) in
    Obj.repr((
# 614 "phobos_parser.mly"
                                _2, union_pos _1 _3 ) : 'subterms))
; (fun parser_env ->
    Obj.repr((
# 617 "phobos_parser.mly"
                                [] ) : 'opt_subterm_list_semi))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'subterms_with_semi) in
    Obj.repr((
# 618 "phobos_parser.mly"
                                _1 ) : 'opt_subterm_list_semi))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'subterm_list_with_semi) in
    Obj.repr((
# 621 "phobos_parser.mly"
                                List.rev _1 ) : 'subterms_with_semi))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'subterm_list_with_semi) in
    let _2 = (peek_val parser_env 1 : Phobos_type.pos) in
    let _3 = (peek_val parser_env 0 : 'sub_term) in
    Obj.repr((
# 625 "phobos_parser.mly"
                                _3 :: _1 ) : 'subterm_list_with_semi))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'sub_term) in
    Obj.repr((
# 626 "phobos_parser.mly"
                                [_1] ) : 'subterm_list_with_semi))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : Phobos_type.mp_pre_term) in
    Obj.repr((
# 629 "phobos_parser.mly"
                                pho_make_bterm [] _1 ) : 'sub_term))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'identifiers_with_comma) in
    let _2 = (peek_val parser_env 1 : Phobos_type.pos) in
    let _3 = (peek_val parser_env 0 : Phobos_type.mp_pre_term) in
    Obj.repr((
# 631 "phobos_parser.mly"
                                pho_make_bterm _1 _3 ) : 'sub_term))
; (fun parser_env ->
    Obj.repr((
# 635 "phobos_parser.mly"
                                [] ) : 'opt_term_params))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'term_params) in
    Obj.repr((
# 636 "phobos_parser.mly"
                                List.rev (fst _1) ) : 'opt_term_params))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : Phobos_type.pos) in
    let _2 = (peek_val parser_env 1 : 'term_param_list) in
    let _3 = (peek_val parser_env 0 : Phobos_type.pos) in
    Obj.repr((
# 640 "phobos_parser.mly"
                                List.rev _2, union_pos _1 _3 ) : 'term_params))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'term_param_list) in
    let _2 = (peek_val parser_env 1 : Phobos_type.pos) in
    let _3 = (peek_val parser_env 0 : 'term_param) in
    Obj.repr((
# 644 "phobos_parser.mly"
                                _3 :: _1 ) : 'term_param_list))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'term_param) in
    Obj.repr((
# 645 "phobos_parser.mly"
                                [_1] ) : 'term_param_list))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : string * Phobos_type.pos) in
    Obj.repr((
# 648 "phobos_parser.mly"
                                match fst _1 with
                                   "s" -> TyString, snd _1
                                 | "n" -> TyNum, snd _1
                                 | "v" -> TyVar, snd _1
                                 | "t" -> TyToken, snd _1
                                 | "l" -> TyLevel, snd _1
                                 | _ ->
                                       raise (ParseError (snd _1, "unknown parameter type"))
                              ) : 'param_type_id))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : string * Phobos_type.pos) in
    let _2 = (peek_val parser_env 1 : Phobos_type.pos) in
    let _3 = (peek_val parser_env 0 : string * Phobos_type.pos) in
    Obj.repr((
# 661 "phobos_parser.mly"
                                match fst _3 with
                                   "s" ->
                                       make_param (MString (fst _1))
                                 | "n" ->
                                       make_param (MNumber (fst _1))
                                 | "v" ->
                                       make_param (MVar (fst _1))
                                 | "t" ->
                                       make_param (MToken (fst _1))
                                 | _ ->
                                       raise (ParseError (snd _3, "unknown meta-parameter type"))
                              ) : 'term_param))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : int * Phobos_type.pos) in
    let _2 = (peek_val parser_env 1 : Phobos_type.pos) in
    let _3 = (peek_val parser_env 0 : 'param_type_id) in
    Obj.repr((
# 675 "phobos_parser.mly"
                                match fst _3 with
                                   TyNum ->
                                       make_param (Number (num_of_int (fst _1)))
                                 | _ ->
                                       raise (ParseError (snd _3, "invalid parameter type"))
                              ) : 'term_param))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : string * Phobos_type.pos) in
    let _2 = (peek_val parser_env 1 : Phobos_type.pos) in
    let _3 = (peek_val parser_env 0 : 'param_type_id) in
    Obj.repr((
# 682 "phobos_parser.mly"
                                match fst _3 with
                                   TyString ->
                                       make_param (String (fst _1))
                                 | TyToken ->
                                       make_param (Token (fst _1))
                                 | TyVar ->
                                       make_param (Var (fst _1)) 
                                 | _ ->
                                       raise (ParseError (snd _3, "invalid parameter type"))
                              ) : 'term_param))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : string * Phobos_type.pos) in
    Obj.repr((
# 692 "phobos_parser.mly"
                                make_param (String (fst _1)) ) : 'term_param))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : int * Phobos_type.pos) in
    Obj.repr((
# 693 "phobos_parser.mly"
                                make_param (Number (num_of_int (fst _1))) ) : 'term_param))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : string * Phobos_type.pos) in
    Obj.repr((
# 695 "phobos_parser.mly"
                                make_param (MString (fst _1)) ) : 'term_param))
; (fun parser_env ->
    Obj.repr((
# 701 "phobos_parser.mly"
                                [] ) : 'opt_term_declarations))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'term_declaration_list) in
    Obj.repr((
# 702 "phobos_parser.mly"
                                _1 ) : 'opt_term_declarations))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'term_declaration) in
    Obj.repr((
# 705 "phobos_parser.mly"
                                _1 ) : 'term_declaration_list))
; (fun parser_env ->
    let _1 = (peek_val parser_env 1 : 'term_declaration_list) in
    let _2 = (peek_val parser_env 0 : 'term_declaration) in
    Obj.repr((
# 707 "phobos_parser.mly"
                                _2 @ _1 ) : 'term_declaration_list))
; (fun parser_env ->
    let _1 = (peek_val parser_env 1 : Phobos_type.pos) in
    let _2 = (peek_val parser_env 0 : 'syntax_terms) in
    Obj.repr((
# 710 "phobos_parser.mly"
                                _2 ) : 'term_declaration))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'syntax_term_list_rev) in
    Obj.repr((
# 713 "phobos_parser.mly"
                                List.rev _1 ) : 'syntax_terms))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'syntax_term) in
    Obj.repr((
# 716 "phobos_parser.mly"
                                [_1] ) : 'syntax_term_list_rev))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'syntax_term_list_rev) in
    let _2 = (peek_val parser_env 1 : Phobos_type.pos) in
    let _3 = (peek_val parser_env 0 : 'syntax_term) in
    Obj.repr((
# 718 "phobos_parser.mly"
                                _3 :: _1 ) : 'syntax_term_list_rev))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'st_identifier) in
    Obj.repr((
# 721 "phobos_parser.mly"
                                _1, 0 ) : 'syntax_term))
; (fun parser_env ->
    let _1 = (peek_val parser_env 3 : 'st_identifier) in
    let _2 = (peek_val parser_env 2 : Phobos_type.pos) in
    let _3 = (peek_val parser_env 1 : 'opt_syntax_subterms) in
    let _4 = (peek_val parser_env 0 : Phobos_type.pos) in
    Obj.repr((
# 723 "phobos_parser.mly"
                                _1, List.length _3 ) : 'syntax_term))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'identifier) in
    Obj.repr((
# 727 "phobos_parser.mly"
                                _1 ) : 'st_identifier))
; (fun parser_env ->
    let _1 = (peek_val parser_env 3 : 'identifier) in
    let _2 = (peek_val parser_env 2 : Phobos_type.pos) in
    let _3 = (peek_val parser_env 1 : 'param_type_id) in
    let _4 = (peek_val parser_env 0 : Phobos_type.pos) in
    Obj.repr((
# 729 "phobos_parser.mly"
                                _1 ) : 'st_identifier))
; (fun parser_env ->
    Obj.repr((
# 732 "phobos_parser.mly"
                                [] ) : 'opt_syntax_subterms))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'syntax_subterms) in
    Obj.repr((
# 733 "phobos_parser.mly"
                                _1 ) : 'opt_syntax_subterms))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'syntax_subterm_list) in
    Obj.repr((
# 736 "phobos_parser.mly"
                                List.rev _1 ) : 'syntax_subterms))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'syntax_subterm) in
    Obj.repr((
# 739 "phobos_parser.mly"
                                [_1] ) : 'syntax_subterm_list))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'syntax_subterm_list) in
    let _2 = (peek_val parser_env 1 : Phobos_type.pos) in
    let _3 = (peek_val parser_env 0 : 'syntax_subterm) in
    Obj.repr((
# 741 "phobos_parser.mly"
                                _3 :: _1 ) : 'syntax_subterm_list))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'syntax_subterm_base) in
    Obj.repr((
# 744 "phobos_parser.mly"
                                _1 ) : 'syntax_subterm))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'identifier) in
    let _2 = (peek_val parser_env 1 : Phobos_type.pos) in
    let _3 = (peek_val parser_env 0 : 'syntax_subterm_base) in
    Obj.repr((
# 746 "phobos_parser.mly"
                                _3 ) : 'syntax_subterm))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'quoted_identifier) in
    Obj.repr((
# 749 "phobos_parser.mly"
                                _1 ) : 'syntax_subterm_base))
; (fun parser_env ->
    let _1 = (peek_val parser_env 3 : 'quoted_identifier) in
    let _2 = (peek_val parser_env 2 : Phobos_type.pos) in
    let _3 = (peek_val parser_env 1 : 'quoted_identifier) in
    let _4 = (peek_val parser_env 0 : Phobos_type.pos) in
    Obj.repr((
# 751 "phobos_parser.mly"
                                _1 ) : 'syntax_subterm_base))
; (fun parser_env ->
    Obj.repr((
# 757 "phobos_parser.mly"
                                [] ) : 'opt_rewrites_section))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'rev_rewrites_section_list) in
    Obj.repr((
# 758 "phobos_parser.mly"
                                List.rev _1 ) : 'opt_rewrites_section))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'rewrites_section) in
    Obj.repr((
# 761 "phobos_parser.mly"
                                [_1] ) : 'rev_rewrites_section_list))
; (fun parser_env ->
    let _1 = (peek_val parser_env 1 : 'rev_rewrites_section_list) in
    let _2 = (peek_val parser_env 0 : 'rewrites_section) in
    Obj.repr((
# 763 "phobos_parser.mly"
                                _2 :: _1 ) : 'rev_rewrites_section_list))
; (fun parser_env ->
    let _1 = (peek_val parser_env 3 : Phobos_type.pos) in
    let _2 = (peek_val parser_env 2 : Phobos_type.pos) in
    let _3 = (peek_val parser_env 1 : 'new_rewrites) in
    let _4 = (peek_val parser_env 0 : Phobos_type.pos) in
    Obj.repr((
# 767 "phobos_parser.mly"
                                _3 ) : 'rewrites_section))
; (fun parser_env ->
    Obj.repr((
# 773 "phobos_parser.mly"
                                [] ) : 'opt_inline_forms))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'inline_form_list_rev) in
    Obj.repr((
# 774 "phobos_parser.mly"
                                List.rev _1 ) : 'opt_inline_forms))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'inline_form) in
    Obj.repr((
# 777 "phobos_parser.mly"
                                [_1] ) : 'inline_form_list_rev))
; (fun parser_env ->
    let _1 = (peek_val parser_env 1 : 'inline_form_list_rev) in
    let _2 = (peek_val parser_env 0 : 'inline_form) in
    Obj.repr((
# 779 "phobos_parser.mly"
                                _2 :: _1 ) : 'inline_form_list_rev))
; (fun parser_env ->
    let _1 = (peek_val parser_env 1 : Phobos_type.pos) in
    let _2 = (peek_val parser_env 0 : Phobos_type.mp_pre_term) in
    Obj.repr((
# 782 "phobos_parser.mly"
                                _2 ) : 'inline_form))
(* Entry main *)
; (fun parser_env -> raise (YYexit (peek_val parser_env 0)))
|]
let yytables =
  { actions=yyact;
    transl_const=yytransl_const;
    transl_block=yytransl_block;
    lhs=yylhs;
    len=yylen;
    defred=yydefred;
    dgoto=yydgoto;
    sindex=yysindex;
    rindex=yyrindex;
    gindex=yygindex;
    tablesize=yytablesize;
    table=yytable;
    check=yycheck;
    error_function=parse_error;
    names_const=yynames_const;
    names_block=yynames_block }
let main (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (yyparse yytables 1 lexfun lexbuf : Phobos_type.phobos_parser_return_type)
