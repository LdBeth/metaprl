val current_line : int ref
val current_schar : int ref
val set_next_line : Lexing.lexbuf -> unit
val set_lexeme_position : Lexing.lexbuf -> string * int * int * int * int
val stringbuf : Buffer.t
val string_start : (int * int) ref
val string_add_char : 'a -> string -> unit
val pop_string : Lexing.lexbuf -> string * (string * int * int * int * int)
val set_string_start : Lexing.lexbuf -> unit
val lex_tables : Lexing.lex_tables
val main : Lexing.lexbuf -> Phobos_parser.token
val __ocaml_lex_main_rec : Lexing.lexbuf -> int -> Phobos_parser.token
val string : Lexing.lexbuf -> unit
val __ocaml_lex_string_rec : Lexing.lexbuf -> int -> unit
val comment : Lexing.lexbuf -> unit
val __ocaml_lex_comment_rec : Lexing.lexbuf -> int -> unit
