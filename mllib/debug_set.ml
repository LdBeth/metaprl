(*
 * Commands to manipulate debug variables.
 *)

open Printf

open Debug

(*
 * Show the file loading.
 *)
let _ =
   if !debug_load then
      eprintf "Loading Debug_set%t" eflush

(*
 * Info about variables.
 *)
type debug_info = 
   { info_name : string;
     info_info : string;
     info_flag : bool ref
   }

let info =
   [|{ info_name = "load";         info_info = "Loading of files";      info_flag = debug_load };
     { info_name = "dform";        info_info = "Display forms";         info_flag = debug_dform };
     { info_name = "rewrite";      info_info = "Rewriter";              info_flag = debug_rewrite };
     { info_name = "refiner";      info_info = "Refiner";               info_flag = debug_refiner };
     { info_name = "simple_print"; info_info = "Term printer";          info_flag = debug_simple_print };
     { info_name = "file_base";    info_info = "File lookup";           info_flag = debug_file_base };
     { info_name = "grammar";      info_info = "Term grammar";          info_flag = debug_grammar };
     { info_name = "resource";     info_info = "Theory resources";      info_flag = debug_resource };
     { info_name = "library_base"; info_info = "File lookup";           info_flag = debug_library_base };
     { info_name = "summary";      info_info = "Theory summaries";      info_flag = debug_summary };
     { info_name = "prog";         info_info = "Theory compiling";      info_flag = debug_filter_prog };
     { info_name = "parse";        info_info = "Theory parsing";        info_flag = debug_filter_parse };
     { info_name = "cache";        info_info = "Summary caching";       info_flag = debug_filter_cache }
   |]

(*
 * ML debugging functions.
 *)
external ml_debug : string -> bool -> unit = "ml_debug"
external ml_get_debug : string -> string * bool = "ml_get_debug"
external ml_debuggers : unit -> (string * string * bool) array = "ml_debuggers"

(*
 * Modify a debugging flag.
 *)
let set_debug name flag =
   let len = Array.length info in
   let rec search i =
      if i = len then
         (* Try a C function *)
         try ml_debug name flag with
            Failure "ml_debug" ->
               eprintf "Debug_set.set_debug: no such variable: %s%t" name eflush;
               raise (Failure "set_debug")
      else
         let { info_name = name'; info_flag = flag' } = info.(i) in
            if name = name' then
               flag' := flag
            else
               search (i + 1)
   in
      search 0

(*
 * Get the value of a debugging flag.
 *)
let get_debug name =
   let len = Array.length info in
   let rec search i =
      if i = len then
         (* Try a C function *)
         try ml_get_debug name with
            Failure "ml_get_debug" ->
               eprintf "Debug_set.get_debug: no such variable: %s%t" name eflush;
               raise (Failure "get_debug")
      else
         let { info_name = name'; info_info = info'; info_flag = flag' } = info.(i) in
            if name = name' then
               info', !flag'
            else
               search (i + 1)
   in
      search 0

(*
 * List all the debug flags.
 *)
let debuggers () =
   let collect { info_name = name; info_info = info; info_flag = flag } =
      name, info, !flag
   in
      Array.append (Array.map collect info) (ml_debuggers ())

(*
 * Command line formatting.
 * string -> path commands
 *)
let set_path doc var path =
   List.iter (function name -> set_debug name true) (String_util.split ':' path)

let set_path_arg doc var =
   Arg.String (set_path doc var)

(*
 * This is a list of hosts to use for database lookup.
 *)
let _ = Env_arg.general "debug" [] "Debug flags" set_path set_path_arg

(*
 * Initialization doesn't do anything.
 * Its really used to force the link step.
 *)
let init () = ()

(*
 * $Log$
 * Revision 1.3  1998/04/28 18:30:26  jyh
 * ls() works, adding display.
 *
 * Revision 1.2  1998/04/24 19:38:49  jyh
 * Updated debugging.
 *
 * Revision 1.1  1998/04/24 02:42:29  jyh
 * Added more extensive debugging capabilities.
 *
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
