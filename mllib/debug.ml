(*
 * Debugging utilities.
 *)

open Printf

(************************************************************************
 * TYPES                                                                *
 ************************************************************************)

(*
 * Info needed for a debug variable.
 *)
type debug_info =
   { debug_name : string;
     debug_description : string;
     debug_value : bool
   }

(*
 * Info about variables.
 *)
type info =
   { info_name : string;
     mutable info_info : string option;
     info_flag : bool ref
   }

(************************************************************************
 * UTILITIES                                                            *
 ************************************************************************)

(*
 * Print some strings.
 *)
let rec print_strings sep out = function
   [h] ->
      output_string out h
 | h::t ->
      output_string out h;
      output_string out sep;
      print_strings sep out t
 | [] ->
      ()

(*
 * Print a newline and flush.
 *)
let eflush out =
   output_char out '\n';
   flush out

(************************************************************************
 * DEBUG                                                                *
 ************************************************************************)

(*
 * Initial info is empty.
 *)
let info = ref []

(*
 * Description of debug flags added from the command line.
 *)
let default_description = "Unitialized debug flag"

(*
 * C debugging functions.
 *)
external ml_debug : string -> bool -> unit = "ml_debug"
external ml_get_debug : string -> string * bool = "ml_get_debug"
external ml_debuggers : unit -> (string * string * bool) array = "ml_debuggers"

(*
 * List all the debug flags.
 *)
let debuggers () =
   let collect { info_name = name; info_info = info; info_flag = flag } =
      let info =
         match info with
            Some info ->
               info
          | None ->
               default_description
      in
         { debug_name = name; debug_description = info; debug_value = !flag }
   in
   let collect' (name, info, flag) =
      { debug_name = name; debug_description = info; debug_value = flag }
   in
      Array.append (**)
         (Array.of_list (List.map collect !info))
         (Array.map collect' (ml_debuggers ()))

(*
 * Print a usage argument.
 *)
let debug_usage () =
   let usage { debug_name = name; debug_description = desc; debug_value = flag } =
      eprintf "\t%s: %s: %b\n" name desc flag
   in
      eprintf "Debugging variables:\n";
      Array.iter usage (debuggers ());
      flush stderr

(*
 * Create a debugging variable.
 *)
let create_debug
    { debug_name = name;
      debug_description = desc;
      debug_value = flag
    } =
   let vinfo = !info in
   let rec search = function
      info :: t ->
         let { info_name = name'; info_info = desc'; info_flag = flag' } = info in
            if name' = name then
               match desc' with
                  None ->
                     info.info_info <- Some desc;
                     flag'
                | Some _ ->
                     raise (Failure (sprintf "Debug.create_debug: variable '%s' is already created" name))
            else
               search t
    | [] ->
         let flag' = ref flag in
         let ninfo =
            { info_name = name;
              info_info = Some desc;
              info_flag = flag'
            }
         in
            info := ninfo :: vinfo;
            flag'
   in
      search vinfo

(*
 * Get the value of a debugging variable.
 *)
let load_debug name =
   let rec search = function
      { info_name = name'; info_flag = flag } :: t ->
         if name' = name then
            flag
         else
            search t
    | [] ->
         raise (Failure (sprintf "Debug.load_debug: variable '%s' has not been created" name))
   in
      search !info

(*
 * Modify a debugging flag.
 *)
let set_debug name flag =
   let rec search = function
      h :: t ->
         let { info_name = name'; info_flag = flag' } = h in
            if name' = name then
               flag' := flag
            else
               search t
    | [] ->
         (* Try a C function *)
         try ml_debug name flag with
            Failure "ml_debug" ->
               raise (Failure "set_debug")
   in
      search !info

(*
 * Possible debug flag.
 * Try setting the flag first.
 *)
let set_possible_debug name flag =
   try set_debug name flag with
      Failure "set_debug" ->
         let flag' = ref flag in
         let ninfo =
            { info_name = name;
              info_info = None;
              info_flag = flag'
            }
         in
            info := ninfo :: !info

(*
 * Get the value of a debugging flag.
 *)
let get_debug name =
   let rec search = function
      h :: t ->
         if h.info_name = name then
            let { info_info = description; info_flag = flag } = h in
            let description =
               match description with
                  Some desc ->
                     desc
                | None ->
                     default_description
            in
               { debug_name = name;
                 debug_description = description;
                 debug_value = !flag
               }
         else
            search t
    | [] ->
         (* Try a C function *)
         try
            let info, flag = ml_get_debug name in
               { debug_name = name;
                 debug_description = info;
                 debug_value = flag
               }
         with
            Failure "ml_get_debug" ->
               eprintf "Debug.get_debug: no such variable: %s%t" name eflush;
               raise (Failure "get_debug")
   in
      search !info

(*
 * Check for no remaining possible debug flags.
 *)
let check_debug () =
   ()
(*
   if List.exists (fun info -> info.info_info = None) !info then
      begin
         debug_usage ();
         raise (Failure "check_debug")
      end
*)

(************************************************************************
 * PARTICULAR DEBUG                                                     *
 ************************************************************************)

(*
 * File loading.
 *)
let debug_load = create_debug (**)
                    { debug_name = "load";
                      debug_description = "Print file names as they load";
                      debug_value = false
                    }

(*
 * $Log$
 * Revision 1.8  1998/06/12 18:36:26  jyh
 * Working factorial proof.
 *
 * Revision 1.7  1998/06/12 13:46:46  jyh
 * D tactic works, added itt_bool.
 *
 * Revision 1.6  1998/05/01 14:59:25  jyh
 * Updating display forms.
 *
 * Revision 1.5  1998/04/28 21:38:06  jyh
 * Adjusted uppercasing.
 *
 * Revision 1.4  1998/04/28 18:30:24  jyh
 * ls() works, adding display.
 *
 * Revision 1.3  1998/04/24 02:42:26  jyh
 * Added more extensive debugging capabilities.
 *
 * Revision 1.2  1998/02/23 14:46:30  jyh
 * First implementation of binary file compilation.
 *
 * Revision 1.1  1997/08/06 16:17:52  jyh
 * This is an ocaml version with subtyping, type inference,
 * d and eqcd tactics.  It is a basic system, but not debugged.
 *
 * Revision 1.1  1997/04/28 15:51:15  jyh
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
 * Revision 1.5  1996/03/08 15:40:36  jyh
 * This version works for most constructs except for ML rewrites.
 * The next step will be to break apart the rewriter so that
 * redices and contracta can be compiled separately.
 *
 * Revision 1.4  1996/02/25 15:16:07  jyh
 * This is a partial checkin as filterModule is being developed.
 * After the meta-logical framework is developed, sequent.* will go away.
 *
 * Revision 1.3  1996/02/18 23:32:25  jyh
 * Changin Format module to more Nuprl-like format.
 *
 * Revision 1.2  1996/02/14 03:51:48  jyh
 * This is a version common to Caml-Light and Caml-Special-Light.
 *
 * Revision 1.1  1996/02/13 21:32:03  jyh
 * This is an intermediate checkin while matching is being added to the rewriter.
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner.run"
 * End:
 * -*-
 *)
