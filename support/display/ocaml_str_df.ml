(*
 * Display forms for structure items.
 *
 * ----------------------------------------------------------------
 *
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/index.html for information on Nuprl,
 * OCaml, and more information about this system.
 *
 * Copyright (C) 1998 Jason Hickey, Cornell University
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * Author: Jason Hickey
 * jyh@cs.cornell.edu
 *)

extends Ocaml
extends Ocaml_base_df
extends Ocaml_sig_df

open Lm_debug
open Lm_printf

let _ =
   show_loading "Loading Ocaml_str_df%t"

(*
 * Exception declarations name type constructors.
 *)
dform str_exception_df1 : str_exception[name:s]{'tl} =
   sig_exception[name:s]{'tl}

dform str_exception_df2 : str_exception[start:n, finish:n, name:s]{'tl} =
   str_exception[name:s]{'tl}

(*
 * External function declaration.
 *)
dform str_external_df1 : str_external[name:s]{'t; 'sl} =
   sig_external[name:s]{'t; 'sl}

dform str_external_df2 : str_external[start:n, finish:n, name:s]{'t; 'sl} =
   str_external[name:s]{'t; 'sl}

(*
 * Unnamed value.
 *)
dform str_expr_df1 : str_expr{'e} =
   szone push_indent "_let" space "_" space "=" space slot{'e} popm ezone

dform str_expr_df2 : str_expr[start:n, finish:n]{'e} =
   str_expr{'e}

(*
 * Module definition.
 *)
dform str_module_df1 : str_module[name:s]{'me} =
   szone push_indent "_module" space cd_begin[name] slot[name] cd_end space "=" space slot{'me}

dform str_module_df2 : str_module[name:s, start:n, finish:n]{'me} =
   str_module[name:s]{'me}

(*
 * Module type definition.
 *)
dform str_module_type_df1 : str_module_type[name:s]{'mt} =
   sig_module_type[name:s]{'mt}

dform str_module_type_df2 : str_module_type[start:n, finish:n, name:s]{'mt} =
   str_module_type[name:s]{'mt}

(*
 * Open a module in scope.
 *)
dform str_open_df1 : str_open{'sl} =
   sig_open{'sl}

dform str_open_df2 : str_open[start:n, finish:n]{'sl} =
   str_open{'sl}

(*
 * Type definition.
 *)
dform str_type_df1 : str_type{'tdl} =
   sig_type{'tdl}

dform str_type_df2 : str_type[start:n, finish:n]{'tdl} =
   str_type{'tdl}

(*
 * Value definition.
*)
declare and_let{'pel}
declare str_let{'e}

dform str_let_df1 : str_let{patt_var[s1:n, f1:n]{v. patt_in[s2:n, f2:n]{'p}}; 'e} =
   (* slot{'v} *) "=" slot{'e}

dform str_let_df2 : str_let{patt_var[s1:n, f1:n]{f. patt_done[s2:n, f2:n]}; 'p} =
   pushm[3] "_let" `" " slot{'f} `" " str_let{'p} popm

dform str_let_df3 : str_let{."fun"[s3:n, f3:n]{
                              ."patt_if"[s4:n, f4:n]{
                                ."patt_var"[s5:n, f5:n]{x.
                                  "patt_body"[s6:n, s7:n]{'p}}}}} =
   slot{'x} `" " str_let{'p}

dform str_let_df4 : str_let{'e} =
   "=" hspace szone{'e}

dform str_let_df5 : str_let[start:n, finish:n]{cons{str_let[s:n, f:n]{'p; 'e}; 'pel}} =
   szone pushm[0] str_let{'p; 'e}
   and_let{'pel}
   popm ezone

dform and_let_df1 : and_let{cons{str_let[s:n, f:n]{'p; 'e}; 'pel}} =
   popm newline pushm[3] "_and" `" " str_let{'p; 'e}
   and_let{'pel}

dform and_let_df2 : and_let{nil} =
   `""

(*
 * Fix definition.
 *)
dform str_fix_df1 : str_fix{'p} =
   szone pushm[0] "_letrec" `" " patt_format{'p; nil} popm ezone

dform str_fix_df2 : str_fix[start:n, finish:n]{'p} =
   str_fix{'p}

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
