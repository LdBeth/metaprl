(*
 * Generate a magic number for a file.
 * We do this by taking an MD5 digest,
 * then converting the first 4 chars to an int.
 *
 * The output is in the form of a let-expression.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2004 Mojave Group, Caltech
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
 * @email{jyh@cs.caltech.edu}
 * @end[license]
 *)
open Printf

(*
 * Compute a magic number for a file.
 *)
let genmagic filename =
   let digest = Digest.file filename in
      printf "0x%s\n" (Digest.to_hex digest)

(*
 * Compute magic numbers for all the files.
 *)
let main () =
   Arg.parse [] genmagic "Compute magic numbers for files";
   flush stdout

let _ =
   Printexc.catch main ()

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
