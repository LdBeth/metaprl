(*
 * Interface to getrusage.
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

type rusage =
   { ru_utime : float;
     ru_stime : float;
     ru_maxrss : int;
     ru_ixrss : int;
     ru_idrss : int;
     ru_isrss : int;
     ru_minflt : int;
     ru_majflt : int;
     ru_nswap : int;
     ru_inblock : int;
     ru_oublock : int;
     ru_msgsnd : int;
     ru_msgrcv : int;
     ru_nsignals : int;
     ru_nvcsw : int;
     ru_nivcsw : int
   }

val getrusage : unit -> rusage

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
