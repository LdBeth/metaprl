(*
 * Utilities for headers.
 * ----------------------------------------------------------------
 *
 * Copyright (C) 2003 Adam Granicz, Caltech
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
 * Author: Adam Granicz
 * Email: granicz@cs.caltech.edu
 *)

open Phobos_type
open Phobos_exn

type digest = Digest.t

type header =
   { version      : string;
     digest       : digest;
     timestamp    : float;
     sizestamp    : int
   }

(* Version *)
let version_string = "pbf-1.0"

(* Digests *)
let create_digest = Digest.string

(* Headers *)
let version_of_header header = header.version
let digest_of_header header = header.digest
let timestamp_of_header header = header.timestamp
let sizestamp_of_header header = header.sizestamp

let create_header digest time size =
   { version      = version_string;
     digest       = digest;
     timestamp    = time;
     sizestamp    = size
   }
