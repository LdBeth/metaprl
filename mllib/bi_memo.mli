(* This file is part of Nuprl-Light, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/index.html for information on Nuprl,
 * OCaml, and more information about this system.
 *
 * Copyright (C) 1998 Yegor Bryukhov, Moscow State University
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
 * Author: Yegor Bryukhov
 *)

open Simplehash_sig

module ExtBiMemo (Hash: SimpleHashSig) :
sig

	type ('params, 'domain, 'd2i, 'i2d, 'image) t

	val create :
                ('params -> 'domain -> 'd2i) ->
		('params -> 'd2i -> 'image) ->
		('d2i -> 'd2i -> bool) ->
		('params -> 'image -> 'i2d) ->
		('params -> 'i2d -> 'domain) ->
		('i2d -> 'i2d -> bool) ->
		('params, 'domain, 'd2i, 'i2d, 'image) t

	val apply :
		('params, 'domain, 'd2i, 'i2d, 'image) t ->
		'params -> 'domain -> 'image

	val invert :
		('params, 'domain, 'd2i, 'i2d, 'image) t ->
		'params -> 'image -> 'domain

	val revert :
                ('param, 'domain, 'd2i, 'i2d, 'image) t ->
                ('param, 'image, 'i2d, 'd2i, 'domain) t

	val extr : ('params, 'domain, 'd2i, 'i2d, 'image) t -> ( ( ('domain * 'image) list array * int ) * ( ('d2i * 'image) list array * int ) * 
	                                                        ( ('image * 'domain) list array * int ) * ( ('i2d * 'domain) list array * int ) )

end







