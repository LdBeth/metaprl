/*
 * A message has a src host, port, and a token.
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
 * Copyright (C) 1999 Jason Hickey, Cornell University
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
 */

package edu.cornell.cs.jyh.nuprl;

class NuprlBusMessage
{
    /**
     * Originating/destination host.
     */
    String src_host;

    /**
     * Originating/destination port.
     */
    int src_port;

    /**
     * Destination host.
     */
    String dst_host;

    /**
     * Destination port.
     */
    int dst_port;

    /**
     * Payload.
     */
    NuprlToken token;

    /**
     * Create a new message.
     */
    NuprlBusMessage(String src_host, int src_port, String dst_host, int dst_port, NuprlToken token)
    {
        this.src_host = src_host;
        this.src_port = src_port;
        this.dst_host = dst_host;
        this.dst_port = dst_port;
        this.token = token;
    }
}
