/*
 * An endpoint is allowed to send/receive data on the bus.
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

interface NuprlBusEndpoint
{
    /**
     * Host of this connection.
     */
    String getHost();

    /**
     * Port of this connection.
     */
    int getPort();

    /**
     * Convert to a port.
     */
    NuprlBusPort getBusPort();

    /**
     * Get the next message that arrived at this endpoint.
     */
    NuprlBusMessage getMessage();

    /**
     * Forward a message.
     */
    void Forward(NuprlBusMessage msg)
        throws NuprlException;

    /**
     * Send a message to a particular endpoint.
     */
    void Send(NuprlBusPort dst, NuprlToken token);

    /**
     * Send a message on the channel.
     */
    void Send(String dst_host, int dst_port, NuprlToken token);

    /**
     * Broadcast a message to all ports on a host.
     */
    void Cast(String dst_host, NuprlToken token);

    /**
     * Broadcast a message to all ports.
     */
    void Cast(NuprlToken token);

    /**
     * Unsubscribe from the channel.
     */
    void Unsubscribe();
}
