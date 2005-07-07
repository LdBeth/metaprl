/*
 * A standard representation of sub-negoatiation data.
 *
 * ----------------------------------------------------------------
 *
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/htmlman/default.html or visit http://metaprl.org/
 * for more information.
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

class NuprlOptionSBToken
extends NuprlOptionBlockToken
{
    /**
     * This is the data.
     */
    byte[] data;

    /**
     * Build it.
     */
    NuprlOptionSBToken(byte[] data, int off, int len)
    {
        super(data.length != 0 ? data[0] : 0);
        this.data = new byte[len];
        for(int i = 0; i != len; i++)
            this.data[i] = data[i + off];
    }

    /**
     * Build it.
     */
    NuprlOptionSBToken(byte[] data)
    {
        super(data.length != 0 ? data[0] : 0);
        this.data = data;
    }

    /**
     * Format it.
     */
    byte[] formatData()
    {
        // Count total number of bytes needed
        int count = 0;
        for(int i = 0; i != data.length; i++) {
            if(data[i] == NuprlConstants.TELNET_IAC)
                count++;
            count++;
        }

        // Quick format
        if(count == data.length)
            return data;

        // Escape the IAC
        byte[] escaped = new byte[count];
        count = 0;
        for(int i = 0; i != data.length; i++) {
            if(data[i] == NuprlConstants.TELNET_IAC)
                escaped[count++] = NuprlConstants.TELNET_IAC;
            escaped[count++] = data[i];
        }
        return escaped;
    }
}
