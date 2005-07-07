/*
 * A data token has a content string.
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

class NuprlDataToken
extends NuprlToken
{
    /**
     * This is the data.
     */
    protected byte[] data;

    /**
     * May be a linked list.
     */
    protected NuprlDataToken next;

    /**
     * Single byte token.
     */
    NuprlDataToken(byte b)
    {
        data = new byte[1];
        data[0] = b;
    }

    /**
     * Create from a byte array.
     */
    NuprlDataToken(byte[] bytes, int off, int len)
    {
        data = new byte[len];
        System.arraycopy(bytes, off, data, 0, len);
    }

    /**
     * Create from a string.
     */
    NuprlDataToken(String s)
    {
        this.data = s.getBytes();
    }

    /**
     * Merge with another data token.
     */
    void append(NuprlDataToken token)
    {
        NuprlDataToken head = this;
        while(head.next != null)
            head = head.next;
        head.next = token;
    }

    /**
     * How many bytes available?
     */
    int available()
    {
        int count = 0;
        for(NuprlDataToken head = this; head != null; head = head.next)
            count += head.data.length;
        return count;
    }

    /**
     * Get the data.
     */
    byte[] getData()
    {
        if(next != null) {
            // Count up the entire buffer
            int length = 0;
            for(NuprlDataToken head = this; head != null; head = head.next)
                length += head.data.length;

            // Now copy into a new buffer
            int offset = 0;
            byte[] buffer = new byte[length];
            for(NuprlDataToken head = this; head != null; head = head.next) {
                byte[] fromdata = head.data;
                System.arraycopy(fromdata, 0, buffer, offset, fromdata.length);
                offset += fromdata.length;
            }
            data = buffer;
            next = null;
        }

        return data;
    }
}

