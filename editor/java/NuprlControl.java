/*
 * This class handles hyperlink events.
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

import javax.swing.*;

class NuprlControl
{
    /**
     * Our endpoint on the bus.
     */
    protected NuprlBus bus;
    protected NuprlBusEndpoint endpoint;
    protected NuprlClient client;

    /**
     * Add ourselves to the bus.
     */
    NuprlControl(NuprlBus bus, int port)
        throws NuprlException
    {
        this.bus = bus;
        this.endpoint = bus.Subscribe(new NuprlControlClient());
    }

    /************************************************************************
     * INPUT HANDLING                                                       *
     ************************************************************************/

    /**
     * Data is collected in a buffer.
     */
    protected void HandleControl(NuprlControlToken token)
    {
    }

    /**
     * Handle commands.
     */
    protected void HandleCommand(NuprlBusMessage msg, NuprlCommandToken token)
    {
        if(client != null) {
            endpoint.Send(client.getHost(), client.getPort(),
                          new NuprlDataToken(NuprlConstants.COMMAND_NAME
                                             + " " + msg.src_port
                                             + " " + token.command + "\n"));
        }
    }

    /**
     * When a URL arrives, we create a new client.
     */
    protected void HandleURL(NuprlURLToken token)
    {
        switch(token.command) {
        case NuprlURLToken.PASSWORD:
            // Start a new client
            NuprlAuthorization auth = new NuprlAuthorization();
            auth.host = token.url.getHost();
            auth.port = token.url.getPort();
            auth.password = token.url.getFile();
            try {
                System.err.println("Connecting to " + auth.host + ":" + auth.port);
                client = new NuprlClient(bus, auth, endpoint.getBusPort());
                endpoint.Send(client.getHost(), client.getPort(), new NuprlDataToken("connect " + auth.password + "\n"));
            }
            catch(NuprlException e) {
                System.err.println("Error on connection to " + auth.host + ": " + e.stringOfException());
            }
            break;
        }
    }

    /**
     * We handle HTML and PRL events.
     */
    protected void Poll(NuprlBusEndpoint endpt)
    {
        NuprlBusMessage msg;
        while((msg = endpt.getMessage()) != null) {
            NuprlToken token = msg.token;
            if(token instanceof NuprlControlToken)
                HandleControl((NuprlControlToken) token);
            else if(token instanceof NuprlCommandToken)
                HandleCommand(msg, (NuprlCommandToken) token);
            else if(token instanceof NuprlURLToken)
                HandleURL((NuprlURLToken) token);
        }
    }

    /**
     * Handle an event in the regular window queue.
     */
    class NuprlControlEvent
        implements Runnable
    {
        NuprlBusEndpoint endpt;

        NuprlControlEvent(NuprlBusEndpoint endpt)
        {
            this.endpt = endpt;
        }

        public void run()
        {
            Poll(endpt);
        }
    }

    /**
     * This is the client interface.
     */
    class NuprlControlClient
        implements NuprlBusClient
    {
        /**
         * Our hostname.
         */
        public String getHost()
        {
            return NuprlConstants.CONTROL_NAME;
        }

        /**
         * Handle a wakeup from the bus.
         */
        public void wakeup(NuprlBusEndpoint endpt)
        {
            SwingUtilities.invokeLater(new NuprlControlEvent(endpt));
        }
    }
}
