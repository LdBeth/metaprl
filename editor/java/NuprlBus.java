/*
 * This is a thread-safe communication channel, with
 * boradcast support.
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

import java.util.*;

import javax.swing.*;

class NuprlBus
{
    /**
     * Wakeup event.
     */
    /**
     * The client manager only gets messages when the client connection
     * fails.  If it does, we have to back off to the connection state.
     * We can drop the messages.
     */
    class NuprlEvent
        implements Runnable
    {
        Endpoint endpt;

        NuprlEvent(Endpoint endpt)
        {
            this.endpt = endpt;
        }

        public void run()
        {
            endpt.client.wakeup(endpt);
        }
    }

    /**
     * Linked list of clients.
     */
    class Endpoint
        implements NuprlBusEndpoint, NuprlBusPort
    {
        /**
         * The host at this endpoint.
         */
        String host;

        /**
         * The port of the endpoint.
         */
        int port;

        /**
         * The client information.
         */
        NuprlBusClient client;

        /**
         * A queue of outgoing messages.
         */
        Vector messages;

        /**
         * Linked list.
         */
        Endpoint next;

        /**
         * Create.
         */
        Endpoint(String host, int port, NuprlBusClient client)
        {
            this.host = host;
            this.port = port;
            this.client = client;
            this.messages = new Vector();
        }

        /**
         * Append a message to the queue, and ask the client to wake up.
         * For efficiency, this is called only from synchronized methods
         * in the NuprlBus.
         */
        void append(NuprlBusMessage msg)
        {
            messages.add(msg);
            SwingUtilities.invokeLater(new NuprlEvent(this));
        }

        /**
         * Get the host port.
         */
        public String getHost()
        {
            return host;
        }

        /**
         * Get the port.
         */
        public int getPort()
        {
            return port;
        }

        /**
         * Coerce this object.
         */
        public NuprlBusPort getBusPort()
        {
            return this;
        }

        /**
         * Get the next message.  For efficiency, we 
         * synchronize vector operations with the NuprlBus.
         */
        public NuprlBusMessage getMessage()
        {
            NuprlBusMessage message = null;
            // System.err.println(host + ": waiting");
            synchronized (NuprlBus.this) {
                // System.err.println(host + ": done");
                if(!messages.isEmpty()) {
                    message = (NuprlBusMessage) messages.firstElement();
                    messages.remove(0);
                }
            }
            return message;
        }

        /**
         * Forward a message.
         */
        public void Forward(NuprlBusMessage msg)
            throws NuprlException
        {
            NuprlBus.this.Forward(host, port, msg);
        }

        /**
         * Send a message to a specific destination.
         */
        public void Send(NuprlBusPort dst, NuprlToken token)
        {
            NuprlBus.this.Send(host, port, dst.getHost(), dst.getPort(), token);
        }

        /**
         * Send a message to a specific destination.
         */
        public void Send(String dst_host, int dst_port, NuprlToken token)
        {
            NuprlBus.this.Send(host, port, dst_host, dst_port, token);
        }

        /**
         * Broadcast a message to all ports on a host.
         */
        public void Cast(String dst_host, NuprlToken token)
        {
            NuprlBus.this.Cast(host, port, dst_host, token);
        }
    
        /**
         * Broadcast to all ports on all hosts.
         */
        public void Cast(NuprlToken token)
        {
            NuprlBus.this.Cast(host, port, token);
        }

        /**
         * Unsubscribe from the bus.
         */
        public void Unsubscribe()
        {
            NuprlBus.this.Unsubscribe(this);
        }
    }

    /**
     * Each host has a manager, and a list of clients.
     * We keep a linked list.
     */
    class Manager
    {
        /**
         * Host name.
         */
        String host;

        /**
         * The last port number to be allocated.
         */
        int port;

        /**
         * Manager client.
         */
        Endpoint manager;

        /**
         * The rest of the clients.
         */
        Endpoint clients;

        /**
         * Next link.
         */
        Manager next;

        /**
         * Create a new manager.
         */
        Manager(Endpoint endpt, Manager next)
        {
            this.host = endpt.host;
            this.manager = endpt;
            this.next = next;
            this.port = 0;
        }
    }

    /************************************************************************
     * ENDPOINT SEARCHING                                                   *
     ************************************************************************/

    /**
     * List of all the hosts.
     */
    protected Manager manager;

    /**
     * Search for a specific endpoint.
     */
    protected Endpoint findEndpoint(Manager manager, int port)
    {
        for(Endpoint head = manager.clients; head != null; head = head.next) {
            if(head.port == port)
                return head;
        }
        return null;
    }

    /**
     * Replace an endpoint.
     */
    protected void replaceEndpoint(Manager manager, Endpoint endpt)
    {
        Endpoint head = manager.clients;
        Endpoint prev = null;
        while(head != null) {
            if(head.port == endpt.port) {
                if(prev == null)
                    manager.clients = endpt;
                else
                    prev.next = endpt;
                endpt.next = head.next;
                return;
            }
            prev = head;
            head = head.next;
        }

        // Not found
        endpt.next = manager.clients;
        manager.clients = endpt;
    }

    /**
     * Remove an endpoint.
     */
    protected void removeEndpoint(Manager manager, int port)
    {
        Endpoint head = manager.clients;
        Endpoint prev = null;

        while(head != null) {
            if(head.port == port) {
                if(prev == null)
                    manager.clients = head.next;
                else
                    prev.next = head.next;
                return;
            }

            prev = head;
            head = head.next;
        }
    }
    
    /**
     * Find the manager with this hostname.
     */
    protected Manager findManager(String host)
    {
        for(Manager head = manager; head != null; head = head.next) {
            if(head.host.equals(host))
                return head;
        }
        return null;
    }

    /**
     * Find a client by its host and port.
     * Return the manager if the specific client can't be found.
     */
    protected Endpoint findEndpoint(String host, int port)
    {
        Manager manager = findManager(host);
        if(manager == null)
            return null;
        Endpoint endpt = findEndpoint(manager, port);
        return endpt == null ? manager.manager : endpt;
    }

    /************************************************************************
     * IMPLEMENTATION                                                       *
     ************************************************************************/

    /**
     * Create the bus.
     */
    NuprlBus()
    {
    }

    /**
     * Set the new manager.
     */
    synchronized NuprlBusEndpoint Manage(NuprlBusClient client)
    {
        String host = client.getHost();
        Endpoint endpt = new Endpoint(host, 0, client);
        Manager manager = findManager(host);
        if(manager == null) {
            manager = new Manager(endpt, this.manager);
            this.manager = manager;
        }
        else
            manager.manager = endpt;
        return endpt;
    }

    /**
     * Add a new endpoint.
     */
    synchronized NuprlBusEndpoint Subscribe(NuprlBusClient client)
        throws NuprlException
    {
        String host = client.getHost();
        Manager manager = findManager(host);
        if(manager == null)
            throw new NuprlException("NuprlBus.Subscribe: no manager for host " + host);
        Endpoint endpt = new Endpoint(host, ++manager.port, client);
        endpt.next = manager.clients;
        manager.clients = endpt;
        return endpt;
    }

    /**
     * Add the endpoint at a specific port.
     */
    synchronized NuprlBusEndpoint Subscribe(NuprlBusClient client, int port)
        throws NuprlException
    {
        String host = client.getHost();
        Manager manager = findManager(host);
        if(manager == null)
            throw new NuprlException("NuprlBus.Subscribe: no manager for host " + host);
        Endpoint endpt = new Endpoint(host, port, client);
        if(port == 0)
            manager.manager = endpt;
        else
            replaceEndpoint(manager, endpt);
        return endpt;
    }

    /**
     * Remove the endpoint.
     */
    protected synchronized void Unsubscribe(Endpoint endpt)
    {
        Manager manager = findManager(endpt.host);
        if(manager != null)
            removeEndpoint(manager, endpt.port);
    }

    /**
     * Send a message to a particular endpoint.
     * If the endpoint does not exist, send it to the manager.
     */
    protected synchronized void Send(String src_host, int src_port, String dst_host, int dst_port, NuprlToken token)
    {
        Manager manager = findManager(dst_host);
        if(manager != null) {
            NuprlBusMessage msg = new NuprlBusMessage(src_host, src_port, dst_host, dst_port, token);
            Endpoint endpt = findEndpoint(manager, dst_port);
            if(endpt == null)
                manager.manager.append(msg);
            else
                endpt.append(msg);
        }
    }

    /**
     * Forward a message.  Fail if the endpoint does not exist.
     */
    protected synchronized void Forward(String src_host, int src_port, NuprlBusMessage msg)
        throws NuprlException
    {
        if(msg.dst_host.equals(src_host) && msg.dst_port == src_port)
            throw new NuprlException("NuprlBus.Forward: forward to self refused: " + src_host + "." + src_port);
        Endpoint endpt = findEndpoint(msg.dst_host, msg.dst_port);
        if(endpt == null)
            throw new NuprlException("NuprlBus.Forward: no such host: " + msg.dst_host + "." + msg.dst_port);
        endpt.append(msg);
    }

    /**
     * Send a message to all the ports on a host.
     */
    protected synchronized void Cast(String src_host, int src_port, String dst_host, NuprlToken token)
    {
        Manager manager = findManager(dst_host);
        if(manager.clients == null)
            manager.manager.append(new NuprlBusMessage(src_host, src_port, dst_host, 0, token));
        else {
            for(Endpoint endpt = manager.clients; endpt != null; endpt = endpt.next)
                endpt.append(new NuprlBusMessage(src_host, src_port, dst_host, endpt.port, token));
        }
    }

    /**
     * Send a message to all clients.
     */
    protected synchronized void Cast(String src_host, int src_port, NuprlToken token)
    {
        System.err.println("CAST");
        for(Manager manager = this.manager; manager != null; manager = manager.next) {
            String dst_host = manager.host;
            if(manager.clients == null)
                manager.manager.append(new NuprlBusMessage(src_host, src_port, dst_host, 0, token));
            else {
                for(Endpoint endpt = manager.clients; endpt != null; endpt = endpt.next)
                    endpt.append(new NuprlBusMessage(src_host, src_port, dst_host, endpt.port, token));
            }
        }
    }
}
