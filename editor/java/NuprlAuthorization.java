/*
 * The authorization contains our username,
 * the machine we want to connect to,
 * and the name we want to be known by.
 */

package edu.cornell.cs.jyh.nuprl;

class NuprlAuthorization
{
    /**
     * Info about the connection.
     */
    String host;
    String user;
    String password;
    int port;

    /**
     * Do we have enough info for a simple connection?
     */
    boolean isAuthComplete()
    {
        return host != null && user != null && password != null;
    }

    /**
     * Enough info for just a host connection?
     */
    boolean isHostComplete()
    {
        return host != null;
    }
}
