/*
 * This is an internal window for editing.
 */
package edu.cornell.cs.jyh.nuprl;

import javax.swing.*;
import javax.swing.event.*;

public class NuprlMenu
extends NuprlInternalFrame
implements NuprlFrame
{
    /**
     * The actual window.
     */
    protected NuprlTerm menu;

    /*
     * Create the window.
     */
    NuprlMenu(NuprlBus bus, int port)
        throws NuprlException
    {
        // Some initial setup
        super("MetaPRL menu", true, false, false, true);
        menu = new NuprlTerm(this, bus, NuprlConstants.MENU_NAME, port);
        getContentPane().add(menu);

        // Show the process number
        setTitle("NuprlMenu [" + port + "]");

        // Watch for window closings
        addInternalFrameListener(new NuprlMenuListener());

        pack();
        setVisible(true);
    }

    /**
     * When the window is closed, disconnect the menu.
     */
    class NuprlMenuListener
        extends InternalFrameAdapter
    {
        public void internalFrameClosed(InternalFrameEvent e)
        {
            menu.Close();
        }
    }

    /*
     * Set the client.
     */
    void setClient(NuprlBusPort client)
    {
        menu.setClient(client);
    }
}
