/*
 * This is an internal window for editing.
 */
package edu.cornell.cs.jyh.nuprl;

import javax.swing.*;
import javax.swing.event.*;

class NuprlCommand
extends NuprlInternalFrame
implements NuprlFrame
{
    /**
     * This is just a wrapper for a NuprlText window.
     */
    protected NuprlText textPane;

    /*
     * Create the window.
     */
    NuprlCommand(NuprlBus bus, int port)
        throws NuprlException
    {
        // Some initial setup
        super("ML command window", true, false, true, true);

        // Create the text pane and configure it.
        textPane = new NuprlText(bus, NuprlConstants.COMMAND_NAME, port, true, this);

        // Set up the menu bar.
        JMenu editMenu = textPane.createEditMenu();
        JMenuBar mb = new JMenuBar();
        mb.add(editMenu);
        setJMenuBar(mb);

        // Now add the window
        getContentPane().add(textPane);
        pack();

        // Watch for window closings
        addInternalFrameListener(new NuprlCommandListener());
    }

    /**
     * When the frame closes, remove the text port.
     */
    class NuprlCommandListener
        extends InternalFrameAdapter
    {
        public void internalFrameClosed(InternalFrameEvent e)
        {
            textPane.Close();
        }
    }

    /*
     * Set the client.
     */
    void setClient(NuprlBusPort client)
    {
        textPane.setClient(client);
    }
}
