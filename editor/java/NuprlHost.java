/**
 * This is a hostname/username/password form.
 */

package edu.cornell.cs.jyh.nuprl;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;
import javax.swing.text.*;

class NuprlHost
extends NuprlInternalFrame
implements ActionListener
{
    /*
     * Four fields for loggin in.
     */
    Document hostname;
    Document portname;

    /*
     * Store the results in the authorization.
     */
    NuprlAuthorization auth;

    /*
     * Target for logging in.
     */
    protected Target login_target;
    protected String login_command;

    /**
     * Handle button click.
     */
    public void actionPerformed(ActionEvent e)
    {
        if(login_target != null) {
            auth.host = getText(hostname);
            try {
                auth.port = Integer.parseInt(getText(portname));
            }
            catch(NumberFormatException exn) {
                // Ignore it
            }
            login_target.performCommand(login_command, auth);
        }
    }

    /**
     * Get the text from one of the documents.
     */
    protected String getText(Document doc)
    {
        String text;

        try {
            text = doc.getText(0, doc.getLength());
        }
        catch(BadLocationException e) {
            text = "";
        }
        return text;
    }

    /**
     * Set the button target.
     */
    void setTarget(String command, Target target)
    {
        login_command = command;
        login_target = target;
    }

    /**
     * Initialize the window.
     */
    NuprlHost(NuprlAuthorization auth)
    {
        // Some initial setup
        super("Host connection", true, false, false, true);
        JComponent component = (JComponent) getContentPane();
        component.setLayout(new BoxLayout(component, BoxLayout.Y_AXIS));
        this.auth = auth;

        // Sizes
        Dimension labelSize = new Dimension(100, 25);
        Dimension fieldSize = new Dimension(300, 25);

        // Host field
        JLabel hostLabel = new JLabel("Hostname:");
        JTextField hostField = new JTextField(auth.host);
        hostLabel.setPreferredSize(labelSize);
        hostField.setPreferredSize(fieldSize);
        hostField.addActionListener(this);
        hostname = hostField.getDocument();
        Box hostBox = new Box(BoxLayout.X_AXIS);
        hostBox.add(hostLabel);
        hostBox.add(hostField);
        component.add(hostBox);

        JLabel portLabel = new JLabel("Port:");
        JTextField portField = new JTextField(Integer.toString(auth.port));
        portLabel.setPreferredSize(labelSize);
        portField.setPreferredSize(fieldSize);
        portname = hostField.getDocument();
        Box portBox = new Box(BoxLayout.X_AXIS);
        portBox.add(portLabel);
        portBox.add(portField);
        component.add(portBox);

        // Connect button
        JButton connectButton = new JButton("Connect");
        connectButton.addActionListener(this);
        component.add(connectButton);

        pack();
        setVisible(true);
    }
}
