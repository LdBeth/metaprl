/**
 * This is a hostname/username/password form.
 */

package edu.cornell.cs.jyh.nuprl;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;
import javax.swing.text.*;

class NuprlLogin
extends NuprlInternalFrame
implements ActionListener
{
    /*
     * Four fields for loggin in.
     */
    Document hostname;
    Document username;
    Document password;

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
            auth.user = getText(username);
            auth.password = getText(password);
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
    NuprlLogin(NuprlAuthorization auth)
    {
        // Some initial setup
        super("Login window", true, false, false, true);
        JComponent component = (JComponent) getContentPane();
        component.setLayout(new BoxLayout(component, BoxLayout.Y_AXIS));
        this.auth = auth;

        // Sizes
        Dimension labelSize = new Dimension(100, 25);
        Dimension fieldSize = new Dimension(300, 25);

        // Host field
        JLabel hostLabel = new JLabel("Hostname");
        JTextField hostField = new JTextField(auth.host);
        hostLabel.setPreferredSize(labelSize);
        hostField.setPreferredSize(fieldSize);
        hostname = hostField.getDocument();
        Box hostBox = new Box(BoxLayout.X_AXIS);
        hostBox.add(hostLabel);
        hostBox.add(hostField);
        component.add(hostBox);

        JLabel userLabel = new JLabel("Username");
        JTextField userField = new JTextField(auth.user);
        userLabel.setPreferredSize(labelSize);
        userField.setPreferredSize(fieldSize);
        username = hostField.getDocument();
        Box userBox = new Box(BoxLayout.X_AXIS);
        userBox.add(userLabel);
        userBox.add(userField);
        component.add(userBox);

        JLabel passLabel = new JLabel("Password");
        JPasswordField passField = new JPasswordField(auth.password);
        passLabel.setPreferredSize(labelSize);
        passField.setPreferredSize(fieldSize);
        password = passField.getDocument();
        Box passBox = new Box(BoxLayout.X_AXIS);
        passBox.add(passLabel);
        passBox.add(passField);
        component.add(passBox);

        // Connect button
        JButton connectButton = new JButton("Connect");
        connectButton.addActionListener(this);
        component.add(connectButton);

        pack();
        setVisible(true);
    }
}
