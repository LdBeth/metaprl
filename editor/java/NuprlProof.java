/*
 * This is an internal window for editing.
 */
package edu.cornell.cs.jyh.nuprl;

import javax.swing.*;
import javax.swing.event.*;

public class NuprlProof
extends NuprlInternalFrame
implements NuprlFrame
{
    /*
     * Windows.
     */
    NuprlTerm goal;
    NuprlText rule;
    NuprlTerm subgoals;

    /************************************************************************
     * IMPLEMENTATION                                                       *
     ************************************************************************/

    /*
     * Create the window.
     */
    public NuprlProof(NuprlBus bus, int port)
        throws NuprlException
    {
        // Some initial setup
        super("MetaPRL proof window", true, true, true, true);

        // Create the windows
        goal = new NuprlTerm(this, bus, NuprlConstants.GOAL_NAME, port);
        rule = new NuprlText(bus, NuprlConstants.RULE_NAME, port, false, this);
        subgoals = new NuprlTerm(new NullFrame(), bus, NuprlConstants.SUBGOALS_NAME, port);

        // Add them in splittable panes
        JSplitPane rule_subgoals = new JSplitPane(JSplitPane.VERTICAL_SPLIT, rule, subgoals);
        JSplitPane goal_rule_subgoals = new JSplitPane(JSplitPane.VERTICAL_SPLIT, goal, rule_subgoals);
        getContentPane().add(goal_rule_subgoals);

        // Watch for window closings
        addInternalFrameListener(new NuprlProofListener());

        pack();
        setVisible(true);
        goal_rule_subgoals.setVisible(true);
        rule_subgoals.setVisible(true);
    }

    /**
     * Null frame ignores title requests.
     */
    class NullFrame
    implements NuprlFrame
    {
        public void setTitle(String s)
        {
        }
    }

    /**
     * When the frame closes, remove the ports.
     */
    class NuprlProofListener
        extends InternalFrameAdapter
    {
        public void internalFrameClosed(InternalFrameEvent e)
        {
            goal.Close();
            rule.Close();
            subgoals.Close();
        }
    }

    /**
     * Set the client window.
     */
    void setClient(NuprlBusPort port)
    {
        goal.setClient(port);
        rule.setClient(port);
        subgoals.setClient(port);
    }
}
