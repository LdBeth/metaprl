/**
 * Describe the context:
 *    Whether we are a client or an applet
 */

package edu.cornell.cs.jyh.nuprl;

import java.awt.*;
import java.io.*;

import javax.swing.*;

interface NuprlContext
{
    // Flags for connection mode
    static final int APPLET_MODE        = 0;
    static final int TELNET_MODE        = 1;
    int getMode();

    // Functions to get resources
    JDesktopPane getDesktop();
    Image getImage(String name);
    InputStream getFile(String name);
}
