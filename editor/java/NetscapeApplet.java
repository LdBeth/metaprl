// NetscapeApplet.java
// Copyright 1996, 1997 Netscape Communications Corp.  All rights reserved.

import netscape.application.*;
import netscape.util.*;

public class NetscapeApplet extends FoundationApplet {
    /** This method must be implemented by the applet developer because
      * there is no way in the standard Java API for system classes (such as,
      * netscape.application) to look up an applet's class by name. The
      * static method <b>Class.forName()</b> simply looks up one level in the
      * stack and gets the ClassLoader associated with the method block of the
      * caller.
      * <p>
      * When the netscape.application classes are installed as
      * system classes, the ClassLoader is <b>null</b>. Thus, when code in
      * netscape.application calls <b>Class.forName()</b> it can only find
      * other system classes.
      * <p>
      * The solution is an API that allows code to
      * find the ClassLoader for an applet by URL, and a public API on
      * ClassLoader to ask it to load classes by name. Until those
      * enhancements can be made and distributed to all the world's Java
      * systems, applets must subclass FoundationApplet and
      * implement the following one-line method:
      * <pre>
      *     public abstract Class classForName(String className)
      *         throws ClassNotFoundException {
      *         return Class.forName(className);
      *     }
      * </pre>
      */
    public Class classForName(String className)
        throws ClassNotFoundException {
        return Class.forName(className);
    }
}
