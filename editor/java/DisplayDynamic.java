/**
 * We extend the term display with dynamic lookup.
 * This class is created with a root URL.  When a term is
 * to be looked up, and it is not found, we look in the
 * directories specified by the opname.
 */

import java.io.*;
import java.net.*;
import netscape.util.*;

/*
 * The display comtains implicitely a display engine.
 */
class DisplayDynamic
extends DisplayTerm
{
    /**
     * This is the root URL used to look up display forms.
     */
    protected URL root;

    /**
     * Root path.
     */
    protected String file;

    /**
     * Keep a hashtable of files that have been loaded.
     * string -> string hashtable:
     *        set if the argument has been checked
     */
    protected Hashtable status;

    /**
     * Create the display relative to the URL.
     */
    DisplayDynamic(URL root, String base)
    {
        try {
            this.root = new URL(root, base);
        }
        catch(MalformedURLException e) {
            this.root = root;
        }
        this.file = this.root.getFile();
        this.status = new Hashtable();
    }

    /**
     * When a term is "stuck", see if we can look up a display form.
     */
    protected void evalStuck(Closure closure, TermNuprl term, Vector results, int i)
    {
        // Collect opname parts into a string vector
        Opname opname = term.operator.opname;
        Vector vec = new Vector();
        while(opname != null) {
            vec.addElement(opname.head);
            opname = opname.tail;
        }

        // Now march backward through the opname
        String pathname = "";
        for(int j = vec.size() - 1; j > 0; j--) {
            String name = (String) vec.elementAt(j);

            // Check the HTML file for display forms
            String path = pathname + "/" + name + ".html";
            if(status.get(path) == null) {
                // Search this file for a display form (it may be a directory)
                URL url;
                try {
                    url = new URL(root, path);
                }
                catch(MalformedURLException e) {
                    url = root;
                }
                TermParser parser = new TermParser(this);
                parser.parseURL(url);
                status.put(path, path);

                // Format if term was evaulated successfully
                try {
                    if(evalMaybe(closure, term, results, i))
                        return;
                }
                catch(EvalError e) {
                    // Ignore it
                }
            }

            // Go to next directory
            pathname = pathname + "/" + path;
        }

        // Still not found, so format using default formatter
        super.evalStuck(closure, term, results, i);
    }
}        

/*
 * $Log$
 * Revision 1.1  1998/02/05 15:46:14  jyh
 * This is a simple term display in an applet.
 *
 */
