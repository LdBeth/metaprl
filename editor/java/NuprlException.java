/**
 * This is the generic exception type we use in this applet.
 */

package edu.cornell.cs.jyh.nuprl;

class NuprlException
extends Exception
{
    StringBuffer info;

    NuprlException()
    {
        info = new StringBuffer();
    }
    
    NuprlException(String s)
    {
        this();
        info.append(s);
    }

    NuprlException(String s1, String s2)
    {
        this();
        info.append(s1);
        info.append(s2);
    }

    NuprlException(int code, String s)
    {
        info.append("Code(");
        info.append(code);
        info.append(") ");
        info.append(s);
    }

    String stringOfException()
    {
        return info.toString();
    }
}
