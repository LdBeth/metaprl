/**
 * This class breaks up the output from an OutputStream into tokens.
 *
 * This simulates outtext.ml
 *
 * Interface:
 *    OutText(OutStream): create the buffer
 *    Raw(String s): print the string on the output
 *    Put(String s): print the string, quoting it if necessary
 *    Put(int i): print an int
 *    Put(double x): print a floating point value
 *    NL(): print a newline
 *    Term(): print a terminator (a "." on a line by itself).
 */

import java.io.*;

public class OutText
extends PrintWriter
implements Marshalable
{
    /**
     * Keep a falg if we are at beginning of line.
     */
    protected boolean bol;

    /**
     * Traverse the object.
     */
    public void Marshal(MarshalInfo info)
    {
        info.Marshal(bol);
    }

    /**
     * Need an OutputStream
     */
    OutText(OutputStream out)
    {
        super(out);
        bol = true;
    }

    /*
     * A string should be escaped if it contains a quote or white space.
     */
    public static boolean EscapeP(String s)
    {
        int length = s.length();
        if(length == 0)
            return true;
        while(--length >= 0) {
            char c = s.charAt(length);
            if(c == '"' || c == '\'' || c == '\\' || Character.isWhitespace(c))
                return true;
        }
        return false;
    }

    /*
     * Escape a string.
     */
    public static String Escape(String s)
    {
        int length = s.length();
        StringBuffer buffer = new StringBuffer(2 * length);
        buffer.append('"');
        for(int i = 0; i != length; i++) {
            char c = s.charAt(i);
            switch(c) {
            case '"':
                buffer.append('\\');
                buffer.append('"');
                break;
            case '\'':
                buffer.append('\\');
                buffer.append('\'');
                break;
            case '\n':
                buffer.append('\\');
                buffer.append('n');
                break;
            case '\r':
                buffer.append('\\');
                buffer.append('r');
                break;
            case '\\':
                buffer.append('\\');
                buffer.append('\\');
                break;
            default:
                buffer.append(c);
                break;
            }
        }
        buffer.append('"');
        return buffer.toString();
    }

    /*
     * Tell if at EOF.
     */
    public boolean EOF()
    {
        return checkError();
    }

    /*
     * Add white space if not at bol.
     */
    public void Space()
    {
        if(bol)
            bol = false;
        else
            print(' ');
    }

    /*
     * Print a string.
     */
    public void Raw(String s)
    {
        Space();
        print(s);
    }
            
    /*
     * Put a string, quoting it if necessary.
     */
    public void Put(String s)
    {
        Space();
        if(EscapeP(s))
            print(Escape(s));
        else
            print(s);
    }

    /*
     * Put an integer.
     */
    public void Put(int i)
    {
        Space();
        print(i);
    }

    /*
     * A floating point number.
     */
    public void Put(double x)
    {
        Space();
        print(x);
    }

    /*
     * A newline.
     */
    public void NL()
    {
        println();
        flush();
        bol = true;
    }

    /*
     * A termination is a '.' on a line by itself.
     */
    public void Term()
    {
        if(bol == false)
            println();
        println(".");
        bol = true;
    }
}

