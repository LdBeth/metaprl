/*
 * This breaks a string into tokens.
 * This is a little better than java.util.StringTokenizer because
 * We handle quoted strings.
 *
 * This is used to implement getargs in InText.
 *
 * The difference from the TextArea is that we capture the <nl>
 * character and transmit the char to the socket.
 */

import java.util.Vector;

/**
 * String tokenizer.
 *
 * This tokenizer is a little more powerfule the java.util.StringTokenizer
 * because it can handle quoted tokens.
 *
 * Interface:
 *    StringTokenizer(String s): read tokens from s
 *    boolean AtEol(): no more tokens left
 *    String NextToken(): get next token
 *    String[] ParseTokens(): gett all the tokens
 *    static String[] ParseArgs(String s): get all the tokens
 */
public class StringTokenizer
implements Marshalable
{
    /**
     * Remember the string and the current position.
     */
    int index, length;
    String str;
    Character split;

    /**
     * Traverse the object.
     */
    public void Marshal(MarshalInfo info)
    {
        info.Marshal(index);
        info.Marshal(length);
        info.Marshal(str);
        info.Marshal(split);
    }

    /**
     * Create a tokenizer from a string.
     * Start from the front.
     */
    StringTokenizer(String s)
    {
        str = s;
        index = 0;
        length = s.length();
    }

    /**
     * String tokenizer from a specific char
     */
    StringTokenizer(String s, char c)
    {
        this(s);
        split = new Character(c);
    }

    /**
     * Test if a char is a space or not
     */
    private boolean isWhitespace(char c)
    {
        boolean rval;

        if(split == null)
            rval = Character.isWhitespace(c);
        else
            rval = c == split.charValue();
        return rval;
    }

    /**
     * Return the index of the first nonwhite char.
     */
    private void SkipWhite()
    {
        while(index != length) {
            if(isWhitespace(str.charAt(index)) == false)
                break;
            index++;
        }
    }

    /**
     * Read a quoted string.
     * Guaranteed the first char is a double quote.
     */
    private String ReadQuotedString()
    {
        StringBuffer buffer = new StringBuffer();

        // Skip initial quote
        index++;

        // Collect until next quote
    loop:
        while(index != length) {
            char c = str.charAt(index);
            switch(c) {
            case '"':
                index++;
                break loop;
            case '\\':
                if(index < length - 1)
                    index++;
                c = str.charAt(index);
                switch(c) {
                case 't':
                    c = '\t';
                    break;
                case 'r':
                    c = '\r';
                    break;
                case 'n':
                    c = '\n';
                    break;
                }
                buffer.append(c);
                break;
            default:
                buffer.append(c);
                break;
            }
            index++;
        }
        
        // Create the string
        return buffer.toString();
    }

    /**
     * Read until the next white space.
     */
    private String ReadString()
    {
        int start = index;
        while(index != length) {
            if(isWhitespace(str.charAt(index)))
                break;
            index++;
        }
        return str.substring(start, index);
    }

    /**
     * Read until the end of line.
     */
    private String ReadRest()
    {
        int start = index;
        index = length;
        return str.substring(start, length);
    }

    /**
     * See if at eol.
     */
    public boolean AtEol()
    {
        SkipWhite();
        return index == length;
    }

    /**
     * Read the next token.
     */
    public String NextToken()
    {
        String arg;

        // Skip white, then read one of the token types
        SkipWhite();
        if(index == length)
            return "";
        char c = str.charAt(index);
        if(c == '"')
            arg = ReadQuotedString();
        else
            arg = ReadString();
        return arg;
    }
        
    /**
     * Parse the entire line.
     */
    public String[] ParseTokens(int count)
    {
        Vector argv = new Vector(10);
        String arg;
        int i, argc;

        // Loop until all tockens are collected
        argc = 0;
        while(index != length) {
            SkipWhite();
            if(index == length)
                break;
            if(argc == count) {
                // The last argument takes the rest of the line
                arg = ReadRest();
            }
            else {
                // Determine if a quoted argument
                char c = str.charAt(index);
                if(c == '"')
                    arg = ReadQuotedString();
                else
                    arg = ReadString();
            }
            argv.addElement(arg);
            argc++;
        }
        
        // Now convert to a string array
        String[] args = new String[argc];
        for(i = 0; i != argc; i++)
            args[i] = (String) argv.elementAt(i);
        return args;
    }

    /**
     * Turn the line into a list of words.
     */
    public static String[] ParseArgs(String command)
    {
        StringTokenizer token = new StringTokenizer(command);
        return token.ParseTokens(-1);
    }

    /**
     * Turn the line into a list of words,
     * split by the character in the argument.
     */
    public static String[] ParseArgs(String command, char c)
    {
        StringTokenizer token = new StringTokenizer(command, c);
        return token.ParseTokens(-1);
    }

    /**
     * Turn the line into a list of words.
     * Expect argc normal arguments, the collect the rest as a single arg.
     */
    public static String[] ParseArgs(String command, int argc)
    {
        StringTokenizer token = new StringTokenizer(command);
        return token.ParseTokens(argc);
    }
}

/*
 * $Log$
 * Revision 1.1  1998/02/05 15:49:26  jyh
 * This is a simple term display in an applet.
 *
 * Revision 1.6  1997/12/15 22:16:57  jyh
 * First working version using ocaml-1.07
 *
 * Revision 1.5  1997/12/15 15:26:22  jyh
 * Upgrading to ocaml-1.07.
 *
 * Revision 1.4  1997/11/24 15:54:09  jyh
 * Fixed some glitch code.
 *
 * Revision 1.3  1997/10/27 15:08:15  jyh
 * First working version of Java EJB.
 *
 * Revision 1.2  1997/08/28 14:34:06  jyh
 * Java interface.
 *
 * Revision 1.1  1997/07/25 19:12:05  jyh
 * Raw Java version.
 *
 */
