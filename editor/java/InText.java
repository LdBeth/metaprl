/**
 * This class breaks up the input from an InputStream into tokens.
 */

import java.io.*;
import StringTokenizer;

public class InText
extends BufferedReader
{
    /**
     * No size.
     */
    public void Marshal(MarshalInfo info)
    {
    }

    /**
     * Need an InputStream.
     */
    InText(InputStream inx)
    {
        super(new InputStreamReader(inx));
    }

    /**
     * Return tokens for the next line.
     */
    public String[] GetArgs()
        throws IOException
    {
        return StringTokenizer.ParseArgs(readLine());
    }

    /**
     * Tokenize only part of the line.
     */
    public String[] GetArgs(int args)
        throws IOException
    {
        return StringTokenizer.ParseArgs(readLine(), args);
    }
}
