/*
 * This defines a text string with an integer.
 * (Don't you wish we had tuples?)
 */
package edu.cornell.cs.jyh.nuprl;

public class ModeLine
{
    public String line;
    public int mode;

    /**
     * Pair the string with the int.
     */
    public ModeLine(String s, int m)
    {
        line = s;
        mode = m;
    }
}
