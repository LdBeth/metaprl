/*
 * This defines a text string with an integer.
 * (Don't you wish we had tuples?)
 */
public class ModeLine
implements Marshalable
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

    /**
     * Compute the size.
     */
    public void Marshal(MarshalInfo info)
    {
        info.Marshal(line);
        info.Marshal(mode);
    }
}

/*
 * $Log$
 * Revision 1.1  1998/02/05 15:49:10  jyh
 * This is a simple term display in an applet.
 *
 */
