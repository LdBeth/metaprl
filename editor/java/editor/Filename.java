/**
 * We have short and long filenames (just like your favorite
 * operating system :().  This structure pairs them up in
 * a functional object.
 */

public class Filename
implements Marshalable
{
    /**
     * Title of the entry.
     */
    protected String long_name;

    /**
     * Name of the entry.
     */
    protected String short_name;

    /**
     * Get the size.
     */
    public void Marshal(MarshalInfo info)
    {
        info.Marshal(long_name);
        info.Marshal(short_name);
    }

    /** Get the long filename */
    public String longName()
    {
        return long_name;
    }

    /** Get the short filename */
    public String shortName()
    {
        return short_name;
    }

    /** Create a short/long filename pair */
    public Filename(String s, String l)
    {
        short_name = s;
        long_name = l;
    }

    /** Create a name with short and long names the same */
    public Filename(String s)
    {
        this(s, s);
    }
}
           
