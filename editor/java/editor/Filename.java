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
           
/*
 * $Log$
 * Revision 1.1  1998/02/05 15:48:56  jyh
 * This is a simple term display in an applet.
 *
 * Revision 1.3  1997/12/15 22:16:38  jyh
 * First working version using ocaml-1.07
 *
 * Revision 1.2  1997/12/15 15:25:49  jyh
 * Upgrading to ocaml-1.07.
 *
 * Revision 1.1  1997/10/27 15:08:11  jyh
 * First working version of Java EJB.
 *
 */
