/**
 * Error that is thrown when two parameters
 * are compared, and they don't match.
 */

public class ParamMatchError
extends Exception
{
    /**
     * The term doing the matching.
     */
    public Param pattern;

    /**
     * Term being matched.
     */
    public Param param;

    /**
     * Need an error message too.
     */
    public ParamMatchError(String msg, Param pattern, Param term)
    {
        super(msg);
        this.pattern = pattern;
        this.param = param;
    }
}

/*
 * $Log$
 * Revision 1.1  1998/02/05 15:47:25  jyh
 * This is a simple term display in an applet.
 *
 */
