/**
 * Error that is thrown when two terms
 * are compared, and they don't match.
 */

public class MatchError
extends Exception
{
    /**
     * The term doing the matching.
     */
    public Term pattern;

    /**
     * Term being matched.
     */
    public Term term;

    /**
     * Need an error message too.
     */
    public MatchError(String msg, Term pattern, Term term)
    {
        super(msg);
        this.pattern = pattern;
        this.term = term;
    }
}

/*
 * $Log$
 * Revision 1.1  1998/02/05 15:46:42  jyh
 * This is a simple term display in an applet.
 *
 */
