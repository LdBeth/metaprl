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

