/**
 * An EvalError is thrown when evaluation gets stuck.
 */

public class EvalError
extends Exception
{
    /**
     * Term being evaluated.
     */
    public Term term;

    /**
     * Need a message too.
     */
    public EvalError(String msg, Term term)
    {
        super(msg);
        this.term = term;
    }
}

