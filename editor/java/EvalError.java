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

/*
 * $Log$
 * Revision 1.1  1998/02/05 15:46:21  jyh
 * This is a simple term display in an applet.
 *
 */
