/*
 * This is what a Nuprl term looks like.
 * A term contains an operator and an array of
 * bound terms.  Variables are intermixed with
 * terms in the subterm list.  A variable is represented
 * as a String.
 */

public class TermNuprl
extends Term
{
    /**
     * Operator.
     */
    Operator operator;

    /**
     * Subterms.
     */
    BoundTerm[] bterms;

    /**
     * Build the term from its parts.
     */
    public TermNuprl(Operator op, BoundTerm[] bterms)
    {
        this.operator = op;
        this.bterms = bterms;
    }
}

/*
 * $Log$
 * Revision 1.1  1998/02/05 15:47:57  jyh
 * This is a simple term display in an applet.
 *
 */
