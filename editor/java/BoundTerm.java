/**
 * A bound term is a term with an array of binding vars.
 */

import netscape.util.*;

public class BoundTerm
{
    /**
     * This is the enclosed term.
     */
    Term term;

    /**
     * These are the binding vars.
     */
    String[] bvars;

    /**
     * Build the term from its parts.
     */
    public BoundTerm(String[] bvars, Term term)
    {
        this.term = term;
        this.bvars = bvars;
    }

    /**
     * Arity of this term.
     */
    public int arity()
    {
        return bvars.length;
    }
}

