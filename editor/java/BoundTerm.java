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

/*
 * $Log$
 * Revision 1.1  1998/02/05 15:46:09  jyh
 * This is a simple term display in an applet.
 *
 */
