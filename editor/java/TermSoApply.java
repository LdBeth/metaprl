/**
 * A second order application is used to
 * make a substitution instance of a term.
 */

public class TermSoApply
extends Term
{
    /**
     * Name of the variable subtituted for,
     */
    String name;

    /**
     * Terms to be subsituted.
     */
    Term[] args;

    /**
     * Build a second order application.
     */
    public TermSoApply(String name, Term[] args)
    {
        this.name = name;
        this.args = args;
    }
}

/*
 * $Log$
 * Revision 1.1  1998/02/05 15:48:04  jyh
 * This is a simple term display in an applet.
 *
 */
