/**
 * A second order variable is like a variable, but
 * it has named subterms that are used to specify substitution.
 */

public class TermSoVar
extends Term
{
    /**
     * The main name of the var.
     */
    String name;

    /**
     * The names of all the bound variables.
     */
    String[] vars;

    /**
     * Make a new SoVar.
     */
    public TermSoVar(String name, String[] vars)
    {
        this.name = name;
        this.vars = vars;
    }
}

/*
 * $Log$
 * Revision 1.1  1998/02/05 15:48:05  jyh
 * This is a simple term display in an applet.
 *
 */
