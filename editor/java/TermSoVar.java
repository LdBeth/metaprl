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

