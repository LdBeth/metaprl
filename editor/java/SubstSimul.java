/**
 * This is a simultaneous substitution.
 * Invariant: the lengths of the variable
 * array and the term array should be the same.
 */
class SubstSimul
extends Subst
{
    /**
     * Variable names.
     */
    final String[] vars;

    /**
     * Term to be substituted.
     */
    final Term[] terms;

    /**
     * Make a new subst.
     */
    SubstSimul(String[] vars, Term[] terms)
    {
        if(vars.length != terms.length)
            throw new RuntimeException("Subst.new: lengths do not match");
        this.vars = vars;
        this.terms = terms;
    }
}

/*
 * $Log$
 * Revision 1.1  1998/02/05 15:47:44  jyh
 * This is a simple term display in an applet.
 *
 */
