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

