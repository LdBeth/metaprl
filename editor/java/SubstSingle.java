/**
 * This is a single substitution.
 */
class SubstSingle
extends Subst
{
    /**
     * Variable name.
     */
    final String var;

    /*
     * Term to be substituted.
     */
    final Term term;

    /**
     * Make a new subst.
     */
    SubstSingle(String var, Term term)
    {
        this.var = var;
        this.term = term;
    }
}

