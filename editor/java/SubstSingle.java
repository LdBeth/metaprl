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

/*
 * $Log$
 * Revision 1.1  1998/02/05 15:47:46  jyh
 * This is a simple term display in an applet.
 *
 */
