/**
 * A parameter substitution pairs a string with
 * a parameter.
 */
class SubstParam
{
    /**
     * Variable names.
     */
    String var;

    /**
     * Param to be substituted.
     */
    Param param;

    /**
     * Make a new subst.
     */
    SubstParam(String var, Param param)
    {
        this.var = var;
        this.param = param;
    }
}

/*
 * $Log$
 * Revision 1.1  1998/02/05 15:47:43  jyh
 * This is a simple term display in an applet.
 *
 */
