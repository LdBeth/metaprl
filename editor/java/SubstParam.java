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

