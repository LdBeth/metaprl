/**
 * Lisp variables have the form:
 *    name[s-expr-list]
 */

public class LispVar
extends LispExpression
{
    /**
     * Variable name.
     */
    public final Token name;

    /**
     * Build one.
     */
    public LispVar(Token name)
    {
        this.name = name;
    }
}

