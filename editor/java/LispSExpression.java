/**
 * S-expressions have the following syntax:
 *    (lisp-epxressions)
 */

public class LispSExpression
extends LispExpression
{
    /**
     * Lisp of sub expressions.
     */
    public final LispExpression[] args;

    /**
     * Build one.
     */
    public LispSExpression(LispExpression[] args)
    {
        this.args = args;
    }
}

