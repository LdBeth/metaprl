/**
 * This is a var with some arguments.
 */

public class LispSoApply
extends LispVar
{
    /**
     * These arg arguments to the var.
     */
    public final LispExpression[] args;

    /**
     * Build one.
     */
    public LispSoApply(Token token, LispExpression[] args)
    {
        super(token);
        this.args = args;
    }
}

