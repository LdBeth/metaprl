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

/*
 * $Log$
 * Revision 1.1  1998/02/05 15:46:38  jyh
 * This is a simple term display in an applet.
 *
 */
