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

/*
 * $Log$
 * Revision 1.1  1998/02/05 15:46:36  jyh
 * This is a simple term display in an applet.
 *
 */
