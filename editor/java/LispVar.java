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

/*
 * $Log$
 * Revision 1.1  1998/02/05 15:46:39  jyh
 * This is a simple term display in an applet.
 *
 */
