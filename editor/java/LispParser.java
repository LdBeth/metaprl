/**
 * This class adds a Lisp-syntax parser to the lexer.
 * We extend the syntax a little by adding second order
 * variables.  A s-expression is:
 *     (s-expr-list)
 *     string
 *     string[s-expr-list]
 */

import java.io.*;
import netscape.util.*;

class LispParser
{
    /**
     * We keep a lexer.
     */
    protected Lexer lexer;

    /**
     * Build a parser from a Lexer.
     */
    public LispParser(Lexer lexer)
    {
        this.lexer = lexer;
    }

    /**
     * Parse the next lisp expression.
     */
    public LispExpression nextExpr()
        throws IOException
    {
        Token token = lexer.nextToken();

        // Quoted string is always a variable
        if(token.quoted)
            return readVar(token);

        // Check for s-expression
        if(token.equals("("))
            return readSExpr();

        // Close list
        if(token.equals(")") || token.equals("]"))
            return null;

        // Should be var
        return readVar(token);
    }        

    /**
     * Read the rest of the variable.
     * This has the optional form [expr-list]
     */
    protected LispVar readVar(Token name)
        throws IOException
    {
        // Check for subterms
        Token token = lexer.nextToken();
        if(token.quoted || token.equals("[") == false) {
            lexer.pushBack(token);
            return new LispVar(name);
        }

        // We have subterms
        return new LispSoApply(name, readArgs());
    }

    /**
     * Read an s-expression.
     */
    protected LispSExpression readSExpr()
        throws IOException
    {
        return new LispSExpression(readArgs());
    }

    /**
     * Read the sub expressions in an s-expression.
     */
    protected LispExpression[] readArgs()
        throws IOException
    {
        // Read the args
        Vector args = new Vector();
        LispExpression expr;
        while((expr = nextExpr()) != null)
            args.addElement(expr);

        // Collect into an array
        int length = args.size();
        LispExpression[] sargs = new LispExpression[length];
        for(int i = 0; i != length; i++)
            sargs[i] = (LispExpression) args.elementAt(i);
        return sargs;
    }
}

/*
 * $Log$
 * Revision 1.1  1998/02/05 15:46:34  jyh
 * This is a simple term display in an applet.
 *
 */
