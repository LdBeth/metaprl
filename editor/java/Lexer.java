/**
 * For our purposes, we separate the inputy stream into
 * strings.
 */

import java.io.*;

public interface Lexer
{
    /**
     * For the interface, we just the the reader function.
     */
    public Token nextToken() throws IOException;

    /**
     * We allow one level of pushback.
     */
    public void pushBack(Token token);
}

/*
 * $Log$
 * Revision 1.1  1998/02/05 15:46:31  jyh
 * This is a simple term display in an applet.
 *
 */
