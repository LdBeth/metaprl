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

