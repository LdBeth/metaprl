/**
 * Lexers return tokens, which are just strings with attributes.
 */

public class Token
{
    /**
     * The string.
     */
    public final String token;

    /**
     * Is it quoted?
     */
    public final boolean quoted;

    /**
     * Some inherited String functions.
     */
    public boolean equals(String s)
    {
        return token.equals(s);
    }

    /**
     * On tokens.
     */
    public boolean equals(Token t)
    {
        return token.equals(t.token);
    }

    /**
     * Make a token.
     */
    public Token(String token)
    {
        this.token = token;
        this.quoted = false;
    }

    /**
     * Specify the quote.
     */
    public Token(String token, boolean quoted)
    {
        this.token = token;
        this.quoted = quoted;
    }
}

