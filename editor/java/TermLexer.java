/**
 * Our reader is simplified from the StreamTokenizer.
 * Really simple lexer:
 *     ; begins a comment
 *     )(.][=>< are special single-character tokens
 *     "string" is a token, chars are escaped with \
 *     string is a token
 */

import java.io.*;

import TermParser;

class TermLexer
implements Lexer
{
    /**
     * Will read data from a Reader.
     */
    PushbackReader reader;

    /*
     * We also allow tokens to be pushed back.
     */
    private Token token;

    /**
     * New lexer.
     */
    TermLexer(PushbackReader reader)
    {
        this.reader = reader;
        this.token = null;
    }

    /*
     * We use our own getc that wil throw an EOFException.
     */
    private char getc()
        throws IOException
    {
        int c = reader.read();
        if(c == -1)
            throw new EOFException("TermLexer.getc: reached eof");
        return (char) c;
    }

    /**
     * Check if a char is special.
     */
    protected static boolean isSpecial(char c)
    {
        boolean flag;

        switch(c) {
        case '(':
        case ')':
        case '.':
        case '[':
        case ']':
        case '"':
            flag = true;
            break;
        default:
            flag = false;
            break;
        }
        return flag;
    }

    /**
     * Push a token back onto the stream.
     */
    public void pushBack(Token token)
    {
        this.token = token;
    }

    /**
     * Get the next token.
     */
    public Token nextToken()
        throws IOException
    {
        // Check pushback
        if(token != null) {
            Token rval = token;
            token = null;
            return rval;
        }

        // Read the next token
        Token token;
        char c = skipWhiteSpace();
        if(c == '"')
            token = readQuotedString();
        else if(isSpecial(c))
            token = new Token(new Character(c).toString());
        else if(c == ';') {
            skipLine();
            token = nextToken();
        }
        else
            token = readString(c);
        return token;
    }

    /**
     * Skip white space.
     */
    protected char skipWhiteSpace()
        throws IOException
    {
        while(true) {
            char c = getc();
            if(Character.isWhitespace(c) == false)
                return c;
        }
    }

    /**
     * Skip the rest of this list.
     */
    protected void skipLine()
        throws IOException
    {
        char c;

        do {
            c = getc();
        } while(c != '\r' && c != '\n');
    }

    /**
     * Read a string.
     * Terminates on white space of IOException.
     */
    protected Token readString(char c)
    {
        StringBuffer buffer = new StringBuffer();
        buffer.append(c);

        try {
            while(true) {
                c = getc();
                if(isSpecial(c) || Character.isWhitespace(c)) {
                    reader.unread(c);
                    break;
                }
                else
                    buffer.append(c);
            }
        }
        catch(IOException e) {
            // Break the loop
        }
        return new Token(buffer.toString());
    }

    /**
     * Read a quoted string.  We read until a matching quote isfound.
     */
    protected Token readQuotedString()
    {
        StringBuffer buffer = new StringBuffer();
        try {
            while(true) {
                char c = getc();
                if(c == '"')
                    break;
                if(c == '\\') {
                    c = (char) reader.read();
                    switch(c) {
                    case 'n':
                        c = '\n';
                        break;
                    case 'r':
                        c = '\r';
                        break;
                    case 't':
                        c = '\t';
                        break;
                    }
                }
                buffer.append(c);
            }
        }
        catch(IOException e) {
            // Break the loop
        }

        return new Token(buffer.toString(), true);
    }
}                        

/*
 * $Log$
 * Revision 1.1  1998/02/05 15:47:55  jyh
 * This is a simple term display in an applet.
 *
 */
