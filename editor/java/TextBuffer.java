/**
 * We want to support basic operations on tagged text.
 */

public interface TextBuffer
{
    /**
     * Add some text to the end of the buffer.
     */
    public void append(String value);

    /**
     * Start a new line.
     */
    public void nl();

    /**
     * Add the given amount of space.
     */
    public void tab(int points);

    /**
     * Set the current font.
     * The font styles are defined in TermFont.
     */
    public void pushFont(int style, int size);

    /**
     * Restore the last font.
     */
    public void popFont();

    /**
     * Get the desired right margin.
     */
    public int getMargin();

    /**
     * Get the current cursor column.
     */
    public int getColumn();

    /**
     * Have we overflowed the margin?
     */
    public boolean overflow();

    /**
     * Get the current character position.
     */
    public int length();

    /**
     * Truncate at a certain length.
     */
    public void truncate(int length);

    /**
     * Return the formatting attributes.
     */
    public Object getAttributes();

    /**
     * Restore the formatting attributes.
     */
    public void setAttributes(Object attributes);
}

/*
 * $Log$
 * Revision 1.1  1998/02/05 15:48:14  jyh
 * This is a simple term display in an applet.
 *
 */
