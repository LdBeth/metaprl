/**
 * Variable is free.
 */

public class FreeVar
extends Exception
{
    /**
     * The variable that is free.
     */
    public String name;

    /**
     * Message and var.
     */
    public FreeVar(String name)
    {
        super("Free variable");
        this.name = name;
    }
}

/*
 * $Log$
 * Revision 1.1  1998/02/05 15:46:24  jyh
 * This is a simple term display in an applet.
 *
 */
