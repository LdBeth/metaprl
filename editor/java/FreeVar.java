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

