/**
 * A variable is just a string.
 */

public class TermVar
extends Term
{
    /**
     * Counter for generating new variables.
     */
    private static int new_var_index = 0;

    /**
     * Generate a new string.
     */
    static String genstring()
    {
        return "$" + new_var_index++;
    }

    /**
     * Generate a new variable guaranteed to be
     * different for all the vars in the system.
     */
    public static TermVar gensym()
    {
        return new TermVar(genstring());
    }

    /**
     * This is the var name.
     */
    public String name;

    /**
     * Build the var.
     */
    public TermVar(String v)
    {
        name = v;
    }
}      

/*
 * $Log$
 * Revision 1.1  1998/02/05 15:48:09  jyh
 * This is a simple term display in an applet.
 *
 */
