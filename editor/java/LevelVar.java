/**
 * This class represents a single level expression variable
 * offset by a natural number.
 */

public class LevelVar
{
    /**
     * This is the var.
     */
    String var;

    /**
     * Offset.
     */
    int offset;

    /**
     * Build from a single var.
     */
    public LevelVar(String v)
    {
        var = v;
        offset = 0;
    }

    /**
     * Offset is given.
     */
    public LevelVar(String v, int o)
    {
        var = v;
        offset = o;
    }

    /**
     * Offset a given level.
     */
    public LevelVar(LevelVar v, int o)
    {
        var = v.var;
        offset = v.offset + o;
    }
}

/*
 * $Log$
 * Revision 1.1  1998/02/05 15:46:29  jyh
 * This is a simple term display in an applet.
 *
 */
