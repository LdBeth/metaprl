/**
 * The operator contains an opname, and a list of parameters.
 */

public class Operator
{
    /**
     * The opname.
     */
    Opname opname;

    /**
     * The parameters.
     */
    Param[] params;

    /**
     * Construct an operator.
     */
    public Operator(Opname opname)
    {
        this.opname = opname;
    }

    /**
     * Construct a parameter list.
     */
    public Operator(Opname opname, Param[] params)
    {
        this.opname = opname;
        this.params = params;
    }
}

/*
 * $Log$
 * Revision 1.1  1998/02/05 15:46:54  jyh
 * This is a simple term display in an applet.
 *
 */
