/*
 * A level expression is a constant,
 * plus a list of variables paried with numbers.
 * Level expressions are always stored in normal form:
 * the vars are sorted.
 */

import netscape.util.*;

public class ParamLevelExp
extends Param
{
    /**
     * The level.
     */
    protected LevelExp level;

    /**
     * Build a level expression from a single var.
     */
    public ParamLevelExp(LevelExp level)
    {
        this.level = level;
    }

    /**
     * Match with another number.
     */
    Param match(Param param2)
        throws ParamMatchError
    {
        if(param2 instanceof ParamLevelExp) {
            if(level.equals(((ParamLevelExp) param2).level))
                return null;
        }
        throw new ParamMatchError("Levels do not match", this, param2);
    }

    /**
     * Type.
     */
    public int getType()
    {
        return PARAM_LEVEL;
    }

    /**
     * Simple type.
     */
    public int getSimpleType()
    {
        return PARAM_LEVEL;
    }

    /**
     * Display form.
     */
    public int display(Vector results, int i)
    {
        // Print result in parens
        results.insertElementAt(new TermString("("), i++);

        // Print the offset
        LevelVar[] vars = level.vars;
        if(level.offset != 0) {
            results.insertElementAt(new TermString(Integer.toString(level.offset)), i++);
            if(vars.length != 0)
                results.insertElementAt(new TermString(" | "), i++);
        }

        // Print the vars
        for(int j = 0; j != vars.length; j++) {
            if(j != 0)
                results.insertElementAt(new TermString(" | "), i++);
            LevelVar var = vars[j];
            results.insertElementAt(new TermString(var.var), i++);
            if(var.offset != 0) {
                String update;
                switch(var.offset) {
                case 1:
                    update ="'";
                    break;
                case 2:
                    update = "''";
                    break;
                default:
                    update = " + " + var.offset;
                    break;
                }
                results.insertElementAt(new TermString(update), i++);
            }
        }
        results.insertElementAt(new TermString(")"), i++);

        return i;
    }
}        

/*
 * $Log$
 * Revision 1.1  1998/02/05 15:46:59  jyh
 * This is a simple term display in an applet.
 *
 */
