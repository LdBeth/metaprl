/**
 * A level expression is a constant,
 * plus a list of variables paried with numbers.
 * Level expressions are always stored in normal form:
 * the vars are sorted.
 */

/*
 * We need a sort class to sort the LevelVars.
 */
class LevelVarSort
extends QuickSort
{
    /*
     * Comparison function.
     */
    public int compare(Object a, Object b)
    {
        return ((LevelVar) a).var.compareTo(((LevelVar) b).var);
    }
}

public class LevelExp
{
    /**
     * Constant value.
     */
    protected int offset;

    /**
     * Level vars.
     */
    protected LevelVar[] vars;

    /**
     * Build a level expression from a single var.
     */
    public LevelExp(String v)
    {
        offset = 0;
        vars = new LevelVar[1];
        vars[0] = new LevelVar(v);
    }

    /**
     * Build a full level expression.
     */
    public LevelExp(int offset, LevelVar[] vars)
    {
        this.offset = offset;
        this.vars = vars;
        Sort sorter = new LevelVarSort();
        sorter.sort(this.vars);
    }

    /**
     * Take max of two expressions.
     */
    public LevelExp(LevelExp exp1, LevelExp exp2)
    {
        /* Max of the offset */
        offset = Math.max(exp1.offset, exp2.offset);

        /* Count up total numbers of vars */
        int i = 0;
        int j = 0;
        int count = 0;
        while(i != exp1.vars.length && j != exp2.vars.length) {
            LevelVar var1 = exp1.vars[i];
            LevelVar var2 = exp2.vars[j];
            int k = var1.var.compareTo(var2.var);
            if(k < 0)
                i++;
            else if(k > 0)
                j++;
            else {
                i++;
                j++;
            }
            count++;
        }
        count += Math.abs(i - j);

        /* Now copy the LevelVar */
        vars = new LevelVar[count];
        i = 0;
        j = 0;
        while(i != exp1.vars.length && j != exp2.vars.length) {
            LevelVar var1 = exp1.vars[i];
            LevelVar var2 = exp2.vars[j];
            int k = var1.var.compareTo(var2.var);
            if(k < 0) {
                vars[count] = var1;
                i++;
            }
            else if(k > 0) {
                vars[count] = var2;
                j++;
            }
            else {
                vars[count] = new LevelVar(var1, Math.max(var1.offset, var2.offset));
                i++;
                j++;
            }
            count++;
        }
        while(i < j)
            vars[count++] = exp1.vars[i++];
        while(j < i)
            vars[count++] = exp2.vars[j++];
    }

    /**
     * Offset a level expression by an offset.
     */
    public LevelExp(LevelExp expr, int off)
    {
        offset = expr.offset + off;
        vars = new LevelVar[expr.vars.length];
        for(int i = 0; i != vars.length; i++)
            vars[i] = new LevelVar(expr.vars[i], off);
    }

    /**
     * See if two levels are equal.
     */
    public boolean equals(LevelExp level)
    {
        if(level.offset != offset || vars.length != level.vars.length)
            return false;
        for(int i = 0; i != vars.length; i++) {
            LevelVar var1 = vars[i];
            LevelVar var2 = level.vars[i];
            if(!var1.var.equals(var2.var) || var1.offset != var2.offset)
                return false;
        }
        return true;
    }
}        

