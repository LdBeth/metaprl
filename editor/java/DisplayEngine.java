/**
 * The Display engine is an extended evaluator with default
 * rules for displaying variables, stuck terms, etc.
 */

import netscape.application.*;
import netscape.util.*;

class DisplayEngine
extends Eval
{
    /**
     * This term has no display form, so use a default one.
     */
    protected void evalStuck(Closure closure, TermNuprl term, Vector results, int i)
    {
        // Push the zone
        results.insertElementAt(new TermPush(1200), i++);
        results.insertElementAt(new TermZone(TermZone.SOFT_ZONE), i++);

        // Display the operator name as a string
        StringBuffer buf = new StringBuffer();
        Opname opname = term.operator.opname;
        if(opname == null)
            buf.insert(0, ".");
        else {
            buf.insert(0, opname.head);
            while((opname = opname.tail) != null) {
                buf.insert(0, '.');
                buf.insert(0, opname.head);
            }
        }
        results.insertElementAt(new TermString(buf.toString()), i++);

        // Display parameters if there are any
        Param[] params = term.operator.params;
        if(params.length != 0) {
            results.insertElementAt(new TermString("["), i++);
            for(int j = 0; j != params.length; j++) {
                i = params[j].display(results, i);
                results.insertElementAt(new TermString(j == params.length - 1 ? "]" : "; "), i++);
            }
        }

        // Push down subterms if there are any
        BoundTerm[] bterms = term.bterms;
        if(bterms.length != 0) {
            results.insertElementAt(new TermString("{"), i++);
            for(int j = 0; j != bterms.length; j++) {
                i = evalBTerm(closure, bterms[j], results, i);
                if(j == bterms.length - 1)
                    results.insertElementAt(new TermString("}"), i++);
                else {
                    results.insertElementAt(new TermString("; "), i++);
                    results.insertElementAt(new TermBreak(true, "", ""), i++);
                }
            }
        }

        // Pop the zone
        results.insertElementAt(new TermPop(), i++);
        results.insertElementAt(new TermZone(TermZone.END_ZONE), i++);
    }

    /*
     * Display a bound term.
     * We display each of the binding variables,
     * then make a new closure for the term.
     */
    protected int evalBTerm(Closure closure, BoundTerm bterm, Vector results, int i)
    {
        String[] bvars = bterm.bvars;
        if(bvars.length != 0) {
            for(int j = 0; j != bvars.length; j++) {
                results.insertElementAt(new TermString(bvars[j]), i++);
                results.insertElementAt(new TermString(j == bvars.length - 1 ? ". " : ", "), i++);
            }
        }

        // Add the term
        results.insertElementAt(new Closure(closure, bterm.term), i++);
        return i;
    }

    /**
     * Display a variable.
     */
    protected void evalVar(TermVar var, Vector results, int i)
    {
        results.insertElementAt(new TermString(var.name), i);
    }

    /**
     * Display a SoVar.
     */
    protected void evalSoVar(TermSoVar var, Vector results, int i)
    {
        results.insertElementAt(new TermString(var.name), i++);
        results.insertElementAt(new TermString("["), i++);
        String[] vars = var.vars;
        for(int j = 0; j != vars.length; j++) {
            results.insertElementAt(new TermString(vars[j]), i++);
            results.insertElementAt(j == vars.length - 1 ? "; " : "]", i++);
        }
    }

    /**
     * Display a SoApply.
     */
    protected void evalSoApply(Closure closure, TermSoApply var, Vector results, int i)
    {
        results.insertElementAt(new TermString(var.name), i++);
        results.insertElementAt(new TermString("["), i++);
        Term[] args = var.args;
        for(int j = 0; j != args.length; j++) {
            results.insertElementAt(new Closure(closure, args[i]), i++);
            results.insertElementAt(j == args.length - 1 ? "; " : "]", i++);
        }
    }
}        

/*
 * $Log$
 * Revision 1.1  1998/02/05 15:46:16  jyh
 * This is a simple term display in an applet.
 *
 */
