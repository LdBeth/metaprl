/**
 * Our "evaluator" is a lzay evaluator.
 * It keys on the operator name, and tries
 * rewrites until one of them succeeds.  This
 * evaluator continues until there are no more
 *    TermNuprl, TermVar, TermSoVar, TermSoApply, Closure
 *
 * It issues upcalls to handle terms that
 * have no rewrites.
 */

import netscape.util.*;

public abstract class Eval
{
    /**
     * Hashtable: opname -> Rewrite Vector
     */
    protected Hashtable base;

    /**
     * Empty evaluator.
     */
    public Eval()
    {
        base = new Hashtable();
    }

    /**
     * Add a rewrite.
     */
    public void add(Rewrite rw)
    {
        Opname opname = rw.redex.operator.opname;
        Vector rewrites = (Vector) base.get(opname);
        if(rewrites == null) {
            rewrites = new Vector();
            base.put(opname, rewrites);
        }
        rewrites.addElement(rw);
    }

    /**
     * Completely evaluate a term.
     * Return a Vector of terms.
     */
    public Vector eval(Term term)
        throws EvalError
    {
        /* Make a vector of closures */
        Vector results = new Vector();
        results.addElement(new Closure(term));

        /* Walk through the vector, fully evaluating every term */
        int i = 0;
        while(i != results.size()) {
            term = (Term) results.removeElementAt(i);
            if(term instanceof Closure)
                eval((Closure) term, results, i);
            else {
                results.insertElementAt(term, i);
                i++;
            }
        }

        return results;
    }

    /**
     * Evaluate a closure.
     *    1. Unfold it if possible
     *    2. Apply the first possible rewrite
     *    X. If both fail, appeal to upcalls
     *
     * i is the index where to put the result.
     */
    private void eval(Closure closure, Vector results, int i)
        throws EvalError
    {
        Term term = closure.term;
        if((term instanceof TermNuprl) == false) {
            try {
                closure = closure.unfold();
            }
            catch(MatchError x) {
                // Ignore it
            }
            catch(FreeVar x) {
                // Ignore it
            }
        }

        // Look at it again
        term = closure.term;
        if(term instanceof TermNuprl)
            eval(closure, (TermNuprl) term, results, i);
        else if(term instanceof TermVar)
            evalVar((TermVar) term, results, i);
        else if(term instanceof TermSoVar)
            evalSoVar((TermSoVar) term, results, i);
        else if(term instanceof TermSoApply)
            evalSoApply(closure, (TermSoApply) term, results, i);
        else
            // Put it back in the result list without its environment
            results.insertElementAt(term, i);
    }

    /**
     * Try to apply a rewrite to the NuprlTerm.
     */
    protected boolean evalMaybe(Closure closure, TermNuprl term, Vector results, int i)
        throws EvalError
    {
        Opname opname = term.operator.opname;
        Vector rewrites = (Vector) base.get(opname);
        if(rewrites != null) {
            int length = rewrites.size();
            for(int j = 0; j != length; j++) {
                Rewrite rw = (Rewrite) rewrites.elementAt(j);
                try {
                    Closure[] new_results = rw.rewrite(closure);
                    for(int k = 0; k != new_results.length; k++)
                        results.insertElementAt(new_results[k], i + k);

                    // Done
                    return true;
                }
                catch(MatchError x) {
                    // Keep going
                }
            }
        }

        // Failed...
        return false;
    }

    /**
     * Try to apply a rewrite to the NuprlTerm.
     */
    protected void eval(Closure closure, TermNuprl term, Vector results, int i)
        throws EvalError
    {
        if(evalMaybe(closure, term, results, i) == false)
            evalStuck(closure, term, results, i);
    }

    /**
     * No rewrite rule exists.
     */
    protected void evalStuck(Closure closure, TermNuprl term, Vector results, int i)
        throws EvalError
    {
        throw new EvalError("Eval.eval: no rule to evaluate", term);
    }
                    
    /**
     * This is the upcall the evaluate a variable.
     */
    protected void evalVar(TermVar term, Vector results, int i)
        throws EvalError
    {
        throw new EvalError("Eval.eval: free variable", term);
    }

    /**
     * This is the upcall to evaluate a SoVar.
     */
    protected void evalSoVar(TermSoVar term, Vector results, int i)
        throws EvalError
    {
        throw new EvalError("Eval.eval: free variable", term);
    }

    /**
     * This is the upcall to evaluate a SoApply.
     */
    protected void evalSoApply(Closure closure, TermSoApply term, Vector results, int i)
        throws EvalError
    {
        throw new EvalError("Eval.eval: free application", term);
    }
}

