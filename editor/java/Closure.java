/**
 * Closures are just delayed substitutions.
 * This is not a closure in the traditional sense,
 * because it is recursive.  A subterm of a closure
 * may be another closure (while the traditional
 * closure has a single environment).  This helps
 * with variable renaming.  If we would use DeBruijn
 * representations, we might choose the traditional
 * form.
 *
 * Each closure contains a term, and an "environment"
 * that contains the "values" of the variables.  This
 * is purely functional: the environment just delays
 * the substitution.
 *
 * Parameters are also substituted, but there are no
 * binding occurrences of parameter variables.
 *
 * Closures are functional.
 */

import netscape.util.*;

/*
 * Keep a stack of substitutions.
 */
class ClosureSubst
extends Vector
{
    /*
     * Specific substitution.
     */
    final Subst subst;

    /*
     * Linked list.
     */
    final ClosureSubst next;

    /*
     * Make a new one.
     */
    ClosureSubst(Subst subst, ClosureSubst next)
    {
        this.subst = subst;
        this.next = next;
    }

    /*
     * Functional append.
     */
    static ClosureSubst append(ClosureSubst head, ClosureSubst tail)
    {
        // Optimization
        if(tail == null)
            return head;
        if(head == null)
            return tail;

        // Copy this list into a vector
        Vector vec = new Vector();
        for(ClosureSubst s = head; s != null; s = s.next)
            vec.addElement(s);

        // Now copy from end
        for(int i = vec.size() - 1; i >= 0; i--) {
            ClosureSubst s = (ClosureSubst) vec.elementAt(i);
            tail = new ClosureSubst(s.subst, tail);
        }
        return tail;
    }
}

/*
 * Parameter substitutions are in another list.
 */
class ClosureSubstParam
{
    /*
     * Specific substition.
     */
    final SubstParam subst;

    /*
     * Linked list.
     */
    final ClosureSubstParam next;

    /*
     * Make a new one.
     */
    ClosureSubstParam(SubstParam subst, ClosureSubstParam next)
    {
        this.subst = subst;
        this.next = next;
    }

    /*
     * Functional append.
     */
    static ClosureSubstParam append(ClosureSubstParam head, ClosureSubstParam tail)
    {
        // Optimization
        if(tail == null)
            return head;
        if(head == null)
            return tail;

        // Copy this list into a vector
        Vector vec = new Vector();
        for(ClosureSubstParam s = head; s != null; s = s.next)
            vec.addElement(s);

        // Now copy from end
        for(int i = vec.size() - 1; i >= 0; i--) {
            ClosureSubstParam s = (ClosureSubstParam) vec.elementAt(i);
            tail = new ClosureSubstParam(s.subst, tail);
        }
        return tail;
    }
}


/**
 * A closure is a term together with an ordered list of
 * simultaneous substitutions.  The substitutions are divided
 * into term substitutions and param subtitutions (for typing
 * reasons).
 */
class Closure
extends Term
{
    /**
     * There is a root term.
     */
    final Term term;

    /**
     * This is an ordered list of substitutions.
     */
    final ClosureSubst term_subst;

    /**
     * Parameter substitutions are kept in a separate list.
     */
    final ClosureSubstParam param_subst;

    /**
     * Build a closure from a single term.
     */
    public Closure(Term term)
    {
        this.term = term;
        this.term_subst = null;
        this.param_subst = null;
    }

    /**
     * Replace the term in the closure.
     */
    public Closure(Closure closure, Term term)
    {
        this.term = term;
        this.term_subst = closure.term_subst;
        this.param_subst = closure.param_subst;
    }

    /*
     * Common case to have a single substitution.
     */
    public Closure(Term term, Subst subst)
    {
        this.term = term;
        this.term_subst = new ClosureSubst(subst, null);
        this.param_subst = null;
    }

    /**
     * This form is used to create closures
     * from substitions.
     */
    protected Closure(Term term, ClosureSubst term_subst, ClosureSubstParam param_subst)
    {
        this.term = term;
        this.term_subst = term_subst;
        this.param_subst = param_subst;
    }

    /**
     * Add a subsitution.
     * This is not standard substitution--it substitutes
     * directly to the inner term.
     */
    public Closure subst(Subst subst)
    {
        ClosureSubst new_term_subst = new ClosureSubst(subst, term_subst);
        return new Closure(term, new_term_subst, param_subst);
    }

    /**
     * Substitute for a parameter.
     */
    public Closure subst(SubstParam subst)
    {
        ClosureSubstParam new_param_subst = new ClosureSubstParam(subst, param_subst);
        return new Closure(term, term_subst, new_param_subst);
    }
        
    /**
     * Unfold the term part, performing substitution
     * until a TermNuprl is obtained, and there are no
     * Meta parameters.
     *
     * We only need to unfold on simple variables.
     * We can lop off the substitution list.
     */
    public Closure unfold()
        throws FreeVar, MatchError
    {
        return unfoldClosure(this);
    }

    /*
     * The actual function is a loop.
     */
    private static Closure unfoldClosure(Closure closure)
        throws FreeVar, MatchError
    {
        /* Unfold until a TermNuprl is obtained */
        Term term = closure.term;
        while((term instanceof TermNuprl) == false) {
            /* If the term is a closure, append the environments */
            if(term instanceof Closure)
                closure = closure.unfold((Closure) term);
            else if(term instanceof TermVar) {
                try {
                    closure = closure.unfold((TermVar) term);
                }
                catch(FreeVar x) {
                    return closure;
                }
            }
            else
                return closure;
            term = closure.term;
        }

        /* Unfold the parameters until they are constants */
        return closure.unfoldParams((TermNuprl) term);
    }

    /*
     * Unfold a closure.
     * This will insert the closure'ssubstitutions before the current
     * environment.
     */
    private Closure unfold(Closure closure)
    {
        /* Insert the substitutions */
        return new Closure(closure.term,
                           ClosureSubst.append(closure.term_subst, term_subst),
                           ClosureSubstParam.append(closure.param_subst, param_subst));
    }

    /*
     * Unfold a var.  This looks up the var in the current
     * environment.  Throws FreeVar if the var is not found.
     */
    private Closure unfold(TermVar var)
        throws FreeVar
    {
        /* Get the var and find the subst */
        String name = var.name;
        for(ClosureSubst subst = term_subst; subst != null; subst = subst.next) {
            Subst var_subst = subst.subst;
            if(var_subst instanceof SubstSingle) {
                SubstSingle sub = (SubstSingle) var_subst;
                if(sub.var.equals(name)) {
                    // Found it
                    return new Closure(sub.term, subst.next, param_subst);
                }
            }
            else if(var_subst instanceof SubstSimul) {
                SubstSimul s = (SubstSimul) var_subst;
                String[] names = s.vars;
                for(int i = 0; i != names.length; i++) {
                    if(names[i].equals(name)) {
                        /* Found the term, so lop off the subst */
                        return new Closure(s.terms[i], subst.next, param_subst);
                    }
                }
            }
        }

        /* Also look through params for this var */
        for(ClosureSubstParam subst = param_subst; subst != null; subst = subst.next) {
            SubstParam param_subst = subst.subst;
            if(param_subst.var.equals(name)) {
                Param param = param_subst.param;
                if(param instanceof ParamString) {
                    /* Subst this var for a string */
                    Term term = new TermString(((ParamString) param).value);
                    return new Closure(term, null, null);
                }
            }
        }

        /* The variable is not found */
        throw new FreeVar(name);
    }

    /*
     * Expand the top parameters until they are no longer meta.
     */
    private Closure unfoldParams(TermNuprl term)
        throws FreeVar
    {
        /* Only do this if there are meta-parameters */
        Param[] params = term.operator.params;
        int i = 0;
        while(i != params.length) {
            Param param = params[i];
            if(param instanceof ParamMeta)
                break;
            i++;
        }
        if(i == params.length)
            return this;

        /* Map the params to new ones */
        Param[] new_params = new Param[params.length];
        for(i = 0; i != params.length; i++)
            new_params[i] = unfoldParam(params[i]);

        /* Construct the new term */
        term = new TermNuprl(new Operator(term.operator.opname, new_params), term.bterms);
        return new Closure(term, term_subst, null);
    }

    /*
     * Substitute a param until it is unfolded down to a constant.
     */
    private Param unfoldParam(Param param)
        throws FreeVar
    {
        ClosureSubstParam subst = this.param_subst;

        // Restart here on a successful param lookup
    found:
        while(param instanceof ParamMeta) {
            String name = ((ParamMeta) param).getName();
            while(subst != null) {
                SubstParam s = subst.subst;
                if(s.var.equals(name)) {
                    param = s.param;
                    continue found;
                }
                subst = subst.next;
            }

            /* If we get to here, it wasn't found */
            throw new FreeVar(name);
        }

        /* Got a simple param */
        return param;
    }
}    

/*
 * $Log$
 * Revision 1.1  1998/02/05 15:46:11  jyh
 * This is a simple term display in an applet.
 *
 */
