/**
 * The is a contractum for a particular pattern.
 * The contractum can be applied to rewrite an
 * instance of the pattern to an instance of the
 * contractum.
 */

import netscape.util.*;

/*
 * Compile a list of substitutions for each of
 * the subterms of the redex.
 */
class SoApplySubst
{
    /* Name of var to be substituted for */
    String var;

    /* Terms that will be substituted for free variables */
    Term[] args;

    /* Linked list */
    SoApplySubst next;

    /*
     * Make a new one.
     */
    SoApplySubst(String var, Term[] args, SoApplySubst next)
    {
        this.var = var;
        this.args = args;
        this.next = next;
    }
}

/*
 * Pair each contractum with its substitution info.
 */
class Contractum
{
    /* The substitutions */
    SoApplySubst[] substs;

    /* The contractum */
    Term contractum;

    /*
     * Make a new one.
     */
    Contractum(SoApplySubst[] substs, Term contractum)
    {
        this.substs = substs;
        this.contractum = contractum;
    }
}

/**
 * The rewrite class keeps a compiled contractum,
 * and a substitution list.
 */
public class Rewrite
{
    /**
     * Keep the original redex for
     * operator matching, and for arities.
     */
    TermNuprl redex;

    /**
     * Keep the contracta info.
     */
    Contractum[] contracta;

    /************************************************************************
     * COMPILING                                                            *
     ************************************************************************/

    /**
     * Build the contractum from the pattern.
     * Identify all variables the occur in the pattern
     * and standardize them apart.  Build the
     * result and the substitution for the rewrite.
     */
    public Rewrite(Term r, Term[] c)
        throws MatchError
    {
        /* Redex is required to be a Nuprl term */
        if((r instanceof TermNuprl) == false)
            throw new MatchError("Redex is not a Nuprl term", r, r);
        redex = (TermNuprl) r;

        /* Generate the variables used to represent the subterms */
        contracta = new Contractum[c.length];
        for(int i = 0; i != c.length; i++) {
            SoApplySubst[] substs = new SoApplySubst[redex.bterms.length];
            Term contractum = compile(substs, redex, c[i]);
            contracta[i] = new Contractum(substs, contractum);
        }
    }

    /*
     * Walk through the contractum, finding the locations
     * for substitution.  Replace them with the redex vars.
     * Pass in the redex, so that the substitution can be specified.
     */
    private Term compile(SoApplySubst[] substs, TermNuprl redex, Term contractum)
        throws MatchError
    {
        Term term;

        // Regular term map the compile onto subterms
        if(contractum instanceof TermNuprl)
            term = compile(substs, redex, (TermNuprl) contractum);
        else if(contractum instanceof TermVar)
            term = contractum;
        else if(contractum instanceof TermSoApply)
            term = compile(substs, redex, (TermSoApply) contractum);
        else
            term = contractum;
        return term;
    }

    /*
     * Compile a TermNuprl by mapping compilation onto subterms.
     */
    private Term compile(SoApplySubst[] substs, TermNuprl redex, TermNuprl contractum)
        throws MatchError
    {
        BoundTerm[] bterms = contractum.bterms;
        BoundTerm[] new_bterms = new BoundTerm[bterms.length];
        for(int i = 0; i != bterms.length; i++) {
            BoundTerm bterm = bterms[i];
            new_bterms[i] = new BoundTerm(bterm.bvars, compile(substs, redex, bterm.term));
        }
        return new TermNuprl(contractum.operator, new_bterms);
    }

    /*
     * Compile a variable application.
     *    1. compile all the subterms
     *    2. replace occurrence with a fresh variable
     *    3. add a subsitution to the rewrite
     */
    private Term compile(SoApplySubst[] substs, TermNuprl redex, TermSoApply contractum)
        throws MatchError
    {
        // Compile all the arguments
        Term[] args = contractum.args;
        Term[] new_args = new Term[args.length];
        for(int i = 0; i != args.length; i++)
            new_args[i] = compile(substs, redex, args[i]);

        // Search for the redex
        BoundTerm[] bterms = redex.bterms;
        for(int i = 0; i != bterms.length; i++) {
            BoundTerm bterm = bterms[i];
            if(bterm.term instanceof TermSoVar) {
                TermSoVar var = (TermSoVar) bterm.term;
                if(var.name.equals(contractum.name)) {
                    // Found the redex; check argument list lengths
                    int length = new_args.length;
                    if(var.vars.length != length || bterm.bvars.length != length)
                        throw new MatchError("Rewrite.compile(TermSoApply): argument lengths do not match", redex, contractum);

                    // Sort the arguments by binding variable index
                    Term[] sorted_args = sortArgs(bterm.bvars, var.vars, new_args);

                    // Make up a new variable to place in this position
                    String new_name = TermVar.genstring();

                    // Add a binding to the contractum
                    substs[i] = new SoApplySubst(new_name, sorted_args, substs[i]);

                    // Replace this occurrence with the plain variable
                    return new TermVar(new_name);
                }
            }
        }

        // Didn't find the var in the redex, so fail
        throw new MatchError("Rewrite.compile(TermSoApply): var does not occur in pattern", redex, contractum);
    }

    /*
     * Sort the arguments according to the two lists.
     */
    Term[] sortArgs(String[] bvars, String[] vars, Term[] args)
        throws MatchError
    {
        // New array
        int length = args.length;
        Term[] new_args = new Term[length];

        // Map them
        for(int i = 0; i != length; i++) {
            String var = vars[i];
            int j = 0;
            while(bvars[j].equals(var) == false && j != length)
                j++;
            if(j == length)
                throw new MatchError("Arity mismatch", args[i], args[i]);
            new_args[j] = args[i];
        }

        return new_args;
    }

    /************************************************************************
     * APPLICATION                                                          *
     ************************************************************************/

    /**
     * Apply the rewrite to a term.
     * Throws MatchError if there is no match.
     *
     * We are passed a term that is a Closure;
     * so make sure it is unfolded to a TermNuprl.
     *
     * Pull apart the parameters and subterms of
     * the passed term.
     */
    public Closure[] rewrite(Closure closure)
        throws MatchError
    {
        // Unfold the term
        try {
            closure.unfold();
        }
        catch(FreeVar x) {
            throw new MatchError("Term is a variable", redex, closure.term);
        }
        TermNuprl term = (TermNuprl) closure.term;

        // Check opnames
        Operator operator1 = redex.operator;
        Opname opname1 = operator1.opname;
        Operator operator2 = term.operator;
        Opname opname2 = operator2.opname;
        if(opname1.equals(opname2) == false)
            throw new MatchError("Operator names do not match", redex, term);

        // Collect parameters into the Vector param_substs
        Param[] params1 = operator1.params;
        Param[] params2 = operator2.params;
        if(params1.length != params2.length)
            throw new MatchError("Parameter counts differ", redex, term);
        Vector param_substs = null;
        for(int i = 0; i != params1.length; i++) {
            Param param1 = params1[i];
            Param param2 = params2[i];
            try {
                if(param1 instanceof ParamMeta) {
                    // Save this for a substitution
                    ParamMeta paramm = (ParamMeta) param1;
                    if(param_substs == null)
                        param_substs = new Vector();
                    param_substs.addElement(new SubstParam(paramm.getName(), paramm.match(param2)));
                }
                else
                    param1.match(param2);
            }
            catch(ParamMatchError x) {
                throw new MatchError("Parameters do not match", redex, term);
            }
        }

        // Check subterm arities
        BoundTerm[] bterms1 = redex.bterms;
        BoundTerm[] bterms2 = term.bterms;
        if(bterms1.length != bterms2.length)
            throw new MatchError("Different number of subterms", redex, term);
        for(int i = 0; i != bterms1.length; i++) {
            if(bterms1[i].bvars.length != bterms2[i].bvars.length)
                throw new MatchError("Arity mismatch", redex, term);
        }

        // Now collect subterms
        Closure[] values = new Closure[contracta.length];
        for(int i = 0; i != contracta.length; i++) {
            Contractum contractum = contracta[i];
            Closure result = new Closure(contractum.contractum);

            // Add param substitutions
            if(param_substs != null) {
                int length = param_substs.size();
                for(int j = 0; j != length; j++) {
                    SubstParam subst = (SubstParam) param_substs.elementAt(j);
                    result = result.subst(subst);
                }
            }

            // Add subterm substitutions
            for(int j = 0; j != bterms2.length; j++) {
                BoundTerm bterm1 = bterms1[j];
                BoundTerm bterm2 = bterms2[j];
                if(bterm1.term instanceof TermVar) {
                    // Make a subsitution on this subterm
                    result = result.subst(new SubstSingle(((TermVar) bterm1.term).name, bterm2.term));
                }
                else {
                    SoApplySubst substs = contractum.substs[j];
                    while(substs != null) {
                        SubstSimul subst = new SubstSimul(bterm2.bvars, substs.args);
                        Closure arg_closure = new Closure(bterm2.term, subst);
                        result = result.subst(new SubstSingle(substs.var, arg_closure));
                        substs = substs.next;
                    }
                }
            }
            values[i] = result;
        }

        return values;
    }
}        

/*
 * $Log$
 * Revision 1.1  1998/02/05 15:47:37  jyh
 * This is a simple term display in an applet.
 *
 */
