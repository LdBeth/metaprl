/**
 * We build a parser for terms.
 * Terms can be stored in HTML files.
 * <!--
 * ...
 * <term>
 * (opname name opname)
 * (term name opname)
 * </term>
 * -->
 *
 * The comments are not necessary for the parsing,
 * but you may want them if this is a real HTML file.
 *
 * The term is transmitted as an s-expression containing a optable
 * and a sequence of terms.  The sequence is use for term compression.
 * This is the essential form we want for a term:
 * A term is a:
 *    1. simple variable is an <atom>
 *    2. second order variable is <atom> [ <term> ... <term> ]
 *    3. nuprl term is
 *       (opname (params) bterms)
 *       
 *       a. opname is (<atom> ... <atom>)
 *       b. Param is
 *          (number s)
 *          (string s)
 *          (token s)
 *          (level off (v1 off1) ... (vn offn))
 *          (var s)
 *          (mnumber s)
 *          (mstring s)
 *          (mtoken s)
 *          (mlevel s)
 *          (mvar s)
 *          (msum param1 param2)
 *          (mdiff param1 param2)
 *          (mproduct param1 param2)
 *          (mquotient param1 param2)
 *          (mrem param1 param2)
 *          (mlessthan param1 param2)
 *          (mequal param1 param2)
 *          (mnotequal param1 param2)
 *
 *      c. bterm is:
 *          (v1 ... vn . term)
 *
 * For term compression we have the following enhancements.
 * The terms and opnames are collected as a list
 *     (<kind> <atom> <value>)
 * where
 *    <kind> is opname or term.
 *    <name> is the name be assigned
 *    <value> is the value to be assigned
 * Terms are "evaluated" by replacing all occurrences of unquoted
 * LispVar's with their definition.
 */

import java.io.*;
import java.net.*;
import netscape.util.*;

/*
 * We have an extra thread to read from the stream.
 */
class ParserThread
extends Thread
{
    // Handle to parser
    TermParser parser;

    // Input stream
    PushbackReader reader;

    // Make a new one
    ParserThread(TermParser parser, PushbackReader reader)
    {
        this.parser = parser;
        this.reader = reader;
    }

    // Parse the stream
    public void run()
    {
        parser.parseFile(reader);
        try {
            reader.close();
        }
        catch(IOException e) {
            // Ignore close errors
        }
    }
}

/**
 * Parser class implementation.
 */
class TermParser
{
    /**
     * We keep an evaluator to add display forms to.
     */
    protected Eval display;

    /**
     * This is the opname table.
     */
    protected Hashtable optable;

    /**
     * Table of terms.
     */
    protected Hashtable termtable;

    /**
     * Save space on empty Param arrays.
     */
    public final static Param[] empty_param_list = new Param[0];

    /**
     * Save space on empty BoundTerm arrays.
     */
    public final static BoundTerm[] empty_bterm_list = new BoundTerm[0];

    /**
     * Empty string list for BoundTerms.
     */
    public final static String[] empty_string_list = new String[0];

    /**
     * Default parameter if parsing fails.
     */
    public final static Param default_param = new ParamNumber("0");

    /**
     * Build a parser.  This parser will scan through
     * the entire file, looking for terms.
     */
    public TermParser(Eval eval)
    {
        optable = new Hashtable();
        termtable = new Hashtable();
        display = eval;
    }

    /**
     * Get a term by name.
     */
    public synchronized Term get(String name)
    {
        return (Term) termtable.get(name);
    }

    /**
     * Parse all the terms in a file.
     * Result is a (string -> Term) Hashtable.
     */
    public synchronized void parseFile(PushbackReader reader)
    {
        // New parser for the reader
        TermLexer lexer = new TermLexer(reader);
        LispParser parser = new LispParser(lexer);

        // Read, parsing the terms after the <term> forms
        while(true) {
            try {
                Token token = lexer.nextToken();
                if(token.equals("<term>"))
                    parseTerm(parser);
            }
            catch(IOException e) {
                // Stop reading on IO error
                break;
            }
        }
    }

    /**
     * Parse from a named file.
     */
    public void parseFile(String name)
        throws FileNotFoundException
    {
        FileReader file_reader = new FileReader(name);
        BufferedReader buffered_reader = new BufferedReader(file_reader);
        PushbackReader push_reader = new PushbackReader(buffered_reader);
        parseFile(push_reader);
        try {
            push_reader.close();
        }
        catch(IOException e) {
            // Ignore it
        }
    }

    /**
     * Parse from a URL.
     */
    public synchronized void parseURL(URL url)
    {
        try {
            InputStream stream = url.openStream();
            InputStreamReader stream_reader = new InputStreamReader(stream);
            BufferedReader buffered_reader = new BufferedReader(stream_reader);
            PushbackReader push_reader = new PushbackReader(buffered_reader);
            Thread parser = new ParserThread(this, push_reader);
            parser.start();
        }
        catch(IOException e) {
            // Ignore the initial error opening the file
        }
    }

    /**
     * A term has opnames and terms.
     * Quit when we get to somethings that is not an s-expression.
     * This will stop non-terminated expressions pretty quickly.
     */
    protected void parseTerm(LispParser parser)
        throws IOException
    {
        while(true) {
            // Get the next expression
            LispExpression expr = parser.nextExpr();
            if((expr instanceof LispSExpression) == false)
                break;

            // Check length, should be at least three
            LispExpression[] args = ((LispSExpression) expr).args;
            if(args.length < 3)
                continue;
            if((args[0] instanceof LispVar) == false || (args[1] instanceof LispVar) == false)
                continue;
            String kind = ((LispVar) args[0]).name.token;
            String name = ((LispVar) args[1]).name.token;
            if(kind.equals("opname"))
                parseOpname(name, args);
            else if(kind.equals("term"))
                parseTerm(name, args[2]);
            else if(kind.equals("display"))
                parseDisplay(args);
            // Other expressions are ignored
        }
    }

    /**
     * Parse an opname.
     * The remaining arguments are quoted strings, possibly terminated with
     * an unqouted opname.
     */
    protected void parseOpname(String name, LispExpression[] args)
    {
        System.err.println("Loaded opname " + name);
        optable.put(name, compileOpname(args, 2));
    }

    /**
     * Parse a term.
     */
    protected void parseTerm(String name, LispExpression expr)
    {
        System.err.println("Loaded term " + name);
        termtable.put(name, compileTerm(new Vector(), expr));
    }

    /**
     * Compile a term from the lisp expression.
     * The context is the current list of bound variables.
     * We need to keep track of them because they are
     * not looked up in the environment.
     */
    protected Term compileTerm(Vector context, LispExpression expr)
    {
        Term term;

        if(expr instanceof LispSoApply)
            term = compileSoApply(context, (LispSoApply) expr);
        else if(expr instanceof LispVar)
            term = compileVar(context, (LispVar) expr);
        else if(expr instanceof LispSExpression)
            term = compileTerm(context, ((LispSExpression) expr).args);
        else
            term = new TermVar("error");
        return term;
    }

    /**
     * Compile a var.
     * If it is bound in the context, or if it is quoted, then it is a var.
     * Otherwise look it up.
     */
    protected Term compileVar(Vector context, LispVar var)
    {
        Token name = var.name;

        // Check if quoted
        if(name.quoted)
            return new TermVar(name.token);

        // Check if bound
        for(int i = context.size() - 1; i >= 0; i--) {
            if(name.equals((String) context.elementAt(i)))
                return new TermVar(name.token);
        }

        // Look it up
        Term term = (Term) termtable.get(name.token);
        if(term == null)
            return new TermVar(name.token);
        else
            return term;
    }

    /**
     * Compile a soApply.
     * This is never looked up, but we have to compile subterms.
     * If all subterms are vars, make this a TermSoVar.
     */
    protected Term compileSoApply(Vector context, LispSoApply expr)
    {
        // Compile arguments
        LispExpression[] args = expr.args;
        Term[] new_args = new Term[args.length];
        boolean all_vars = true;
        for(int i = 0; i != args.length; i++) {
            Term term = compileTerm(context, args[i]);
            if((term instanceof TermVar) == false)
                all_vars = false;
            new_args[i] = term;
        }

        // If all vars, get String array
        if(all_vars) {
            String[] vars = new String[new_args.length];
            for(int i = 0; i != new_args.length; i++)
                vars[i] = ((TermVar) new_args[i]).name;
            return new TermSoVar(expr.name.token, vars);
        }

        // Regular variable application
        return new TermSoApply(expr.name.token, new_args);
    }

    /**
     * Compile a term.
     *     Sub-expressions should be
     *     1. opname
     *     2. param list
     *     3. bterms
     */
    protected Term compileTerm(Vector context, LispExpression[] args)
    {
        // Check for empty term
        if(args.length == 0)
            return new TermNuprl(new Operator(null, empty_param_list), empty_bterm_list);

        // Special terms
        if(args[0] instanceof LispVar) {
            Token token = ((LispVar) args[0]).name;
            if(token.quoted == false) {
                String name = token.token;
                if(name.equals("break"))
                    return compileBreak(args);
                if(name.equals("lzone"))
                    return new TermZone(TermZone.LINEAR_ZONE);
                if(name.equals("szone"))
                    return new TermZone(TermZone.SOFT_ZONE);
                if(name.equals("hzone"))
                    return new TermZone(TermZone.HARD_ZONE);
                if(name.equals("ezone"))
                    return new TermZone(TermZone.END_ZONE);
                if(name.equals("push"))
                    return compilePush(args);
                if(name.equals("pop"))
                    return new TermPop();
                if(name.equals("string"))
                    return compileString(args);
                if(name.equals("font"))
                    return compileFont(args);
            }
        }

        // Normal opname
        Opname opname = compileOpname(args[0]);

        // Check for empty params
        if(args.length == 1)
            return new TermNuprl(new Operator(opname, empty_param_list), empty_bterm_list);
        Param[] params = compileParams(args[1]);

        // Get bound term
        BoundTerm[] bterms = compileBTerms(context, args);

        return new TermNuprl(new Operator(opname, params), bterms);
    }

    /**
     * Compile a TermBreak.
     */
    protected TermBreak compileBreak(LispExpression[] args)
    {
        boolean soft = false;
        String take = null;
        String notake = null;
        for(int i = 1; i < args.length; i++) {
            LispExpression arg = args[i];
            if(arg instanceof LispVar) {
                Token token = ((LispVar) arg).name;
                if(token.quoted) {
                    if(notake == null)
                        notake = token.token;
                    else if(take == null)
                        take = token.token;
                }
                else if(token.token.equals("soft"))
                    soft = true;
            }
        }

        return new TermBreak(soft, take, notake);
    }

    /**
     * Push takes an optional numeric.
     */
    protected TermPush compilePush(LispExpression[] args)
    {
        int space = 0;

        // See if numeric argument
        if(args.length >= 2) {
            LispExpression arg = args[1];
            if(arg instanceof LispVar) {
                try {
                    space = Integer.parseInt(((LispVar) arg).name.token);
                }
                catch(NumberFormatException e) {
                    // Ignore it
                }
            }
        }

        return new TermPush(space);
    }

    /**
     * String takes an optional string argument.
     */
    protected TermString compileString(LispExpression[] args)
    {
        String str = "";
        if(args.length >= 2) {
            LispExpression arg = args[1];
            if(arg instanceof LispVar)
                str = ((LispVar) arg).name.token;
        }
        return new TermString(str);
    }
       
    /**
     * Font takes an optional command.
     */
    protected TermFont compileFont(LispExpression[] args)
    {
        int style = FontBase.FONT_NOP;
        int size = FontBase.FONT_NOP;

        // Collect arguments
        for(int i = 1; i < args.length; i++) {
            LispExpression arg = args[i];
            if(arg instanceof LispVar) {
                String command = ((LispVar) arg).name.token;
                if(command.equals("plain"))
                    style = FontBase.FONT_PLAIN;
                else if(command.equals("bold"))
                    style = FontBase.FONT_BOLD;
                else if(command.equals("emphasis"))
                    style = FontBase.FONT_EMPHASIS;
                else if(command.equals("symbol"))
                    style = FontBase.FONT_SYMBOL;
                else if(command.equals("Small"))
                    size = FontBase.FONT_EXTRA_SMALL;
                else if(command.equals("small"))
                    size = FontBase.FONT_SMALL;
                else if(command.equals("normal"))
                    size = FontBase.FONT_NORMAL;
                else if(command.equals("large"))
                    size = FontBase.FONT_LARGE;
                else if(command.equals("Large"))
                    size = FontBase.FONT_EXTRA_LARGE;
            }
        }
        return new TermFont(style, size);
    }
       
    /**
     * Compile an opname.
     */
    protected Opname compileOpname(LispExpression expr)
    {
        Opname opname;

        // If s-expr, then we have to collect the opname
        if(expr instanceof LispSExpression)
            opname = compileOpname(((LispSExpression) expr).args, 0);
        else if(expr instanceof LispVar) {
            Token name = ((LispVar) expr).name;
            if(name.quoted)
                opname = new Opname(name.token, null);
            else {
                opname = (Opname) optable.get(name.token);
                if(opname == null)
                    opname = new Opname(name.token, null);
            }
        }
        else
            opname = null;
        return opname;
    }

    /**
     * Compute the opname from the arg list.
     */
    protected Opname compileOpname(LispExpression[] args, int off)
    {
        // Collect the opname strings into a vector
        Vector strings = new Vector();
        Opname tail = null;
        for(int i = off; i < args.length; i++) {
            LispExpression arg = args[i];
            if(arg instanceof LispVar) {
                Token name = ((LispVar) arg).name;
                if(name.quoted)
                    strings.addElement(name.token);
                else {
                    tail = (Opname) optable.get(name.token);
                    break;
                }
            }
        }

        // Now construct the opname
        for(int i = strings.size() - 1; i >= 0; i--)
            tail = new Opname((String) strings.elementAt(i), tail);
        return tail;
    }

    /**
     * Compile a list of parameters.
     */
    protected Param[] compileParams(LispExpression expr)
    {
        // Should be a list
        if((expr instanceof LispSExpression) == false)
            return empty_param_list;

        // Compile the args
        LispExpression[] args = ((LispSExpression) expr).args;
        Param[] params = new Param[args.length];
        for(int i = 0; i != args.length; i++)
            params[i] = compileParam(args[i]);
        return params;
    }

    /**
     * Compile a single param.
     * This should also be a s-expr.
     */
    protected Param compileParam(LispExpression expr)
    {
        // Should be a list
        if((expr instanceof LispSExpression) == false)
            return default_param;

        // Compile arg
        LispExpression[] args = ((LispSExpression) expr).args;
        if(args.length == 0 || (args[0] instanceof LispVar) == false)
            return default_param;
        String name = ((LispVar) args[0]).name.token;

        // Switch on type
        Param param;
        if(name.equals("number"))
            param = new ParamNumber(stringParam(args));
        else if(name.equals("string"))
            param = new ParamString(stringParam(args));
        else if(name.equals("token"))
            param = new ParamToken(stringParam(args));
        else if(name.equals("level"))
            param = new ParamLevelExp(levelParam(args));
        else if(name.equals("var"))
            param = new ParamVar(stringParam(args));
        else if(name.equals("mnumber"))
            param = new ParamMNumber(stringParam(args));
        else if(name.equals("mstring"))
            param = new ParamMString(stringParam(args));
        else if(name.equals("mtoken"))
            param = new ParamMToken(stringParam(args));
        else if(name.equals("mlevel"))
            param = new ParamMLevel(stringParam(args));
        else if(name.equals("mvar"))
            param = new ParamMVar(stringParam(args));
        else if(name.equals("msum"))
            param = new ParamMSum(firstParam(args), secondParam(args));
        else if(name.equals("mdiff"))
            param = new ParamMDiff(firstParam(args), secondParam(args));
        else if(name.equals("mproduct"))
            param = new ParamMProduct(firstParam(args), secondParam(args));
        else if(name.equals("mquotient"))
            param = new ParamMQuotient(firstParam(args), secondParam(args));
        else if(name.equals("mrem"))
            param = new ParamMRem(firstParam(args), secondParam(args));
        else if(name.equals("mlessthan"))
            param = new ParamMLessThan(firstParam(args), secondParam(args));
        else if(name.equals("mequal"))
            param = new ParamMEqual(firstParam(args), secondParam(args));
        else if(name.equals("mnotequal"))
            param = new ParamMNotEqual(firstParam(args), secondParam(args));
        else
            param = default_param;
        return param;
    }

    /*
     * Get the string argument for a parameter.
     */
    private String stringParam(LispExpression[] args)
    {
        if(args.length < 2 || (args[1] instanceof LispVar) == false)
            return "error";
        else
            return ((LispVar) args[1]).name.token;
    }

    /*
     * Get the first parameter.
     */
    private Param firstParam(LispExpression[] args)
    {
        return args.length < 2 ? default_param : compileParam(args[1]);
    }

    /*
     * Get the second parameter.
     */
    private Param secondParam(LispExpression[] args)
    {
        return args.length < 3 ? default_param : compileParam(args[2]);
    }

    /**
     * Parse a level expression.
     * Any LispVar argsument should be a constant.
     * Ant LispSExpression should be a var/int pair.
     */
    protected LevelExp levelParam(LispExpression[] args)
    {
        int offset = 0;
        Vector vars = new Vector();
        for(int i = 1; i != args.length; i++) {
            LispExpression arg = args[i];
            if(arg instanceof LispVar) {
                try {
                    int off = Integer.parseInt(((LispVar) arg).name.token);
                    if(off > offset)
                        offset = off;
                }
                catch(NumberFormatException x) {
                    // Its a variable
                    vars.addElement(new LevelVar(((LispVar) arg).name.token, 0));
                }
            }
            else if(arg instanceof LispSExpression) {
                LispExpression[] pair = ((LispSExpression) arg).args;
                if(pair.length > 0 && pair[0] instanceof LispVar) {
                    int off = 0;
                    if(pair.length > 1 && pair[1] instanceof LispVar) {
                        try {
                            off = Integer.parseInt(((LispVar) pair[1]).name.token);
                        }
                        catch(NumberFormatException x) {
                            // Leave offset at 0
                        }
                    }
                    vars.addElement(new LevelVar(((LispVar) pair[0]).name.token, off));
                }
            }
        }

        // Now collect the vars into an array
        int length = vars.size();
        LevelVar[] new_vars = new LevelVar[length];
        for(int i = 0; i != length; i++)
            new_vars[i] = (LevelVar) vars.elementAt(i);

        return new LevelExp(offset, new_vars);
    }

    /**
     * Parse the remaining terms as BoundTerms.
     * this means we should read variables until a single ".",
     * then read the rest as a term.
     */
    protected BoundTerm[] compileBTerms(Vector context, LispExpression[] args)
    {
        BoundTerm[] bterms = new BoundTerm[args.length - 2];
        for(int i = 2; i != args.length; i++)
            bterms[i - 2] = compileBTerm(context, args[i]);
        return bterms;
    }

    /**
     * Compile a single BoundTerm.
     * It should have the form of a variable, or
     *  (v1 vn . term)
     */
    protected BoundTerm compileBTerm(Vector context, LispExpression expr)
    {
        BoundTerm bterm;

        if(expr instanceof LispSoApply)
            bterm = new BoundTerm(empty_string_list, compileSoApply(context, (LispSoApply) expr));
        else if(expr instanceof LispVar)
            bterm = new BoundTerm(empty_string_list, compileVar(context, (LispVar) expr));
        else if(expr instanceof LispSExpression)
            bterm = compileBTerm(context, ((LispSExpression) expr).args);
        else
            bterm = new BoundTerm(empty_string_list, new TermVar("error"));
        return bterm;
    }

    /**
     * Compile a BoundTerm in a s-expression.
     */
    protected BoundTerm compileBTerm(Vector context, LispExpression[] args)
    {
        // Find the "." separator
        int separator = 0;
        while(separator != args.length) {
            LispExpression arg = args[separator];
            if(arg instanceof LispVar && ((LispVar) arg).name.equals("."))
                break;
            separator++;
        }

        // No separator, this is just a regular term
        if(separator == args.length)
            return new BoundTerm(empty_string_list, compileTerm(context, args));

        // Collect all the vars
        String[] vars = new String[separator];
        for(int i = 0; i != separator; i++) {
            LispExpression arg = args[i];
            String name = arg instanceof LispVar ? ((LispVar) arg).name.token : "*error*";
            vars[i] = name;
            context.addElement(name);
        }

        // Compile the term
        Term term;
        if(separator + 1 != args.length)
            term = compileTerm(context, args[separator + 1]);
        else
            term = new TermNuprl(new Operator(null, empty_param_list), empty_bterm_list);

        // Restore the context
        for(int i = 0; i != separator; i++)
            context.removeLastElement();

        return new BoundTerm(vars, term);
    }

    /**
     * Parse a display form and add it to the evaluator.
     */
    protected void parseDisplay(LispExpression[] args)
    {
        Term redex = compileTerm(new Vector(), args[2]);
        Term[] contracta = new Term[args.length - 3];
        for(int i = 0; i != contracta.length; i++)
            contracta[i] = compileTerm(new Vector(), args[i + 3]);
        try {
            display.add(new Rewrite(redex, contracta));
        }
        catch(MatchError e) {
            System.err.println("Display form error: " + e.getMessage());
        }
    }
}

/*
 * $Log$
 * Revision 1.1  1998/02/05 15:47:58  jyh
 * This is a simple term display in an applet.
 *
 */
