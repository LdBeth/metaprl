/**
 * This class breaks up the output from an OutputStream into tokens.
 *
 * This simulates outtext.ml
 *
 * Interface:
 *    OutText(OutStream): create the buffer
 *    Raw(String s): print the string on the output
 *    Put(String s): print the string, quoting it if necessary
 *    Put(int i): print an int
 *    Put(double x): print a floating point value
 *    NL(): print a newline
 *    Term(): print a terminator (a "." on a line by itself).
 */

import java.io.*;

public class OutText
extends PrintWriter
implements Marshalable
{
    /**
     * Keep a falg if we are at beginning of line.
     */
    protected boolean bol;

    /**
     * Traverse the object.
     */
    public void Marshal(MarshalInfo info)
    {
        info.Marshal(bol);
    }

    /**
     * Need an OutputStream
     */
    OutText(OutputStream out)
    {
        super(out);
        bol = true;
    }

    /*
     * A string should be escaped if it contains a quote or white space.
     */
    public static boolean EscapeP(String s)
    {
        int length = s.length();
        if(length == 0)
            return true;
        while(--length >= 0) {
            char c = s.charAt(length);
            if(c == '"' || c == '\'' || c == '\\' || Character.isWhitespace(c))
                return true;
        }
        return false;
    }

    /*
     * Escape a string.
     */
    public static String Escape(String s)
    {
        int length = s.length();
        StringBuffer buffer = new StringBuffer(2 * length);
        buffer.append('"');
        for(int i = 0; i != length; i++) {
            char c = s.charAt(i);
            switch(c) {
            case '"':
                buffer.append('\\');
                buffer.append('"');
                break;
            case '\'':
                buffer.append('\\');
                buffer.append('\'');
                break;
            case '\n':
                buffer.append('\\');
                buffer.append('n');
                break;
            case '\r':
                buffer.append('\\');
                buffer.append('r');
                break;
            case '\\':
                buffer.append('\\');
                buffer.append('\\');
                break;
            default:
                buffer.append(c);
                break;
            }
        }
        buffer.append('"');
        return buffer.toString();
    }

    /*
     * Tell if at EOF.
     */
    public boolean EOF()
    {
        return checkError();
    }

    /*
     * Add white space if not at bol.
     */
    public void Space()
    {
        if(bol)
            bol = false;
        else
            print(' ');
    }

    /*
     * Print a string.
     */
    public void Raw(String s)
    {
        Space();
        print(s);
    }
            
    /*
     * Put a string, quoting it if necessary.
     */
    public void Put(String s)
    {
        Space();
        if(EscapeP(s))
            print(Escape(s));
        else
            print(s);
    }

    /*
     * Put an integer.
     */
    public void Put(int i)
    {
        Space();
        print(i);
    }

    /*
     * A floating point number.
     */
    public void Put(double x)
    {
        Space();
        print(x);
    }

    /*
     * A newline.
     */
    public void NL()
    {
        println();
        flush();
        bol = true;
    }

    /*
     * A termination is a '.' on a line by itself.
     */
    public void Term()
    {
        if(bol == false)
            println();
        println(".");
        bol = true;
    }
}

/*
 * $Log$
 * Revision 1.2  1999/06/25 00:18:25  jyh
 * These are some minor changes to make things work better.  Changed
 * meaning of ThinOption in elim_resource: it now means to thin the hyp
 * by default, unless overridden by thinningT false.
 *
 * Fixed some proof operations.  Added "move_to_assum" command to take
 * the current subgoal and make it an extra assumption of the entire
 * proof (it may not work at the moment).
 *
 * ls now takes a _string_ argument.  Use ls "u";; to display only the
 * unproved rules in the current module.
 *
 * Proved many membership variants of the standard type constructors,
 * but there are a few more to go.  When you are defining theories, I
 * believe you should use membership, not equality.  After all, equality
 * is derivable from membership, and membership is a lot easier.
 *
 * Still to go: ASCII format proof files; save proofs _without_ extracts
 * by default.  The expand () function does not reexpand proofs correctly.
 * A few problems with proof navigation.
 *
 * Revision 1.1  1999/06/23 04:47:59  jyh
 * This is a major release; I bumped the release number to 0.6.
 * WARNING: There are major changes here, and there may be problems
 *    with this version that prevent you from using your old proof.
 *    Save your work before using this version.
 * NOTE: If you decide to try this out, and you find problems,
 *    please(!) send me email so I can fix it.
 *
 * Changes:
 *    1. The proof structure is totally changed, so that the proof
 *       editor edits the raw proof extracts.  This means that you
 *       can view the actions of the refiner down to the primitive
 *       rule applications.  In the proof editor, you can use
 *       "down 0" (or "cd "0") to descend into the proof of a rule box.
 *       Primitive proofs are called "extracts", and are labeled with
 *       brackets (like [extract]).  To expand the extract, use the command
 *       "unfold ()".  You should be able to get all the way down to
 *       the primitive rule/rewrite applications.
 *
 *       This also means that the format of the proof files (the .prlb
 *       files) has changed.  The old proof files are still valid,
 *       but this is a hack and will be deprecated in the next
 *       few months.  I haven't yet added Alexey's ASCII proof
 *       files, but that will come with the next release.
 *
 *       As usual, the "undo ()" command undoes the last proof step,
 *       including "unfold ()".  The "nop ()" command, reset the undo
 *       stack.  I also added a "redo ()" command that undoes the
 *       last undo.
 *
 *       There is a simple rule-box proof cache for collecting proofs
 *       as you edit them.  If cached proofs are available, you will
 *       see them in brackets (like [* # * *]) on the status line.
 *       I haven't yet:( added the commands to use cached proofs.
 *
 *    2. Refiner changes.  I added two new features: the "cutT <term>"
 *       tactic cuts in a new assumption.  Also, you can apply
 *       rewrites directly on assumptions, with the "rwc" and
 *       "rwch" operations, that take a clause argument.  Basically,
 *       this means that instead of folding the goal, you can unfold
 *       the assumption.  I believe this is sound; let me know if
 *       you think otherwise!
 *
 *    3. Resource changes.  I added resource automation, built on
 *       the basic resource parsing Alexey added.  Ratherthan writing
 *       resource code for every rule, you can annotate most rules
 *       to generate the resource code directly.  You can see lots of
 *       examples in the Itt theory.  Basically, there are three useful
 *       resources annotations:
 *          intro_resource []: adds the rule as an introduction in dT
 *          intro_resource [SelectOption i]: adds to selT i dT
 *          elim_resource []: adds as an elimination rule to dT.
 *             This normally tries to thin the hyp that was eliminated.
 *          elim_resource [ThinOption]: don't thin the hyp
 *
 *       Rules should be annotated with labels on their clauses,
 *       like [wf], [main], [assertion], etc.  This means that in
 *       most tactic aplcations, you no longer need to have the
 *       thenLT [addHiddenLabel "main"; ...] part.
 *
 *       N.B.  This is the most likey parts of this release to
 *       cause problems, because I deleted most old resource code.
 *
 *       Also, you can still write standard resource code, but there
 *       is no longer a d_resource--it has been broken into two parts:
 *       the intro_resource for application to goals, and elim_resource
 *       for application to hyps.
 *
 *    4. The proof editor is multi-threaded, so you can work on multiple
 *       proofs simultaneously.  In the normal Emacs editor, this is
 *       no help for you.  But there is a new Java editor with the
 *       standard point-and-click interface, and it views the proof
 *       in HTML, with multiple windows etc.  The beautiful thing is
 *       that you can use display forms to add commands to edit windows.
 *       The sad thing is that I built it on NT, Java 1.2 is required,
 *       and I haven't tried the blackdown.org Java release on Linux.
 *       This editor is pending some bug fixes from Sun to get the
 *       fonts right (they call this a standard release?).
 *
 *       In any case, here are the direct implications.  The display
 *       forms have an "html" mode.  The display form formatting in
 *       the module Rformat has been completely redone, but display
 *       _should_ be the same as it was before.
 *
 *       It is likely that I will add an HTML dump, so we can send
 *       uneditable proofs around by email or on the web.  Check out
 *       the file theories/base/summary.ml to see some example HTML
 *       display forms.
 *
 *       The other issue: your MetaPRL process starts a web server on
 *       YOUR local machine using YOUR process id on the "next" available
 *       TCP port, and it serves files only from the search path that you pass
 *       MetaPRL.  I realize that this has security implications.  This
 *       is necessary to get browser access to the working MetaPRL proof.
 *       Is this crazy?  Let me know your beliefs, religious or
 *       otherwise.
 *
 *    5. There are numerous minor changes.  I redid parts of the WeakMemo,
 *       Term_header_constr, and TermHash.  The theories/tactic directory
 *       has been split into tactic/boot (which is not compiled by MetaPRL),
 *       and theories/tactic (which is).
 *
 * Jason
 *
 * Revision 1.5  1997/12/15 22:16:50  jyh
 * First working version using ocaml-1.07
 *
 * Revision 1.4  1997/12/15 15:26:05  jyh
 * Upgrading to ocaml-1.07.
 *
 * Revision 1.3  1997/11/24 15:54:05  jyh
 * Fixed some glitch code.
 *
 * Revision 1.2  1997/08/28 14:34:05  jyh
 * Java interface.
 *
 * Revision 1.1  1997/07/25 19:12:02  jyh
 * Raw Java version.
 *
 */
