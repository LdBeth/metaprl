/**
 * We have short and long filenames (just like your favorite
 * operating system :().  This manages pathnames, and conversion
 * to short/long strings.  Pathnames are functional objects.
 *
 * The path is a linked list, from the leaf to the root.
 */

class PathComponent
extends Filename
implements Marshalable
{
    PathComponent next;

    PathComponent(Filename file, PathComponent n)
    {
        super(file.shortName(), file.longName());
        next = n;
    }

    PathComponent(PathComponent p)
    {
        this(p, p.next);
    }

    /**
     * Size has an extra pointer.
     */
    public void Marshal(MarshalInfo info)
    {
        super.Marshal(info);
        next.Marshal(info);
    }
}

public class Pathname
implements Marshalable
{
    /** Inverted list of filenames */
    protected PathComponent path;

    /**
     * Size of this pathname.
     */
    public void Marshal(MarshalInfo info)
    {
        path.Marshal(info);
    }

    /** Get the filename of the leaf */
    public Filename filename()
    {
        return path;
    }

    /** Get the short path */
    public String shortName()
    {
        PathComponent p = path;
        if(p == null)
            return "/";

        StringBuffer buf = new StringBuffer();
        while(p != null) {
            buf.insert(0, p.shortName());
            buf.insert(0, "/");
            p = p.next;
        }
        return buf.toString();
    }

    /** Return true if this is a root directory */
    public boolean isRoot()
    {
        return path == null;
    }

    /** Count up the number of filenames in the path */
    public int count()
    {
        int i = 0;
        PathComponent p = path;
        while(p != null) {
            i++;
            p = p.next;
        }
        return i;
    }

    /** Get the parent directory */
    public Pathname parent()
    {
        if(path == null)
            return this;
        return new Pathname(path.next);
    }

    /** Ascend the path with the given number of components */
    public Pathname parent(int i)
    {
        int j = count();
        if(i < 0 || i > j)
            throw new IllegalArgumentException("Pathname.parent");
        j -= i;
        PathComponent p = path;
        while(j != 0) {
            p = p.next;
            j--;
        }
        return new Pathname(p);
    }

    /** Build a subdirectory */
    public Pathname subdir(Filename file)
    {
        return new Pathname(new PathComponent(file, path));
    }

    /** Build a subdirectory, with a simple filename */
    public Pathname subdir(String file)
    {
        return new Pathname(new PathComponent(new Filename(file), path));
    }

    /** Build a subdirectory, with a simple filename */
    public Pathname subdir(String name, String title)
    {
        return new Pathname(new PathComponent(new Filename(name, title), path));
    }

    /** Split a pathname into its components */
    public Filename[] split()
    {
        // Count the total components
        int count = 0;
        PathComponent p = path;
        while(p != null) {
            count++;
            p = p.next;
        }
        
        // Collect them
        Filename[] files = new Filename[count];
        p = path;
        while(count != 0) {
            files[--count] = p;
            p = p.next;
        }

        return files;
    }

    /** Make an array of all the paths */
    public Pathname[] hier()
    {
        // Count the total components
        int count = 0;
        PathComponent p = path;
        while(p != null) {
            count++;
            p = p.next;
        }
        
        // Collect them
        Pathname path = this;
        Pathname[] paths = new Pathname[count];
        while(count != 0) {
            paths[--count] = path;
            path = path.parent();
        }

        return paths;
    }


    /**
     * This is a relatively expensive operation.
     * Chop some directories off the front.
     *
     * i: the number of components to chop
     */
    public Pathname relative(int i)
    {
        // Compute the length
        PathComponent comp = path;
        int length = 0;
        while(comp != null) {
            length++;
            comp = comp.next;
        }

        // Truncate, functionally, at the right point
        length -= i;
        if(length <= 0)
            return new Pathname();
        else {
            comp = new PathComponent(path);
            Pathname p = new Pathname(comp);
            while(--length != 0)
                comp = comp.next = new PathComponent(comp.next);
            comp.next = null;
            return p;
        }
    }

    /**
     * This is a relatively expensive operation.
     * Add a directory to the front.
     */
    public Pathname relative(Filename file)
    {
        // Degenerate case
        if(path == null) {
            PathComponent comp = new PathComponent(file, null);
            return new Pathname(comp);
        }

        // Link together a new path
        PathComponent comp = new PathComponent(path);
        Pathname p = new Pathname(comp);
        while(comp.next != null)
            comp = comp.next = new PathComponent(comp.next);
        comp.next = new PathComponent(file, null);
        return p;
    }

    /**
     * Relative,creating a filename from the string.
     */
    public Pathname relative(String s)
    {
        return relative(new Filename(s, s));
    }

    /**
     * Get the first directory on the path.
     */
    public Filename root()
    {
        PathComponent comp = path;
        if(comp == null)
            return null;
        while(comp.next != null)
            comp = comp.next;
        return comp;
    }

    /**
     * Add a suffix to the tail.
     */
    public Pathname suffix(String suffix)
    {
        // Degenerate case
        if(path == null)
            return new Pathname(suffix);

        // Link together a new path
        String name = path.shortName() + suffix;
        PathComponent comp = new PathComponent(new Filename(name, name), path.next);
        return new Pathname(comp);
    }

    // Build a pathname from the component list
    protected Pathname(PathComponent p)
    {
        path = p;
    }

    /** Construct a path from a shortname */
    public Pathname(String s)
    {
        String[] dirs = StringTokenizer.ParseArgs(s, '/');
        PathComponent p = null;
        for(int i = 0; i != dirs.length; i++)
            p = new PathComponent(new Filename(dirs[i], dirs[i]), p);
        path = p;
    }

    /** Singleton path */
    public Pathname(Filename n)
    {
        this(new PathComponent(n, null));
    }

    /** Root path */
    public Pathname()
    {
        path = null;
    }
}
           
/*
 * $Log$
 * Revision 1.1  1998/02/05 15:49:15  jyh
 * This is a simple term display in an applet.
 *
 * Revision 1.7  1997/12/15 22:16:51  jyh
 * First working version using ocaml-1.07
 *
 * Revision 1.6  1997/12/15 15:26:08  jyh
 * Upgrading to ocaml-1.07.
 *
 * Revision 1.5  1997/11/24 15:54:06  jyh
 * Fixed some glitch code.
 *
 * Revision 1.4  1997/11/07 21:48:48  jyh
 * Working Ejb.ifc.
 *
 * Revision 1.3  1997/11/06 15:20:09  jyh
 * Incremental change toward recording.
 *
 * Revision 1.2  1997/10/29 00:27:22  jyh
 * Batch mode Java.
 *
 * Revision 1.1  1997/10/27 15:08:13  jyh
 * First working version of Java EJB.
 *
 */
