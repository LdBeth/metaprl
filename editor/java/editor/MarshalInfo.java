/**
 * This interface just provides a function to
 * compute the total memory size of this object.
 * Also give numbers for ints etc.
 */

import java.net.*;
import java.util.BitSet;

import netscape.application.*;
import netscape.util.Vector;
import netscape.util.Hashtable;

public abstract class MarshalInfo
{
    /**
     * Record an int.
     */
    public abstract void Marshal(int i);

    /**
     * An array of ints.
     */
    public abstract void Marshal(int[] i);

    /**
     * Record a bool.
     */
    public abstract void Marshal(boolean f);

    /**
     * Record a double.
     */
    public abstract void Marshal(double x);

    /**
     * A char.
     */
    public abstract void Marshal(Character c);

    /**
     * Size of a string in bytes.
     */
    public abstract void Marshal(String s);

    /**
     * Handle an array of strings.
     */
    public abstract void Marshal(String[] s);

    /**
     * Size of a marshalable item.
     */
    public abstract void Marshal(Marshalable item);

    /**
     * Size of an array.
     */
    public abstract void Marshal(Marshalable[] items);

    /**
     * Size of a vector.
     */
    public abstract void Marshal(Vector vec);

    /**
     * Traverse a hashtable.
     */
    public abstract void Marshal(Hashtable table);

    /**
     * A socket.
     */
    public abstract void Marshal(Socket s);

    /**
     * A BitSet.
     */
    public abstract void Marshal(BitSet set);

    /**
     * A StringBuffer.
     */
    public abstract void Marshal(StringBuffer buf);

    /**
     * A thread.
     */
    public abstract void Marshal(Thread thread);

    /**
     * A Timer.
     */
    public abstract void Marshal(Timer timer);

    /**
     * A bitmap.
     */
    public abstract void Marshal(Image image);

    /**
     * A button.
     */
    public abstract void Marshal(View view);

    /**
     * Array of views.
     */
    public abstract void Marshal(View[] views);

    /**
     * A URL.
     */
    public abstract void Marshal(URL url);

    /**
     * A Rect.
     */
    public abstract void Marshal(Rect rect);

    /**
     * A color.
     */
    public abstract void Marshal(Color color);

    /**
     * A size.
     */
    public abstract void Marshal(Size size);

    /**
     * A point.
     */
    public abstract void Marshal(Point point);

    /**
     * A range of text.
     */
    public abstract void Marshal(Range range);

    /**
     * A layout.
     */
    public abstract void Marshal(LayoutManager layout);

    /**
     * A target.
     */
    public abstract void Marshal(Target target);

    /**
     * A menu.
     */
    public abstract void Marshal(Menu menu);

    /**
     * A sound.
     */
    public abstract void Marshal(Sound sound);

    /**
     * A font.
     */
    public abstract void Marshal(Font font);
}

