/*
 * A queue implemented as a vector.
 */

import netscape.util.*;

class Queue
extends Vector
implements Marshalable
{
    /**
     * Compute the total size of the queue.
     * Each element must implement Size interface.
     */
    public void Marshal(MarshalInfo info)
    {
        info.Marshal((Vector) this);
    }

    /**
     * Adding something to the queue.
     */
    public synchronized void PushFirst(Object x)
    {
        insertElementAt(x, 0);
    }

    /**
     * Add something to the end of the queue.
     */
    public synchronized void PushLast(Object x)
    {
        addElement(x);
    }

    /**
     * Removing the head element.
     */
    public synchronized Object PopFirst()
    {
        Object x = firstElement();
        removeElementAt(0);
        return x;
    }

    /**
     * Remove the final element.
     */
    public synchronized Object PopLast()
    {
        int index = size() - 1;
        Object x = elementAt(index);
        removeElementAt(index);
        return x;
    }

    /**
     * Is the queue empty?
     */
    public synchronized boolean IsEmpty()
    {
        return size() == 0;
    }
}

