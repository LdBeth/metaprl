/**
 * This interface just provides a function to
 * compute the total memory size of this object.
 * Also give numbers for ints etc.
 */

public abstract interface Marshalable
{
    /**
     * We keep a marker for the object.
     */
    int mark = 0;

    /**
     * Size of this object.
     */
    public abstract void Marshal(MarshalInfo info);
}

