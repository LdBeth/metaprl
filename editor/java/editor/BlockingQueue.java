/*
 * Q queue that blocks when it is empty.
 * The queue is finite, so blocking can occurr during
 * during push operations as well.
 */

import Queue;
import Semaphore;

class BlockingQueue
extends Queue
implements Marshalable
{
    /*
     * Default length.
     */
    private static final int defaultLength = 1;

    /*
     * Use reader/writer semaphores
     */
    private Semaphore readers;
    private Semaphore writers;

    /**
     * Compute the total size.
     */
    public void Marshal(MarshalInfo info)
    {
        super.Marshal(info);
        info.Marshal(readers);
        info.Marshal(writers);
    }

    /**
     * Queue is initially empty.
     */
    BlockingQueue(int len)
    {
        super();
        readers = new Semaphore(0);
        writers = new Semaphore(len);
    }

    /**
     * Default queue.
     */
    BlockingQueue()
    {
        this(defaultLength);
    }


    /**
     * Adding and element to the beginning.
     */
    public void PushFirst(Object x)
    {
        writers.Wait(1);
        super.PushFirst(x);
        readers.Signal(1);
    }

    /**
     * Add and element to the end.
     */
    public void PushLast(Object x)
    {
        writers.Wait(1);
        super.PushLast(x);
        readers.Signal(1);
    }

    /**
     * Remove and element from the beginning.
     */
    public Object PopFirst()
    {
        readers.Wait(1);
        Object x = super.PopFirst();
        writers.Signal(1);
        return x;
    }

    /**
     * Remove an element from the end.
     */
    public Object PopLast()
    {
        readers.Wait(1);
        Object x = super.PopLast();
        writers.Signal(1);
        return x;
    }
}

