/**
 * A standard semaphore.  The semaphore is unfair,
 * unless Java enforces some sort of order on Object.notify().
 */
package edu.cornell.cs.jyh.nuprl;

class Semaphore
{
    /*
     * Keep the value of the semaphore as well
     * as the number of threads blocked.
     */
    private int value;
    private int waiters;

    /**
     * Specify the initial value of the semaphore when it
     * is created.
     */
    Semaphore(int i)
    {
        value = i;
        waiters = 0;
    }

    /**
     * Default semaphore has a value of zero.
     */
    Semaphore()
    {
        this(0);
    }

    /**
     * Wait.
     * If the count is not large enough, block
     */
    public synchronized void Wait(int count)
    {
        // Wait for value to get large enough
        while(count > value) {
            // Take everything available
            count -= value;
            value = 0;
            waiters++;

            // Wait for someone to help us out
            try {
                this.wait();
            }
            catch(InterruptedException x) {
                // Ignore this
            }
            finally {
                waiters--;
            }
        }
        
        // Take out our bite
        value -= count;

        // See if more will wake up
        if(value != 0 && waiters != 0)
            this.notify();
    }

    /**
     * Signal.
     * Restart some threads if possible.
     */
    public synchronized void Signal(int count)
    {
        // Add to current value
        value += count;
        if(waiters != 0)
            this.notify();
    }
}
