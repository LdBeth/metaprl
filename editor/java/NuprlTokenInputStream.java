/*
 * Q queue that blocks when it is empty.
 * The queue is finite, so blocking can occurr during
 * during push operations as well.
 */
package edu.cornell.cs.jyh.nuprl;

import java.io.InputStream;
import java.io.IOException;
import java.util.Vector;

class NuprlTokenInputStream
extends InputStream
{
    /**
     * Queue of waiting tokens.
     */
    protected Vector queue = new Vector();

    /*
     * Use reader/writer semaphores
     */
    protected Semaphore readers = new Semaphore(0);

    /**
     * The current data buffer being read.
     */
    protected byte[] buffer;

    /**
     * Number of bytes that have already been returned from the buffer.
     */
    protected int index;

    /**
     * Readers and writers may close the pipe.
     */
    protected boolean read_closed_flag = false;
    protected boolean write_closed_flag = false;

    /**
     * Special return codes.  Must not be valid bytes.
     */
    protected static final int READ_CLOSED_CODE = -3;
    protected static final int WRITE_CLOSED_CODE = -2;
    protected static final int POLL_FAILED_CODE = -1;
    
    /**
     * Add an element to the queue.
     */
    synchronized void write(NuprlDataToken token)
    {
        if(NuprlDebug.debug_token_stream)
            System.err.println("NuprlTokenInputStream.write");
        queue.add(token);
        readers.Signal(1);
    }

    /**
     * Close the writer.
     */
    synchronized void writeclose()
    {
        if(NuprlDebug.debug_token_stream)
            System.err.println("NuprlTokenInputStream.writeclose");
        write_closed_flag = true;
        readers.Signal(1);
    }

    /**
     * Non-blocking read.
     */
    protected synchronized int poll(byte[] buf, int off, int len)
    {
        int code;

        // Check the buffer
        if(read_closed_flag)
            code = READ_CLOSED_CODE;
        else if(buffer == null) {
            if(write_closed_flag && queue.isEmpty())
                code = WRITE_CLOSED_CODE;
            else
                code = POLL_FAILED_CODE;
        }
        else {
            int available = buffer.length - index;
            if(available > len) {
                System.arraycopy(buffer, index, buf, off, len);
                index += len;
                code = len;
            }
            else {
                System.arraycopy(buffer, index, buf, off, available);
                buffer = null;
                code = available;
            }
        }

        return code;
    }

    /**
     * Poll for a single byte.
     */
    protected synchronized int poll()
    {
        int code;

        if(read_closed_flag)
            code = READ_CLOSED_CODE;
        else if(buffer == null) {
            if(write_closed_flag && queue.isEmpty())
                code = WRITE_CLOSED_CODE;
            else
                code = POLL_FAILED_CODE;
        }
        else {
            code = buffer[index++];
            if(index == buffer.length)
                buffer = null;
        }
        return code;
    }

    /**
     * Poll to skip over some data.
     */
    protected synchronized int pollskip(long len)
    {
        int code;

        if(read_closed_flag)
            code = READ_CLOSED_CODE;
        else if(buffer == null) {
            if(write_closed_flag && queue.isEmpty())
                code = WRITE_CLOSED_CODE;
            else
                code = POLL_FAILED_CODE;
        }
        else {
            int amount = buffer.length - index;
            if(len >= amount) {
                buffer = null;
                code = amount;
            }
            else {
                index += len;
                code = (int) len;
            }
        }
        return code;
    }

    /**
     * Pop a token off the queue.
     */
    protected synchronized void pop()
    {
        if(queue.isEmpty() == false) {
            NuprlDataToken token = (NuprlDataToken) queue.firstElement();
            queue.remove(0);
            buffer = token.getData();
            if(buffer.length == 0)
                buffer = null;
            else
                index = 0;
        }
    }

    /**
     * Wait for some new input.
     */
    protected void block()
    {
        System.err.println("NuprlTokenInputStream.block: begin");
        readers.Wait(1);
        System.err.println("NuprlTokenInputStream.block: unblocked");
        pop();
        System.err.println("NuprlTokenInputStream.block: end");
    }

    /**
     * See how many bytes are immediately available.
     */
    public synchronized int available()
        throws IOException
    {
        int code;
        if(read_closed_flag || buffer == null)
            code = 0;
        else
            code = buffer.length - index;
        if(NuprlDebug.debug_token_stream)
            System.err.println("NuprlTokenInputStream.available: " + code);
        return code;
    }

    /**
     * Read a single character.
     */
    public int read()
        throws IOException
    {
        int code;

    loop:
        while(true) {
            code = poll();
            switch(code) {
            case READ_CLOSED_CODE:
                throw new IOException("Stream is closed");
            case WRITE_CLOSED_CODE:
                code = -1;
                break loop;
            case POLL_FAILED_CODE:
                block();
                break;
            default:
                break loop;
            }
        }
        if(NuprlDebug.debug_token_stream)
            System.err.println("NuprlTokenInputStream.read(): " + code);
        return code;
    }

    /**
     * Read some data.
     */
    public int read(byte[] buf, int off, int len)
        throws IOException
    {
        int code;

    loop:
        while(true) {
            code = poll(buf, off, len);
            switch(code) {
            case READ_CLOSED_CODE:
                throw new IOException("Stream is closed");
            case WRITE_CLOSED_CODE:
                code = -1;
                break loop;
            case POLL_FAILED_CODE:
                block();
                break;
            default:
                break loop;
            }
        }
        if(NuprlDebug.debug_token_stream)
            System.err.println("NuprlTokenInputStream.read(b,o,l): " + code);
        return code;
    }

    /**
     * Read some data.
     */
    public int read(byte[] buf)
        throws IOException
    {
        return read(buf, 0, buf.length);
    }

    /**
     * Skip over some bytes.
     */
    public long skip(long len)
        throws IOException
    {
        long amount = 0;
    loop:
        while(len > 0) {
            int code = pollskip(len);
            switch(code) {
            case READ_CLOSED_CODE:
                throw new IOException("Stream is closed");
            case WRITE_CLOSED_CODE:
                break loop;
            case POLL_FAILED_CODE:
                block();
                break;
            default:
                amount += code;
                len -= amount;
                break;
            }
        }
        if(NuprlDebug.debug_token_stream)
            System.err.println("NuprlTokenInputStream.skip: " + amount);
        return amount;
    }

    /**
     * Close the port.
     * This jsut sets the flag.
     */
    public synchronized void close()
    {
        read_closed_flag = true;
    }

    /**
     * Mark/reset is not supported.
     */
    public void mark(int readlimint)
    {
    }

    public void reset()
    {
    }

    public boolean markSupported()
    {
        return false;
    }
}
