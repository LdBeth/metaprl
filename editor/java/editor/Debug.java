/**
 * This is a client that monitors some text.
 */

import netscape.application.*;

class Debug
extends ScrollGroup
implements Marshalable
{
    /**
     * Target for output commands.
     */
   Target target;

    /**
     * Command that is sent to the target.
     */
    String command;

    /*
     * Debug windows for the protocol
     */
    TtyArea display;
    BlockingQueue command_queue;
    BlockingQueue print_queue;

    /*
     * Threads
     */
    Thread reader;
    Thread writer;

    /**
     * Traverse the object.
     */
    public void Marshal(MarshalInfo info)
    {
        info.Marshal((Marshalable) display);
        info.Marshal((Marshalable) command_queue);
        info.Marshal((Marshalable) print_queue);
        info.Marshal(reader);
        info.Marshal(writer);
    }

    /**
     * Reader thread copies data to display.
     */
    class Reader
    extends Thread
    {
        public void run()
        {
            while(true) {
                ModeLine line = (ModeLine) print_queue.PopLast();
                display.AppendLine(line.line, line.mode == 0 ? TtyArea.OUT_TEXT : TtyArea.PROG_TEXT);
            }
        }
    }
            
    /**
     * Write thread takes commands from the window
     * and writes then to the client.
     */
    class Writer
    extends Thread
    {
        public void run()
        {
            while(true) {
                String line = (String) command_queue.PopLast();
                if(Debug.this.target != null)
                    Debug.this.target.performCommand(Debug.this.command, line);
            }
        }
    }

    /**
     * Stop debugging.
     */
    public void Close()
    {
        reader.destroy();
        writer.destroy();
    }

    /**
     * Constructor adds the action event, and grabs the line
     * of text.
     */
    Debug()
    {
        super();

        // Buffer to get nice scrolling
        setBuffered(true);

        // Create the queues
        command_queue = new BlockingQueue();
        print_queue = new BlockingQueue();

        // Display area is laced in scroll area
        setVertScrollBarDisplay(ALWAYS_DISPLAY);
        setHasHorizScrollBar(false);
        display = new TtyArea(command_queue);
        setContentView(display);

        // Start threads
        reader = new Reader();
        writer = new Writer();
        reader.start();
        writer.start();
    }

    /**
     * Set the target for commands.
     */
    void setTarget(Target target)
    {
        this.target = target;
    }

    /**
     * Set the command that is passed to the target.
     */
    void setCommand(String command)
    {
        this.command = command;
    }
}

/*
 * $Log$
 * Revision 1.1  1998/02/05 15:48:52  jyh
 * This is a simple term display in an applet.
 *
 */
