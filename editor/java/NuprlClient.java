/**
 * This is the basic connection to the server.
 * We include parsing of Telnet options, from RFCs 854-861, 930, and 1073,
 * as well as escape-code parsing used in terminals.
 *
 * This class contains code for connecting to a socket, and handling
 * the Telnet/ESC commands on the port.
 */

package edu.cornell.cs.jyh.nuprl;

import java.io.*;
import java.net.*;

class NuprlClient
implements NuprlBusClient
{
    /************************************************************************
     * CLIENT STATE                                                         *
     ************************************************************************/

    /**
     * Keep track of the socket connection.
     */
    protected Socket socket;

    /**
     * This is our endpoint on the bus.
     */
    protected NuprlBusEndpoint endpoint;

    /**
     * This is the command window for option negotiation.
     */
    protected NuprlBusPort command_port;

    /************************************************************************
     * OUTPUT STATE                                                         *
     ************************************************************************/

    /**
     * Output stream includes buffering, and printing of command values.
     */
    protected BufferedOutputStream stdin;

    /************************************************************************
     * INPUT STATE                                                          *
     ************************************************************************/

    /**
     * Raw input stream.
     */
    protected InputStream stdout;

    /**
     * We implement buffering on reads.
     */
    private static final int BUFFER_SIZE                = 1 << 16;

    /**
     * This is the address data is to be sent to.
     */
    protected String dst_host;
    protected int dst_port;

    /*
     * The reader is implemented as a state machine.
     */
    private int state                                   = STATE_DATA;

    // Normal data collection
    private final static int STATE_DATA                 = 0;

    // Telnet states
    private final static int STATE_IAC                  = 1;
    private final static int STATE_IAC_OPTION           = 2;
    private final static int STATE_IAC_SB               = 3;
    private final static int STATE_IAC_SB_IAC           = 4;

    // Terminal states
    private final static int STATE_ESC                  = 5;
    private final static int STATE_ESC_LBRACK           = 6;
    private final static int STATE_ESC_RBRACK           = 7;
    private final static int STATE_ESC_DIGIT            = 8;
    private final static int STATE_ESC_HOST             = 9;

    /**
     * Keep track of flags that have been sent and received.
     *
     * sentOptionFlags: flags that we have sent to the server
     * receivedOptionFlags: flags we have received from the server
     */
    private byte[] sentOptionFlags                      = new byte[256];
    private byte[] receivedOptionFlags                  = new byte[256];

    /**
     * Variables for constructing the tokens.
     */
    private byte telnetOption                           = 0;
    private int telnetSBIndex                           = 0;
    private byte[] telnetSBBuffer                       = new byte[MAX_ARGUMENTS];

    /************************************************************************
     * TERMINAL CODES                                                       *
     ************************************************************************/

    /**
     * Start terminal command.
     * Possible escape strings:
     *    Regular expression syntax:
     *       \E: ASCII escape
     *       \c: a literal occurrence of character c
     *       c: a literal occurence of character c if c is not a special character
     *       [charset]: matches any character in the set, char1-char2
     *          means ASCII characters c, where char1 <= c <= char2
     *       expr+: 0 or 1 occurrences of expr
     *       expr*: 0 or more occurrences of expr
     *       expr1 expr2: 1 occurrence of expr1 followed by 1 occurrence of expr2
     *       (expr): same as expr
     *
     *    \E\[[?]+[0-9]*(;[0-9]*)*[^0-9;?]
     *    \E[0-4]c
     *    \E[5-9]
     *
     *    \E{([A-z]*;[0-9]*)+}
     */
    private final static byte TERMINAL_ESC              = (byte) 27;

    /*
     * For termination.
     */
    private final static byte TERMINAL_CTRL_G           = (byte) 7;

    /**
     * For collecting the content of \E\[...
     */
    private final static int MAX_ARGUMENTS              = 100;

    private byte escapeArgumentChar                     = 0;
    private String[] escapeArguments                    = new String[MAX_ARGUMENTS];
    private int escapeArgumentIndex                     = 0;
    private byte[] escapeArgumentString                 = new byte[MAX_ARGUMENTS];
    private int escapeArgumentSIndex                    = 0;

    /************************************************************************
     * IMPLEMENTATION                                                       *
     ************************************************************************/

    /**
     * Constructor makes a disconnected client.
     */
    NuprlClient(NuprlBus bus, NuprlAuthorization auth, NuprlBusPort command_port)
        throws NuprlException
    {
        // Socket interface
        try {
            socket = new Socket(auth.host, auth.port);
            stdout = socket.getInputStream();
            stdin  = new BufferedOutputStream(socket.getOutputStream());
        }
        catch(ConnectException e) {
            throw new NuprlException("ConnectException: ", e.getMessage());
        }
        catch(IOException e) {
            throw new NuprlException("IOException: ", e.getMessage());
        }

        // Join the bus
        endpoint = bus.Subscribe(this);

        // Set the port for option negotiation
        this.command_port = command_port;
        this.dst_host = command_port.getHost();
        this.dst_port = command_port.getPort();

        // Start input processing
        Thread inputThread = new InputThread();
        inputThread.start();
    }

    /**
     * Disconnect.
     */
    void Close()
    {
        if(socket != null) {
            try {
                socket.close();
            }
            catch(IOException e) {
                // Ignore it
            }
        }
        endpoint.Unsubscribe();
    }

    /*
     * Names of the codes.
     */
    String telnetCommandName(byte code)
    {
        String command;

        switch(code) {
        case NuprlConstants.TELNET_SE:
            command = "SE";
            break;
        case NuprlConstants.TELNET_NOP:
            command = "NOP";
            break;
        case NuprlConstants.TELNET_DM:
            command = "DM";
            break;
        case NuprlConstants.TELNET_BREAK:
            command = "BREAK";
            break;
        case NuprlConstants.TELNET_INTERRUPT:
            command = "INTERRUPT";
            break;
        case NuprlConstants.TELNET_ABORT_OUTPUT:
            command = "ABORT";
            break;
        case NuprlConstants.TELNET_ARE_YOU_THERE:
            command = "ARE_YOU_THERE";
            break;
        case NuprlConstants.TELNET_ERASE_CHAR:
            command = "ERASE_CHAR";
            break;
        case NuprlConstants.TELNET_ERASE_LINE:
            command = "ERASE_LINE";
            break;
        case NuprlConstants.TELNET_GO_AHEAD:
            command = "GO_AHEAD";
            break;
        case NuprlConstants.TELNET_SB:
            command = "SB";
            break;
        case NuprlConstants.TELNET_WILL:
            command = "WILL";
            break;
        case NuprlConstants.TELNET_WONT:
            command = "WONT";
            break;
        case NuprlConstants.TELNET_DO:
            command = "DO";
            break;
        case NuprlConstants.TELNET_DONT:
            command = "DONT";
            break;
        case NuprlConstants.TELNET_IAC:
            command = "IAC";
            break;
        default:
            command = "<UNKNOWN " + code + ">";
            break;
        }
        return command;
    }

    /**
     * Get the name of an option.
     */
    String telnetOptionName(byte code)
    {
        String command;

        switch(code) {
        case NuprlConstants.TELNET_OPT_BINARY:
            command = "BINARY";
            break;
        case NuprlConstants.TELNET_OPT_ECHO:
            command = "ECHO";
            break;
        case NuprlConstants.TELNET_OPT_SUPPRES_GA:
            command = "SUPPRESS_GA";
            break;
        case NuprlConstants.TELNET_OPT_STATUS:
            command = "STATUS";
            break;
        case NuprlConstants.TELNET_OPT_TIMING_MARK:
            command = "TIMING_MARK";
            break;
        case NuprlConstants.TELNET_OPT_EXOPL:
            command = "EXOPL";
            break;
        case NuprlConstants.TELNET_OPT_TTYPE:
            command = "TTYPE";
            break;
        case NuprlConstants.TELNET_OPT_EOR:
            command = "EOR";
            break;
        case NuprlConstants.TELNET_OPT_NAWS:
            command = "NAWS";
            break;
        default:
            command = "<UNKNOWN " + code + ">";
            break;
        }
        return command;
    }

    /************************************************************************
     * OUTPUT HANDLING                                                      *
     ************************************************************************/

    /**
     * This is the standard hostname for this component.
     */
    public String getHost()
    {
        return NuprlConstants.CLIENT_NAME;
    }

    /**
     * Get the port that this client owns.
     */
    public int getPort()
    {
        return endpoint.getPort();
    }

    /**
     * Bus wants some action.
     */
    public void wakeup(NuprlBusEndpoint endpoint)
    {
        try {
            NuprlBusMessage msg;
            while((msg = endpoint.getMessage()) != null) {
                NuprlToken token = msg.token;
                if(token instanceof NuprlDataToken)
                    Write(((NuprlDataToken) token).getData());
                else if(token instanceof NuprlOptionRequestToken)
                    OptionRequest(msg.src_host, msg.src_port, (NuprlOptionRequestToken) token);
                else if(token instanceof NuprlOptionResponseToken)
                    OptionResponse(msg.src_host, msg.src_port, (NuprlOptionResponseToken) token);
                else if(token instanceof NuprlOptionBlockToken)
                    OptionBlock(msg.src_host, msg.src_port, (NuprlOptionBlockToken) token);
            }
        }
        catch(IOException e) {
            // Ignore it--we'll eventually see it when we try to read
        }
    }

    /**
     * Send some data over the channel, but escape the IAC chars.
     */
    private void Write(byte[] bytes)
        throws IOException
    {
        if(NuprlDebug.debug_input)
            System.err.println("NuprlClient.Write");

        // Search for occurrences of IAC
        int start = 0;
        int end = bytes.length;
        while(true) {
            int i = start;
            while(i != end) {
                if(bytes[i] == NuprlConstants.TELNET_IAC)
                    break;
                i++;
            }

            // Send the front part of the string
            if(i != start) {
                stdin.write(bytes, start, i - start);
                if(NuprlDebug.debug_input)
                    System.err.println("Wrote: " + new String(bytes, start, i - start));
            }
            if(i == end)
                break;

            // Escape the IAC
            stdin.write(NuprlConstants.TELNET_IAC);
            stdin.write(NuprlConstants.TELNET_IAC);
            if(NuprlDebug.debug_input)
                System.err.println("Wrote: IAC IAC");
            start = i + 1;
        }
        stdin.flush();
    }

    /**
     * Set an option.
     * If the option has already been negotiated and vetoed,
     * send back the response.
     */
    private void OptionRequest(String src_host, int src_port, NuprlOptionRequestToken token)
        throws IOException
    {
        if(NuprlDebug.debug_input)
            System.err.println("NuprlClient.OptionRequest");

        boolean flag = token.flag;
        byte sent = sentOptionFlags[token.option];
        byte recv = receivedOptionFlags[token.option];
        boolean ack = recv == NuprlConstants.TELNET_WILL || recv == NuprlConstants.TELNET_WONT;

        // Check if we have already finished negotiation
        if(flag && sent == NuprlConstants.TELNET_DO && ack || !flag && sent == NuprlConstants.TELNET_DONT && ack) {
            if(NuprlDebug.debug_telnet)
                System.err.println("NuprlClient.OptionRequest: already met: IAC "
                                   + telnetCommandName(sent)
                                   + " "
                                   + telnetOptionName(token.option));

            // Reply to the sender with the final option
            endpoint.Send(src_host,
                          src_port,
                          new NuprlOptionResponseToken(token.option, recv == NuprlConstants.TELNET_WILL ? true : false));
        }
        else {
            // Send the new option
            byte reply = flag ? NuprlConstants.TELNET_DO : NuprlConstants.TELNET_DONT;
            sentOptionFlags[token.option] = reply;
            stdin.write(NuprlConstants.TELNET_IAC);
            stdin.write(reply);
            stdin.write(token.option);
            stdin.flush();

            // Print what we're doing
            if(NuprlDebug.debug_telnet) {
                System.err.println("Send: IAC "
                                   + telnetCommandName(reply)
                                   + " "
                                   + telnetOptionName(token.option));
            }
        }
    }

    /**
     * Reply to an option request.
     */
    private void OptionResponse(String src_host, int src_port, NuprlOptionResponseToken token)
        throws IOException
    {
        if(NuprlDebug.debug_input)
            System.err.println("NuprlClient.OptionResponse");

        boolean flag = token.flag;
        byte sent = sentOptionFlags[token.option];

        if((flag && sent != NuprlConstants.TELNET_WILL) || (!flag && sent != NuprlConstants.TELNET_WONT)) {
            byte reply = flag ? NuprlConstants.TELNET_WILL : NuprlConstants.TELNET_WONT;
            sentOptionFlags[token.option] = reply;
            stdin.write(NuprlConstants.TELNET_IAC);
            stdin.write(reply);
            stdin.write(token.option);
            stdin.flush();

            // Show what we did
            if(NuprlDebug.debug_telnet)
                System.err.println("Send: IAC " + telnetCommandName(reply) + " " + telnetOptionName(token.option));
        }
        else if(NuprlDebug.debug_telnet)
            System.err.println("Not sent: IAC " + (flag ? "WILL " : "WONT ") + telnetOptionName(token.option));
    }

    /**
     * Send an option block.
     * The option must be enabled.
     */
    private void OptionBlock(String src_host, int src_port, NuprlOptionBlockToken token)
        throws IOException
    {
        if(NuprlDebug.debug_input)
            System.err.println("NuprlClient.OptionBlock from " + src_host + "." + src_port);

        byte sent = sentOptionFlags[token.option];
        byte recv = receivedOptionFlags[token.option];

        if((sent == NuprlConstants.TELNET_WILL && recv == NuprlConstants.TELNET_DO) || (sent == NuprlConstants.TELNET_DO && recv == NuprlConstants.TELNET_WILL)) {
            // Send the block
            stdin.write(NuprlConstants.TELNET_IAC);
            stdin.write(NuprlConstants.TELNET_SB);
            byte[] data = token.formatData();
            stdin.write(data, 0, data.length);
            stdin.write(NuprlConstants.TELNET_IAC);
            stdin.write(NuprlConstants.TELNET_SE);
            stdin.flush();

            if(NuprlDebug.debug_telnet) {
                System.err.println("Send: IAC SB "
                                   + new String(data, 0, data.length)
                                   + " IAC SE");
            }
        }
        else if(NuprlDebug.debug_telnet)
            System.err.println("Option not set: " + telnetOptionName(token.option));
    }

    /************************************************************************
     * INPUT                                                                *
     ************************************************************************/

    /**
     * Send some data to the bus.
     */
    private void Send(byte[] buffer, int start, int end)
    {
        endpoint.Send(dst_host, dst_port, new NuprlDataToken(buffer, start, end - start));
    }

    /**
     * Send a single byte.
     */
    private void Send(byte b)
    {
        endpoint.Send(dst_host, dst_port, new NuprlDataToken(b));
    }

    /**
     * Send an escape code.
     */
    private void SendEscapedArguments(byte c)
    {
        endpoint.Send(dst_host, dst_port, new NuprlArgumentToken(escapeArgumentChar,
                                                                 escapeArguments,
                                                                 escapeArgumentIndex,
                                                                 c));
    }

    /**
     * Send a control character.
     */
    private void SendControlCharacter(byte c)
    {
        endpoint.Send(dst_host, dst_port, new NuprlControlToken(c));
    }

    /**
     * Send an escaped character.
     */
    private void SendEscapedCharacter(byte c)
    {
        endpoint.Send(dst_host, dst_port, new NuprlEscapedToken(c));
    }

    /**
     * Send the sync token.
     */
    private void SendSync()
    {
        endpoint.Send(dst_host, dst_port, new NuprlSyncToken());
    }

    /**
     * Change the current host/port.
     */
    private void SetDestination()
    {
        SendSync();
        if(escapeArgumentIndex >= 2) {
            dst_host = escapeArguments[0];
            try {
                dst_port = Integer.parseInt(escapeArguments[1]);
            }
            catch(NumberFormatException e) {
                System.err.println("NuprlClient.SetDestination: not a number: " + escapeArguments[1]);
            }
        }
    }

    /**
     * Broadcast an option notification.
     */
    private void SendOption(byte c)
    {
        switch(telnetOption) {
        case NuprlConstants.TELNET_WILL:
            endpoint.Send(command_port, new NuprlOptionResponseToken(c, true));
            break;
        case NuprlConstants.TELNET_WONT:
            endpoint.Send(command_port, new NuprlOptionResponseToken(c, false));
            break;
        case NuprlConstants.TELNET_DO:
            endpoint.Send(command_port, new NuprlOptionRequestToken(c, true));
            break;
        case NuprlConstants.TELNET_DONT:
            endpoint.Send(command_port, new NuprlOptionRequestToken(c, false));
            break;
        }
    }

    /**
     * Broadcast the sub-negotiation.
     */
    private void SendSBOption()
    {
        endpoint.Send(command_port, new NuprlOptionSBToken(telnetSBBuffer, 0, telnetSBIndex));
    }

    /*
     * State is not normal data.
     */
    private void Parse(byte[] buffer, int end)
    {
        int start = 0;
    loop:
        while(start != end) {
            int i = start;

            // Optimize the normal state of collecting data
            if(state == STATE_DATA) {
                // Look for the next special byte
                while(i != end) {
                    byte c = buffer[i];
                    if(c == NuprlConstants.TELNET_IAC || c < ' ')
                        break;
                    i++;
                }

                // Found any input?
                if(i != start) {
                    Send(buffer, start, i);
                    start = i;
                }
            }

            // Parse special bytes
            while(i != end) {
                byte c = buffer[i];

                switch(state) {
                case STATE_DATA:
                    // Normal state
                    if(c == NuprlConstants.TELNET_IAC) {
                        if(NuprlDebug.debug_telnet)
                            System.err.println("IAC");
                        state = STATE_IAC;
                    }
                    else if(c == TERMINAL_ESC) {
                        if(NuprlDebug.debug_terminal)
                            System.err.println("ESC");
                        state = STATE_ESC;
                    }
                    else if(c < ' ') {
                        if(NuprlDebug.debug_terminal)
                            System.err.println("CONTROL-" + (char) (c + '@') + " " + c);
                        SendControlCharacter(c);
                        state = STATE_DATA;
                    }
                    else {
                        // Jump to fast data parser
                        start = i;
                        continue loop;
                    }
                    break;

                /************************************************************************
                 * TERMINAL PARSING                                                     *
                 ************************************************************************/

                case STATE_ESC:
                    // Initialize escape buffers
                    escapeArgumentChar = c;
                    escapeArgumentIndex = 0;
                    escapeArgumentSIndex = 0;

                    // Choose case
                    switch(c) {
                    case '[':
                        if(NuprlDebug.debug_terminal)
                            System.err.println("ESC [");
                        state = STATE_ESC_LBRACK;
                        break;

                    case ']':
                        if(NuprlDebug.debug_terminal)
                            System.err.println("ESC ]");
                        state = STATE_ESC_RBRACK;
                        break;

                    case '{':
                        if(NuprlDebug.debug_terminal)
                            System.err.println("ESC {");
                        state = STATE_ESC_HOST;
                        break;

                    case '0':
                    case '1':
                        if(NuprlDebug.debug_terminal)
                            System.err.println("ESC " + (char) c);
                        state = STATE_ESC_DIGIT;
                        break;

                    default:
                        if(NuprlDebug.debug_terminal)
                            System.err.println("ESC " + (char) c + " <done>");
                        SendEscapedCharacter(c);
                        state = STATE_DATA;
                        break;
                    }
                    break;

                case STATE_ESC_LBRACK:
                    // Parsing \E[ arguments
                    if(NuprlDebug.debug_terminal)
                        System.err.println("ESC [ ... " + (char) c);

                    if(c >= '0' && c <= '9') {
                        if(escapeArgumentSIndex < MAX_ARGUMENTS)
                            escapeArgumentString[escapeArgumentSIndex++] = c;
                    }
                    else if(c == '?' || c == ']' || c == '[')
                        escapeArgumentChar = c;
                    else {
                        // Push the argument
                        if(escapeArgumentIndex < MAX_ARGUMENTS) {
                            escapeArguments[escapeArgumentIndex++] =
                                new String(escapeArgumentString, 0, escapeArgumentSIndex);
                        }
                        escapeArgumentSIndex = 0;

                        // May terminate the block
                        if(c != ';') {
                            SendEscapedArguments(c);
                            state = STATE_DATA;
                        }
                    }
                    break;

                case STATE_ESC_RBRACK:
                    // Parsing \E[ arguments
                    if(NuprlDebug.debug_terminal)
                        System.err.println("ESC ] ... " + (char) c);

                    if(c != ';' && c != TERMINAL_CTRL_G) {
                        if(escapeArgumentSIndex < MAX_ARGUMENTS)
                            escapeArgumentString[escapeArgumentSIndex++] = c;
                    }
                    else {
                        // Push the argument
                        if(escapeArgumentIndex < MAX_ARGUMENTS) {
                            escapeArguments[escapeArgumentIndex++] =
                                new String(escapeArgumentString, 0, escapeArgumentSIndex);
                        }
                        escapeArgumentSIndex = 0;

                        // May terminate the block
                        if(c != ';') {
                            SendEscapedArguments(c);
                            state = STATE_DATA;
                        }
                    }
                    break;

                case STATE_ESC_HOST:
                    // Parsing \E{ arguments
                    if(c >= '0' && c <= '9' || c >= 'A' && c <= 'Z' || c >= 'a' && c <= 'z') {
                        if(escapeArgumentSIndex < MAX_ARGUMENTS)
                            escapeArgumentString[escapeArgumentSIndex++] = c;
                    }
                    else {
                        // Push the argument
                        if(escapeArgumentIndex < MAX_ARGUMENTS) {
                            escapeArguments[escapeArgumentIndex++] =
                                new String(escapeArgumentString, 0, escapeArgumentSIndex);
                        }
                        escapeArgumentSIndex = 0;

                        // May terminate the block
                        if(c != ';') {
                            SetDestination();
                            state = STATE_DATA;
                        }
                    }
                    break;

                case STATE_ESC_DIGIT:
                    // Terminated by a single byte
                    SendEscapedArguments(c);
                    state = STATE_DATA;
                    break;

                /************************************************************************
                 * TELNET PARSING                                                       *
                 ************************************************************************/

                case STATE_IAC:
                    // Begin telnet negotiation option
                    switch(c) {
                    case NuprlConstants.TELNET_IAC:
                        // Doubled IAC sends and IAC
                        Send(NuprlConstants.TELNET_IAC);
                        state = STATE_DATA;
                        break;
                    case NuprlConstants.TELNET_WILL:
                        telnetOption = NuprlConstants.TELNET_WILL;
                        state = STATE_IAC_OPTION;
                        break;
                    case NuprlConstants.TELNET_WONT:
                        telnetOption = NuprlConstants.TELNET_WONT;
                        state = STATE_IAC_OPTION;
                        break;
                    case NuprlConstants.TELNET_DONT:
                        telnetOption = NuprlConstants.TELNET_DONT;
                        state = STATE_IAC_OPTION;
                        break;
                    case NuprlConstants.TELNET_DO:
                        telnetOption = NuprlConstants.TELNET_DO;
                        state = STATE_IAC_OPTION;
                        break;
                    case NuprlConstants.TELNET_SB:
                        telnetSBIndex = 0;
                        state = STATE_IAC_SB;
                        break;
                    default:
                        // NOP, DM, BREAK, INTERRUPT, ABORT, ARE_YOU_THERE, ERASE_CHAR, ERASE_LINE, GO_AHEAD, ...
                        state = STATE_DATA;
                        break;
                    }

                    // Show negotiation
                    if(NuprlDebug.debug_telnet)
                        System.err.println("IAC " + telnetCommandName(c));
                    break;

                case STATE_IAC_OPTION:
                    // Show negotiation
                    if(NuprlDebug.debug_telnet)
                        System.err.println("IAC " + telnetCommandName(telnetOption) + " " + telnetOptionName(c));

                    // Send the option command to destination 0
                    SendOption(c);
                    receivedOptionFlags[c] = telnetOption;
                    if(telnetOption == NuprlConstants.TELNET_DO || telnetOption == NuprlConstants.TELNET_DONT)
                        sentOptionFlags[c] = 0;

                    // Back to data mode
                    state = STATE_DATA;
                    break;

                case STATE_IAC_SB:
                    // Show negotiation
                    if(NuprlDebug.debug_telnet)
                        System.err.println("IAC SB " + c);

                    switch(c) {
                    case NuprlConstants.TELNET_IAC:
                        // IAC codes are escaped in the body of the subnegotiation
                        state = STATE_IAC_SB_IAC;
                        break;

                    default:
                        // This should be a telnet option
                        if(telnetSBIndex < MAX_ARGUMENTS)
                            telnetSBBuffer[telnetSBIndex++] = c;
                        break;
                    }
                    break;

                case STATE_IAC_SB_IAC:
                    switch(c) {
                    case NuprlConstants.TELNET_IAC:
                        // IAC has been escaped
                        if(NuprlDebug.debug_telnet)
                            System.err.println("<SB> IAC IAC");
                        if(telnetSBIndex < MAX_ARGUMENTS)
                            telnetSBBuffer[telnetSBIndex++] = NuprlConstants.TELNET_IAC;
                        break;

                    case NuprlConstants.TELNET_SE:
                        // Subnegotiation is terminated
                        if(NuprlDebug.debug_telnet)
                            System.err.println("SB: " + new String(telnetSBBuffer, 0, telnetSBIndex));
                        SendSBOption();
                        state = STATE_DATA;
                        break;

                    default:
                        // This should not happen
                        break;
                    }
                    break;

                default:
                    System.err.println("NuprlClient: illegal state: " + state);
                    state = STATE_DATA;
                    break;
                }

                // Advance to next byteacter
                i++;
            }

            // If loop exits, we have parsed it all
            start = i;
        }
    }

    /**
     * Input is read by a reader thread.
     */
    class InputThread
    extends Thread
    {
        public void run()
        {
            byte[] buffer = new byte[BUFFER_SIZE];

            try {
                if(NuprlDebug.debug_input)
                    System.err.println("NuprlClient.InputThread: starting");
                while(true) {
                    // Read some characters
                    if(NuprlDebug.debug_input)
                       System.err.println("NuprlClient.InputThread: reading");
                    int count = stdout.read(buffer, 0, BUFFER_SIZE);
                    if(count <= 0)
                        break;

                    if(NuprlDebug.debug_input)
                        System.err.println("NuprlClient.InputThread(" + count + ") String = " + new String(buffer, 0, count));
                    Parse(buffer, count);
                }
            }
            catch(IOException e) {
                System.err.println("Read: " + e.getMessage());
            }
            finally {
                Close ();
            }
        }
    }
}
