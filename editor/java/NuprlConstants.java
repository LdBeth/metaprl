/*
 * Global constants.
 *
 * ----------------------------------------------------------------
 *
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/index.html for information on Nuprl,
 * OCaml, and more information about this system.
 *
 * Copyright (C) 1999 Jason Hickey, Cornell University
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * Author: Jason Hickey
 * jyh@cs.cornell.edu
 */

package edu.cornell.cs.jyh.nuprl;

class NuprlConstants
{
    /**
     * Name of the command window.
     */
    static final String COMMAND_NAME    = "NuprlCommand";

    /**
     * Name of the client component.
     */
    static final String CLIENT_NAME     = "NuprlClient";

    /**
     * Name of a menu.
     */
    static final String MENU_NAME       = "NuprlMenu";

    /**
     * Name of a goal window.
     */
    static final String GOAL_NAME       = "NuprlGoal";

    /**
     * Name of a rule box window.
     */
    static final String RULE_NAME       = "NuprlRule";

    /**
     * Name of a subgoals window.
     */
    static final String SUBGOALS_NAME   = "NuprlSubgoals";

    /**
     * Name of the manager component.
     */
    static final String CONTROL_NAME    = "NuprlControl";

    /**
     * Control code for setting the base URL in a window.
     */
    static final int METAPRL_BASE_CODE  = 0;

    /**
     * Control code for setting the base MetaPRL directory in a window.
     */
    static final int METAPRL_DIR_CODE   = 1;

    /**
     * Manifest hostname for changing the MetaPRL directory.
     */
    static final String METAPRL_CD_HOST = "cd.metaprl.local";

    /**
     * Manifest hostname for sending a command to MetaPRL.
     */
    static final String METAPRL_COMMAND_HOST = "command.metaprl.local";

    /************************************************************************
     * TELNET STATE                                                         *
     ************************************************************************/

    /**
     * End-of-subnegotiation.
     */
    final static byte TELNET_SE                         = (byte) 240;

    /**
     * No-operation.
     */
    final static byte TELNET_NOP                        = (byte) 241;

    /**
     * Data-mark.
     */
    final static byte TELNET_DM                         = (byte) 242;

    /**
     * Break.
     */
    final static byte TELNET_BREAK                      = (byte) 243;

    /**
     * Interrupt process.
     */
    final static byte TELNET_INTERRUPT                  = (byte) 244;

    /**
     * Abort output.
     */
    final static byte TELNET_ABORT_OUTPUT               = (byte) 245;

    /**
     * Are you there?
     */
    final static byte TELNET_ARE_YOU_THERE              = (byte) 246;

    /**
     * Erase a single byteacter.
     */
    final static byte TELNET_ERASE_CHAR                 = (byte) 247;

    /**
     * Erase an entire line.
     */
    final static byte TELNET_ERASE_LINE                 = (byte) 248;

    /**
     * Go ahead and do something.
     */
    final static byte TELNET_GO_AHEAD                   = (byte) 249;

    /**
     * Start subnegotiation.
     */
    final static byte TELNET_SB                         = (byte) 250;

    /**
     * Request to do something.
     * Will do it if not rejected.
     */
    final static byte TELNET_WILL                       = (byte) 251;

    /**
     * Require not to do something.
     */
    final static byte TELNET_WONT                       = (byte) 252;

    /**
     * Please do something.
     */
    final static byte TELNET_DO                         = (byte) 253;

    /**
     * Require not to do something.
     */
    final static byte TELNET_DONT                       = (byte) 254;

    /**
     * This is the telnet escape code.
     */
    final static byte TELNET_IAC                        = (byte) 255;

    /**
     * Transmit data in binary option.
     * IAC WILL BINARY: request to send data in binary
     * IAC WONT BINARY: force ASCII only
     * IAC DO   BINARY: request to send data in binary
     * IAC DONT BINARY: force ASCII only
     */
    final static byte TELNET_OPT_BINARY                 = (byte) 0;

    /**
     * Request server to echo input lines.
     *
     * IAC WILL ECHO: request the server to echo
     * IAC WONT ECHO: force the server not to echo
     * IAC DO   ECHO: request the server to echo
     * IAC DONT ECHO: force the server not to echo
     */
    final static byte TELNET_OPT_ECHO                   = (byte) 1;

    /**
     * Supress go-ahead requests.
     *
     * IAC WILL SUPRESS_GA: request to supress go-ahead
     * IAC WONT SUPRESS_GA: will not supress go-ahead
     * IAC DO   SUPRESS_GA: please supress go-ahead
     * IAC DONT SUPRESS_GA: will not supress go-ahead
     */
    final static byte TELNET_OPT_SUPPRES_GA             = (byte) 3;

    /**
     * Discuss status of options.
     * IAC DONT STATUS: dont discuss options
     * IAC WONT STATUS: wont discuss options
     * IAC SB STATUS SEND IAC SE: please send options
     * IAC SB STATUS IS (WILL option | WONT option) IAS SE
     *
     * This is so complex, we dont handle it.
     */
    final static byte TELNET_OPT_STATUS                 = (byte) 5;
    final static byte TELNET_OPT_SB_SEND                = (byte) 1;
    final static byte TELNET_OPT_SB_IS                  = (byte) 0;

    /**
     * Timing mark.
     * This is used to insert a timing mark at the "appropriate" place.
     * All we do is request the owner to create a timing mark.
     *
     * IAC DO   TIMING_MARK: request a timing mark
     * IAC WILL TIMING_MARK: this is the timing mark that was requested
     * IAC WONT TIMING_MARK: the request is refused
     * IAC DONT TIMING_MARK: timing mark has been ignored
     */
    final static byte TELNET_OPT_TIMING_MARK            = (byte) 6;

    /**
     * Extended options.
     *
     * IAC DO   EXOPL: please negotiate extended options
     * IAC WILL EXOPL: will nogotiate extended options
     * IAC WONT EXOPL: wont negotiate extended options
     * IAC SB   EXOPL subcommand
     * subcommand ::= (DO | DONT | WILL | WONT) option IAC SE
     *             |  SB <option code> <parameters> SE IAC SE
     */
    final static byte TELNET_OPT_EXOPL                  = (byte) 255;

    /**
     * Terminal type.
     *
     * IAC WILL TTYPE: willing to negotiate terminal type
     * IAC DO   TTYPE: please send terminal type
     * IAC DONT TTYPE: dont send terminal type
     * IAC WONT TTYPE: wont send terminal type
     * IAC SB   TTYPE SEND IAC SE: please send terminal type
     * IAC SB   TTYPE IS <ascii terminal name ignore case> IAC SE
     */
    final static byte TELNET_OPT_TTYPE                  = (byte) 24;

    /**
     * End of "record".
     * A record is some unit of data.
     *
     * IAC WILL EOR: will send end-of-record when encountered
     * IAC WONT EOR: wont send end-of-record
     * IAC DO   EOR: please send edn-of-record
     * IAC DONT EOR: dont bother me with this crap
     */
    final static byte TELNET_OPT_EOR                    = (byte) 25;

    /**
     * Window size.
     *
     * IAC WILL NAWS: please accept window size info
     * IAC WONT NAWS: will not send window size
     * IAC DO   NAWS: please send window size info
     * IAC DONT NAWS: dont send this crap
     * IAC SB NAWS <width : 16bits> <height : 16bits> IAC SE
     *
     * if IAC occurs in 16bits, it must be doubled as usual.
     */
    final static byte TELNET_OPT_NAWS                   = (byte) 31;

}
