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
}
