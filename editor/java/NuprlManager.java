/**
 * This is the window manager.
 */

package edu.cornell.cs.jyh.nuprl;

import java.awt.*;
import java.awt.event.*;
import java.beans.*;

import javax.swing.*;

class NuprlManager
{
    /**
     * The manager has several states that it can be in.
     */
    protected int state                         = STATE_LOGIN;

    protected final static int STATE_LOGIN      = 0;
    protected final static int STATE_COMMAND    = 1;
    protected final static int STATE_MENU       = 2;
    protected final static int STATE_PROOF      = 3;

    /**
     * Perform auto-layout of windows.
     */
    protected boolean auto_layout               = true;

    /**
     * Window layout constants.
     */
    protected final static int MARGIN_WIDTH     = 5;

    protected final static int MENU_OFFSET      = 32;
    protected final static int MENU_WIDTH       = 200;
    protected final static int MIN_MENU_WIDTH   = 200;
    protected final static int MIN_MENU_HEIGHT  = 200;

    protected final static int PROOF_OFFSET     = 32;
    protected final static int MIN_PROOF_WIDTH  = 400;
    protected final static int MIN_PROOF_HEIGHT = 400;

    protected final static int COMMAND_HEIGHT   = 200;

    /*
     * Keep a handle to the context to handle global operations.
     */
    protected NuprlContext context;

    /**
     * Communication is over the NuprlBus.
     */
    protected NuprlBus bus;

    /**
     * The login window prompts for the hostname/port,
     * and saves the login info in the authorization.
     */
    protected NuprlAuthorization auth;
    protected NuprlHost login;

    /**
     * The telnet connection to the client.
     */
    protected NuprlClient client;

    /**
     * The command window is the main telnet window.
     */
    protected static final int DEFAULT_COMMAND_PORT     = 1;

    protected NuprlCommand command;

    /*
     * The menu windows display information.
     */
    protected static final int DEFAULT_MENU_PORT        = 1;

    protected NuprlMenu[] menus                         = new NuprlMenu[2];

    /**
     * There can be several proof windows.
     */
    protected NuprlProof[] proofs                       = new NuprlProof[2];

    /**
     * Destination of control requests.
     */
    protected static final int DEFAULT_CONTROL_PORT     = 1;

    protected NuprlControl control;

    /**
     * Target of login button.
     */
    protected static final String login_command         = "login";

    /************************************************************************
     * IMPLEMENTATION                                                       *
     ************************************************************************/

    /*
     * On initialization, create all the windows.
     */
    NuprlManager(NuprlContext context, NuprlAuthorization auth)
    {
        // Save authorization
        this.auth = auth;
        this.context = context;

        // Watch the window size
        context.getDesktop().addComponentListener(new NuprlComponentListener());
    }

    void start()
    {
        if(NuprlDebug.debug_manager)
            System.err.println("NuprlManager.start: begin");

        // Start the communication channel
        bus = new NuprlBus();
        bus.Manage(new NuprlClientManager());
        bus.Manage(new NuprlCommandManager());
        bus.Manage(new NuprlMenuManager());
        bus.Manage(new NuprlProofManager(NuprlConstants.GOAL_NAME));
        bus.Manage(new NuprlProofManager(NuprlConstants.RULE_NAME));
        bus.Manage(new NuprlProofManager(NuprlConstants.SUBGOALS_NAME));
        bus.Manage(new NuprlControlManager());

        // Do we need to ask for the host info
        synchronized (bus) {
            if(auth.isHostComplete()) {
                try {
                    state = STATE_COMMAND;

                    if(NuprlDebug.debug_manager)
                        System.err.println("NuprlManager.start: initializing NuprlClient");

                    client = new NuprlClient(bus, auth, new NuprlCommandManager());
                }
                catch(NuprlException e) {
                    System.err.println("NuprlManager.NuprlLoginTarget.performCommand: " + e.stringOfException());
                    state = STATE_LOGIN;
                }
            }
            else
            {
                state = STATE_LOGIN;
                if (login == null) placeLogin();
            }
        }

        if(NuprlDebug.debug_manager)
            System.err.println("NuprlManager.start: end");
    }

    /**
     * Do the layout.
     * There are two modes:
     *   in command mode, the command window is made as big as possible
     *   in proof mode, the windows are paneled.
     */
    protected synchronized void autoLayout()
    {
        if(NuprlDebug.debug_manager)
            System.err.println("NuprlManager.autoLayout: begin");

        JDesktopPane desktop = context.getDesktop();
        Dimension size = desktop.getSize();
        if (bus == null) {
            if(NuprlDebug.debug_manager)
               System.err.println("NuprlManager.autoLayout: bus is null");
            return;
        }
        switch(state) {
        case STATE_LOGIN:
            autoLayoutLogin(size);
            break;
        case STATE_COMMAND:
            autoLayoutCommand(size);
            break;
        case STATE_MENU:
            autoLayoutMenu(size);
            break;
        case STATE_PROOF:
            autoLayoutProof(size);
            break;
        default:
            System.err.println("NuprlManager: illegal state " + state);
            break;
        }

        if(NuprlDebug.debug_manager)
            System.err.println("NuprlManager.autoLayout: end");
    }

    /**
     * Layout the login window.
     */
    protected void autoLayoutLogin(Dimension size)
    {
        iconifyMenu();
        iconifyProofs();
        expandCommand(size);
        placeLogin();
    }

    /**
     * Layout for command mode.
     */
    protected void autoLayoutCommand(Dimension size)
    {
        iconifyMenu();
        iconifyProofs();
        expandCommand(size);
        closeLogin();
    }

    /**
     * Layout for proof mode.
     */
    protected void autoLayoutMenu(Dimension size)
    {
        iconifyMenu();
        iconifyProofs();
        placeCommand(size);
        closeLogin();
    }

    /**
     * Layout for proof mode.
     */
    protected void autoLayoutProof(Dimension size)
    {
        placeMenu(size);
        placeProofs(size);
        placeCommand(size);
        closeLogin();
    }

    /**
     * Close the login window.
     */
    protected void closeLogin()
    {
        if(login != null && login.isDead() == false) {
            try {
                login.setClosed(true);
            }
            catch(PropertyVetoException e) {
                // Ignore it
            }
        }
        login = null;
    }

    /**
     * Place the login window.
     * This will open a new login window if necessary.
     */
    protected void placeLogin()
    {
        if(login == null || login.isDead()) {
            login = new NuprlHost(auth);
            login.setTarget(login_command, new NuprlLoginTarget());
            context.getDesktop().add(login);
        }
        login.setLocation(MARGIN_WIDTH, MARGIN_WIDTH);
        login.moveToFront();
    }

    /**
     * Iconify the menu if it exists.
     */
    protected void iconifyMenu()
    {
        NuprlMenu menu = menus[DEFAULT_MENU_PORT];
        if(menu != null) {
            if(menu.isDead())
                menu = null;
            else {
                try {
                    menu.setIcon(true);
                }
                catch(PropertyVetoException e) {
                    // Ignore it
                }
            }
        }
    }

    /**
     * Place the menu at the standard position.
     * Create a new menu if necessary.
     */
    protected void placeMenu(Dimension size)
    {
        if(NuprlDebug.debug_manager)
            System.err.println("NuprlManager.placeMenu: standard menu");

        // Place it if it does not already exist
        NuprlMenu menu = menus[DEFAULT_MENU_PORT];
        if(menu == null || menu.isDead()) {
            try {
                menu = new NuprlMenu(bus, DEFAULT_MENU_PORT);
            }
            catch(NuprlException e) {
                System.err.println("NuprlManager.placeMenu: " + e.stringOfException());
                System.exit(1);
            }
            menu.setClient(new NuprlControlManager());
            context.getDesktop().add(menu);
            menus[DEFAULT_MENU_PORT] = menu;
            menu.toFront();
        }

        // Set up the window
        menu.setLocation(MARGIN_WIDTH, MARGIN_WIDTH);
        menu.setSize(MENU_WIDTH, size.height - COMMAND_HEIGHT - 3 * MARGIN_WIDTH);
    }

    /**
     * Place a single menu window.  This will open a new window
     * if necessary.
     */
    protected void placeMenu(int port)
    {
        if(NuprlDebug.debug_manager)
            System.err.println("NuprlManager.placeMenu: " + port);

        JDesktopPane desktop = context.getDesktop();
        Dimension size = desktop.getSize();

        // Default menu is handled specially
        if(port == DEFAULT_MENU_PORT) {
            placeMenu(size);
            return;
        }

        // Enlarge the menu array if necessary
        if(port >= menus.length) {
            NuprlMenu[] new_menus = new NuprlMenu[port * 2];
            System.arraycopy(menus, 0, new_menus, 0, menus.length);
            menus = new_menus;
        }

        // Check the window
        NuprlMenu menu = menus[port];
        if(menu == null || menu.isDead()) {
            try {
                menu = new NuprlMenu(bus, port);
            }
            catch(NuprlException e) {
                System.err.println("NuprlManager.placeMenu: " + e.stringOfException());
                System.exit(1);
            }
            menu.setClient(new NuprlControlManager());
            menus[port] = menu;

            // Count up the number of existing menus
            int count = 0;
            for(int i = 2; i < menus.length; i++) {
                NuprlMenu menu2 = menus[i];
                if(menu2 != null) {
                    if(menu2.isDead())
                        menus[i] = null;
                    else
                        count++;
                }
            }

            // Place the window
            int width = size.width - 2 * MARGIN_WIDTH - count * MENU_OFFSET;
            int height = size.height - COMMAND_HEIGHT - 3 * MARGIN_WIDTH;
            if(width < MIN_MENU_WIDTH)
                width = MIN_MENU_WIDTH;
            if(height < MIN_MENU_HEIGHT)
                height = MIN_MENU_HEIGHT;
            menu.setLocation(MARGIN_WIDTH + count * MENU_OFFSET, MARGIN_WIDTH);
            menu.setSize(width, height);
            desktop.add(menu);
            menu.toFront();
        }
        else if(menu.isIcon()) {
            try {
                menu.setIcon(false);
            }
            catch(PropertyVetoException e) {
                // Ignore it
            }
        }
    }

    /**
     * Iconify all the proof windows.
     */
    protected void iconifyProofs()
    {
        for(int i = 0; i != proofs.length; i++) {
            NuprlProof proof = proofs[i];
            if(proof != null) {
                if(proof.isDead())
                    proofs[i] = null;
                else {
                    try {
                        proof.setIcon(true);
                    }
                    catch(PropertyVetoException e) {
                        // Ignore it
                    }
                }
            }
        }
    }

    /**
     * Place all the existing proof windows.
     */
    protected void placeProofs(Dimension size)
    {
        // Count up number of proof windows
        int count = 0;
        for(int i = 0; i != proofs.length; i++) {
            NuprlProof proof = proofs[i];
            if(proof != null) {
                if(proof.isDead())
                    proofs[i] = null;
                else
                    count++;
            }
        }

        // Calculate the sizes of the windows
        int width = size.width - MENU_WIDTH - 4 * MARGIN_WIDTH - (count - 1) * PROOF_OFFSET;
        int height = size.height - COMMAND_HEIGHT - 3 * MARGIN_WIDTH - (count - 1) * PROOF_OFFSET;
        if(width < MIN_PROOF_WIDTH)
            width = MIN_PROOF_WIDTH;
        if(height < MIN_PROOF_HEIGHT)
            height = MIN_PROOF_HEIGHT;

        // Now place them
        int left = MENU_WIDTH + 2 * MARGIN_WIDTH;
        int top = MARGIN_WIDTH;
        for(int i = 0; i != proofs.length; i++) {
            NuprlProof proof = proofs[i];
            if(proof != null) {
                proof.setSize(width, height);
                proof.setLocation(left, top);
                if(proof.isIcon()) {
                    try {
                        proof.setIcon(false);
                    }
                    catch(PropertyVetoException e) {
                        // Ignore it
                    }
                }
                left += PROOF_OFFSET;
                top += PROOF_OFFSET;
            }
        }
    }

    /**
     * Place a single proof window.  This will open a new window
     * if necessary.
     */
    protected void placeProof(int port)
    {
        if(NuprlDebug.debug_manager)
            System.err.println("NuprlManager.placeProof: " + port);

        // Enlarge the proof array if necessary
        if(port >= proofs.length) {
            NuprlProof[] new_proofs = new NuprlProof[port * 2];
            System.arraycopy(proofs, 0, new_proofs, 0, proofs.length);
            proofs = new_proofs;
        }

        // Check the window
        NuprlProof proof = proofs[port];
        if(proof == null || proof.isDead()) {
            JDesktopPane desktop = context.getDesktop();
            Dimension size = desktop.getSize();
            try {
                proof = new NuprlProof(bus, port);
                proofs[port] = proof;
            }
            catch(NuprlException e) {
                System.err.println("NuprlManager.placeProof: " + e.stringOfException());
                System.exit(1);
            }
            proof.setClient(new NuprlControlManager());

            // Place the window
            int width = size.width - MENU_WIDTH - 4 * MARGIN_WIDTH;
            int height = size.height - COMMAND_HEIGHT - 3 * MARGIN_WIDTH;
            if(width < MIN_PROOF_WIDTH)
                width = MIN_PROOF_WIDTH;
            if(height < MIN_PROOF_HEIGHT)
                height = MIN_PROOF_HEIGHT;
            proof.setLocation(MENU_WIDTH + 2 * MARGIN_WIDTH, MARGIN_WIDTH);
            proof.setSize(width, height);
            desktop.add(proof);

            proof.toFront();
        }
    }

    /**
     * Makes sure command is non-null
     */
    private void ensureCommandExists()
    {
        // Place it if it does not already exist
        if(command == null || command.isDead()) {
            System.err.println("New command window");
            try {
                command = new NuprlCommand(bus, DEFAULT_COMMAND_PORT);
            }
            catch(NuprlException e) {
                System.err.println("NuprlManager.ensureCommandExists: " + e.stringOfException());
                System.exit(1);
            }
            command.setClient(new NuprlClientManager());
            context.getDesktop().add(command);
        }
    }

    /**
     * Place the command window in its normal size.
     */
    protected void placeCommand(Dimension size)
    {
        ensureCommandExists();

        // Small window
        command.setLocation(MARGIN_WIDTH, size.height - COMMAND_HEIGHT - MARGIN_WIDTH);
        command.setSize(size.width - 2 * MARGIN_WIDTH, COMMAND_HEIGHT);
    }

    /**
     * Place the command window full size.
     */
    protected void expandCommand(Dimension size)
    {
        ensureCommandExists();

        // Bigger window
        command.setLocation(MARGIN_WIDTH, MARGIN_WIDTH);
        command.setSize(size.width - 2 * MARGIN_WIDTH, size.height - 2 * MARGIN_WIDTH);
    }

    /************************************************************************
     * MANAGERS                                                             *
     ************************************************************************/

    /**
     * Handle the login request.
     */
    class NuprlLoginTarget
    implements Target
    {
        public void performCommand(String cmd, Object obj)
        {
            if(client != null)
                client.Close();
            try {
                ensureCommandExists();
                command.setVisible(true);
                client = new NuprlClient(bus, auth, new NuprlCommandManager());
            }
            catch(NuprlException e) {
                System.err.println("NuprlManager.NuprlLoginTarget.performCommand: " + e.stringOfException());
                System.exit(1);
            }
            state = STATE_COMMAND;
            autoLayout();
        }
    }

    /**
     * The client manager only gets messages when the client connection
     * fails.  If it does, we have to back off to the connection state.
     * We can drop the messages.
     */
    class NuprlControlManager
        implements NuprlBusClient, NuprlBusPort
    {
        /**
         * We handle messages to the command window.
         */
        public String getHost()
        {
            return NuprlConstants.CONTROL_NAME;
        }

        /**
         * The default port.
         */
        public int getPort()
        {
            return 1;
        }

        /**
         * what to do when we are called.
         */
        public void wakeup(NuprlBusEndpoint endpt)
        {
            if(NuprlDebug.debug_manager)
                System.err.println("NuprlManager.NuprlControlEvent.run: begin");

            // Create the control window
            synchronized (bus) {
                if(control == null) {
                    try {
                        control = new NuprlControl(bus, DEFAULT_CONTROL_PORT);
                    }
                    catch(NuprlException e) {
                        System.err.println("NuprlManager.NuprlControlEvent.run: " + e.stringOfException());
                        System.exit(1);
                    }
                }

                // Forward messages
                NuprlBusMessage msg;
                while((msg = endpt.getMessage()) != null) {
                    try {
                        endpt.Forward(msg);
                    }
                    catch(NuprlException e) {
                        System.err.println("NuprlControlEvent.run.Forward: " + e.stringOfException());
                        System.exit(1);
                    }
                }
            }

            if(NuprlDebug.debug_manager)
                System.err.println("NuprlManager.NuprlControlEvent.run: end");
        }
    }

    /**
     * The client manager only gets messages when the client connection
     * fails.  If it does, we have to back off to the connection state.
     * We can drop the messages.
     */
    class NuprlClientManager
        implements NuprlBusClient, NuprlBusPort
    {
        /**
         * We handle messages to the command window.
         */
        public String getHost()
        {
            return NuprlConstants.CLIENT_NAME;
        }

        /**
         * Handle messages at this port.
         */
        public int getPort()
        {
            return 1;
        }

        /**
         * what to do when we are called.
         */
        public void wakeup(NuprlBusEndpoint endpt)
        {
            if(NuprlDebug.debug_manager)
                System.err.println("NuprlManager.NuprlClientEvent.run");

            // Change state
            if(state != STATE_LOGIN) {
                state = STATE_LOGIN;
                autoLayout();
            }
        }
    }

    /**
     * The menu manager handles messages when there is no menu window.
     * We check to make sure there is no window, and then we create one.
     * The menu will subscribe to the bus.
     */
    class NuprlMenuManager
        implements NuprlBusClient
    {
        /**
         * We handle messages to the command window.
         */
        public String getHost()
        {
            return NuprlConstants.MENU_NAME;
        }

        /**
         * what to do when we are called.
         */
        public void wakeup(NuprlBusEndpoint endpt)
        {
            if(NuprlDebug.debug_manager)
                System.err.println("NuprlManager.NuprlMenuEvent.run");

            synchronized (bus) {
                NuprlBusMessage msg;
                while((msg = endpt.getMessage()) != null) {
                    // Recreate the menu window and forward the message
                    placeMenu(msg.dst_port);
                    try {
                        endpt.Forward(msg);
                    }
                    catch(NuprlException e) {
                        System.err.println("NuprlMenuEvent.run.Forward: " + e.stringOfException());
                        System.exit(1);
                    }
                }
            }

            if(state == STATE_LOGIN || state == STATE_COMMAND) {
                state = STATE_MENU;
                autoLayout();
            }
        }
    }

    /**
     * There may be several proof windows.
     * The proof manager handles events when a proof window
     * does not exist, or it has died.  We create the proof window
     * if necessary.
     */
    class NuprlProofManager
        implements NuprlBusClient
    {
        /**
         * We create one of these managers for each window type.
         */
        String host;

        /**
         * Create a manager for this proof type.
         */
        NuprlProofManager(String host)
        {
            this.host = host;
        }

        /**
         * We handle messages to the command window.
         */
        public String getHost()
        {
            return host;
        }

        /**
         * what to do when we are called.
         */
        public void wakeup(NuprlBusEndpoint endpt)
        {
            if(NuprlDebug.debug_manager)
                System.err.println("NuprlManager.NuprlProofEvent.run");

            synchronized (bus) {
                NuprlBusMessage msg;
                while((msg = endpt.getMessage()) != null) {
                    // Recreate the proof window, and forward the message
                    placeProof(msg.dst_port);
                    try {
                        endpt.Forward(msg);
                    }
                    catch(NuprlException e) {
                        System.err.println("NuprlProofEvent.run.Forward: " + e.stringOfException());
                        System.exit(1);
                    }
                }

                // Possibly relayout the windows
                if(state == STATE_COMMAND) {
                    state = STATE_PROOF;
                    autoLayout();
                }
            }
        }
    }

    /*
     * Handle messages to the command window.
     */
    class NuprlCommandManager
        implements NuprlBusClient, NuprlBusPort
    {
        /**
         * We handle messages to the command window.
         */
        public String getHost()
        {
            return NuprlConstants.COMMAND_NAME;
        }

        /**
         * The default command port.
         */
        public int getPort()
        {
            return DEFAULT_COMMAND_PORT;
        }

        /**
         * what to do when we are called.
         */
        public void wakeup(NuprlBusEndpoint endpt)
        {
            if(NuprlDebug.debug_manager)
                System.err.println("NuprlManager.NuprlCommandEvent.run: begin");

            synchronized (bus) {
                // Restart the command window
                state = STATE_COMMAND;
                autoLayout();

                // Forward the messages
                NuprlBusMessage msg;
                while((msg = endpt.getMessage()) != null) {
                    try {
                        endpt.Forward(msg);
                    }
                    catch(NuprlException e) {
                        System.err.println("NuprlCommandEvent.run.Forward: " + e.stringOfException());
                        System.exit(1);
                    }
                }
            }

            if(NuprlDebug.debug_manager)
                System.err.println("NuprlManager.NuprlCommandEvent.run: end");
        }
    }

    /************************************************************************
     * COMPONENT LISTENER                                                   *
     ************************************************************************/

    /**
     * Re-layout the window whever the desktop size changes.
     */
    class NuprlComponentListener
        extends ComponentAdapter
    {
        /**
         * Component is shown, so layout the windows.
         */
        public void componentShown(ComponentEvent event)
        {
            autoLayout();
        }

        /**
         * Component was resized, so relayout the windows.
         */
        public void componentResized(ComponentEvent event)
        {
            autoLayout();
        }
    }
}
