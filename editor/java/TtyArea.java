/**
 * This TextArea implements a window for interaction.
 * When <return> is pressed, the current line is
 * provided as output.
 *
 * The current line is prompted for at the end of the page.
 * Text is added before the prompt.
 */

package edu.cornell.cs.jyh.nuprl;

import java.awt.*;              // For layout managers
import java.awt.event.*;        // For action and window events

import javax.swing.*;
import javax.swing.text.*;
import javax.swing.event.*;
import javax.swing.undo.*;

/**
 * This text area maintains a vector of strings and a font.
 */
class TtyArea
extends JTextPane
{
    /************************************************************************
     * FIELDS                                                               *
     ************************************************************************/

    /*
     * undo helpers.
     */
    protected UndoAction undoAction;
    protected RedoAction redoAction;
    protected UndoManager undo = new UndoManager();

    /************************************************************************
     * ACTIONS                                                              *
     ************************************************************************/

    /*
     * Actions to perform on undo and redo.
     */
    class UndoAction
        extends AbstractAction
    {
        public UndoAction()
        {
            super("Undo");
            setEnabled(false);
        }
          
        public void actionPerformed(ActionEvent e)
        {
            try {
                undo.undo();
            } catch (CannotUndoException ex) {
                System.out.println("Unable to undo: " + ex);
                ex.printStackTrace();
            }
            updateUndoState();
            redoAction.updateRedoState();
        }
          
        protected void updateUndoState() {
            if (undo.canUndo()) {
                setEnabled(true);
                putValue(Action.NAME, undo.getUndoPresentationName());
            } else {
                setEnabled(false);
                putValue(Action.NAME, "Undo");
            }
        }      
    }    

    class RedoAction
        extends AbstractAction
    {
        public RedoAction() {
            super("Redo");
            setEnabled(false);
        }

        public void actionPerformed(ActionEvent e) {
            try {
                undo.redo();
            } catch (CannotRedoException ex) {
                System.out.println("Unable to redo: " + ex);
                ex.printStackTrace();
            }
            updateRedoState();
            undoAction.updateUndoState();
        }

        protected void updateRedoState()
        {
            if (undo.canRedo()) {
                setEnabled(true);
                putValue(Action.NAME, undo.getRedoPresentationName());
            } else {
                setEnabled(false);
                putValue(Action.NAME, "Redo");
            }
        }
    }    

    /************************************************************************
     * KEY BINDINGS                                                         *
     ************************************************************************/

    /*
     * The following two methods allow us to find an
     * action provided by the editor kit by its name.
     */
    Hashtable actions;

    private void createActionTable(JTextComponent textComponent)
    {
        actions = new Hashtable();
        Action[] actionsArray = textComponent.getActions();
        for (int i = 0; i < actionsArray.length; i++) {
            Action a = actionsArray[i];
            actions.put(a.getValue(Action.NAME), a);
        }
    }

    private Action getActionByName(String name)
    {
        return (Action) actions.get(name);
    }

    /*
     * Add a couple of emacs key bindings to the key map for navigation,
     * and catch the return key event.
     */
    protected void addKeymapBindings()
    {
        //Add a new key map to the keymap hierarchy.
        Keymap keymap = textPane.addKeymap("MyNuprlBindings", textPane.getKeymap());

        // Ctrl-b to go backward one character
        Action action = getActionByName(DefaultEditorKit.backwardAction);
        KeyStroke key = KeyStroke.getKeyStroke(KeyEvent.VK_B, Event.CTRL_MASK);
        keymap.addActionForKeyStroke(key, action);

        // Ctrl-f to go forward one character
        action = getActionByName(DefaultEditorKit.forwardAction);
        key = KeyStroke.getKeyStroke(KeyEvent.VK_F, Event.CTRL_MASK);
        keymap.addActionForKeyStroke(key, action);

        // Ctrl-p to go up one line
        action = getActionByName(DefaultEditorKit.upAction);
        key = KeyStroke.getKeyStroke(KeyEvent.VK_P, Event.CTRL_MASK);
        keymap.addActionForKeyStroke(key, action);

        // Ctrl-n to go down one line
        action = getActionByName(DefaultEditorKit.downAction);
        key = KeyStroke.getKeyStroke(KeyEvent.VK_N, Event.CTRL_MASK);
        keymap.addActionForKeyStroke(key, action);

        textPane.setKeymap(keymap);
    }

    /************************************************************************
     * MENUS                                                                *
     ************************************************************************/

    /*
     * Edit menu allows undo/redo as well as cut/paste.
     */
    protected JMenu createEditMenu()
    {
        JMenu menu = new JMenu("Edit");

        undoAction = new UndoAction();
        menu.add(undoAction);

        redoAction = new RedoAction();
        menu.add(redoAction);

        menu.addSeparator();

        /*
         * These actions come from the default editor kit.
         * Get the ones we want and stick them in the menu.
         */
        menu.add(getActionByName(DefaultEditorKit.cutAction));
        menu.add(getActionByName(DefaultEditorKit.copyAction));
        menu.add(getActionByName(DefaultEditorKit.pasteAction));

        menu.addSeparator();

        menu.add(getActionByName(DefaultEditorKit.selectAllAction));

        return menu;
    }

    //Create the style menu.
    protected JMenu createStyleMenu()
    {
        JMenu menu = new JMenu("Style");

        Action action = new StyledEditorKit.BoldAction();
        action.putValue(Action.NAME, "Bold");
        menu.add(action);

        action = new StyledEditorKit.ItalicAction();
        action.putValue(Action.NAME, "Italic");
        menu.add(action);

        action = new StyledEditorKit.UnderlineAction();
        action.putValue(Action.NAME, "Underline");
        menu.add(action);

        menu.addSeparator();

        menu.add(new StyledEditorKit.FontSizeAction("12", 12));
        menu.add(new StyledEditorKit.FontSizeAction("14", 14));
        menu.add(new StyledEditorKit.FontSizeAction("18", 18));

        menu.addSeparator();

        menu.add(new StyledEditorKit.FontFamilyAction("Serif", "Serif"));
        menu.add(new StyledEditorKit.FontFamilyAction("SansSerif", "SansSerif"));

        menu.addSeparator();

        menu.add(new StyledEditorKit.ForegroundAction("Red", Color.red));
        menu.add(new StyledEditorKit.ForegroundAction("Green", Color.green));
        menu.add(new StyledEditorKit.ForegroundAction("Blue", Color.blue));
        menu.add(new StyledEditorKit.ForegroundAction("Black", Color.black));

        return menu;
    }

    /************************************************************************
     * TEXT PANE                                                            *
     ************************************************************************/

    protected void initStylesForTextPane()
    {
        //Initialize some styles.
        Style def = StyleContext.getDefaultStyleContext().getStyle(StyleContext.DEFAULT_STYLE);

        Style regular = addStyle("regular", def);
        StyleConstants.setFontFamily(def, "SansSerif");

        Style s = addStyle("italic", regular);
        StyleConstants.setItalic(s, true);

        s = addStyle("bold", regular);
        StyleConstants.setBold(s, true);

        s = addStyle("small", regular);
        StyleConstants.setFontSize(s, 10);

        s = addStyle("large", regular);
        StyleConstants.setFontSize(s, 16);

        // s = addStyle("icon", regular);
        // StyleConstants.setAlignment(s, StyleConstants.ALIGN_CENTER);
        // StyleConstants.setIcon(s, new ImageIcon("images/Pig.gif"));

        s = addStyle("button", regular);
        StyleConstants.setAlignment(s, StyleConstants.ALIGN_CENTER);
        JButton button = new JButton(new ImageIcon("images/image1.gif"));
        button.setMargin(new Insets(0, 0, 0, 0));
        button.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                System.err.println("Button was just pushed");
            }
        });
        StyleConstants.setComponent(s, button);
    }

    /**
     * Constructor set the <cr> filter, and the queue
     * for placing output.
     */
    TtyArea()
    {
        setCaretPosition(0);
        setMargin(new Insets(5, 5, 5, 5));

        //Set up the menu bar.
        createActionTable(this);
        JMenu editMenu = createEditMenu();
        JMenu styleMenu = createStyleMenu();
        JMenuBar mb = new JMenuBar();
        mb.add(editMenu);
        mb.add(styleMenu);
        setJMenuBar(mb);

        //Add some key bindings to the keymap.
        addKeymapBindings();

        //Put the initial text into the text pane.
        initDocument();

        //Start watching for undoable edits and caret changes.
        lsd.addUndoableEditListener(new MyUndoableEditListener());
        textPane.addCaretListener(caretListenerLabel);
        lsd.addDocumentListener(new MyDocumentListener());
    }
        String newline = "\n";

        String[] initString =
                { "This is an editable JTextPane, ",		//regular
                  "another ",					//italic
                  "styled ",					//bold
                  "text ",					//small
                  "component, ",				//large
                  "which supports embedded components..." + newline,//regular
                  " " + newline,				//button
                  "...and embedded icons..." + newline,		//regular
                  " ", 						//icon
                  newline + "JTextPane is a subclass of JEditorPane that " +
                    "uses a StyledEditorKit and StyledDocument, and provides " +
                    "cover methods for interacting with those objects."
                 };

        String[] initStyles = 
                { "regular", "italic", "bold", "small", "large",
                  "regular", "button", "regular", "icon",
                  "regular"
                };

        initStylesForTextPane();

        Document doc = getDocument();

        try {
            for (int i=0; i < initString.length; i++) {
                doc.insertString(doc.getLength(), initString[i], getStyle(initStyles[i]));
            }
        } catch (BadLocationException ble) {
            System.err.println("Couldn't insert initial text.");
        }

        // Listen for input
        doc.putProperty("name", "Text Field");
        doc.addDocumentListener(new MyDocumentListener());
    }
}
