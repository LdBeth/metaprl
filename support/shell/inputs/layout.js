/*
 * Get the size of the window.
 * The code for finding the window size
 * is based on code from www.howtocreate.co.uk.
 */
var window_width = 0;
var window_height = 0;

function GetWindowSize()
{
    if(typeof(window.innerWidth) == 'number') {
        // Non-IE
        window_width = window.innerWidth;
        window_height = window.innerHeight;
    }
    else if(document.documentElement && (document.documentElement.clientWidth || document.documentElement.clientHeight)) {
        // IE 6+ in 'standards compliant mode'
        window_width = document.documentElement.clientWidth;
        window_height = document.documentElement.clientHeight;
    }
    else if(document.body && (document.body.clientWidth || document.body.clientHeight)) {
        // IE 4 compatible
        window_width = document.body.clientWidth;
        window_height = document.body.clientHeight;
    }
    else {
        alert('Cant figure out window size');
        window_width = 800;
        window_height = 800;
    }
}

/*
 * The styles depend on the window size.
 */
function ResizeBoxes(rulebox_height)
{
    parent.document.body.rows = '40,*,100,40,' + rulebox_height;
}

/*
 * Save the window size as a cookie, so MetaPRL can get a hold of it.
 */
var window_width_name = 'MetaPRL.width';

function SetWindowCookie()
{
   SetCookie(window_width_name, '' + window_width, null, "/", null, false);
}

/************************************************************************
 * Load events.
 */

/*
 * Versions.
 */
var version = new Array();

/*
 * Make sure the windows are up-to-date.
 */
function Update(session)
{
    if(version['menu'] != session['menu'])
        parent.menu.location.reload();
    if(version['content'] != session['content']) {
        if(version['location'] != session['location'])
            parent.content.location.href = session['location'];
        else
            parent.content.location.reload();
    }
    if(version['message'] != session['message'])
        parent.message.location.reload();
    if(version['buttons'] != session['buttons'])
        parent.buttons.location.reload();
    if(version['rule'] != session['rule'])
        parent.rule.location.reload();
}

/*
 * Resize event.
 */
function LoadFrame()
{
   GetWindowSize();
   SetWindowCookie();
}

function LoadMenu(session)
{
    version['menu'] = session['menu'];
}

function LoadContent(session)
{
    version['location'] = session['location'];
    version['content'] = session['content'];

    // Update the other windows
    Update(session);
}

function LoadMessage(session)
{
    version['message'] = session['message'];
    parent.message.scrollTo(0, 100000);
}

function LoadButtons(session)
{
    version['buttons'] = session['buttons'];

    // Reset history button
    parent.buttons.document.getElementById('historybox').selectedIndex = 0;
}

function LoadRule(session)
{
    version['rule'] = session['rule'];

    // Focus on the rule box
    parent.rule.document.commandform.command.focus();

    // Update the other windows
    Update(session);
}

/************************************************************************
 * Handle events
 */

/*
 * Print the command in the rulebox.
 */
function Prompt(cmd)
{
    parent.rule.document.commandform.command.value = cmd;
}

/*
 * Evaluate a command in the rulebox.
 */
function Command(cmd)
{
    parent.rule.document.commandform.command.value = cmd;
    parent.rule.document.commandform.submit();
}

/*
 * Bring up a URL in another window.
 */
function URL(where)
{
    window.open(where);
}

/*
 * Generic event handler.
 */
function HandleCommand(macros, id)
{
    var command = 'parent.' + macros[id];
    eval(command);
}

/*
 * The user selected a command from a menu.
 */
function MenuCommand(macros, menu)
{
    if(menu.selectedIndex != 0) {
        var id = menu.options[menu.selectedIndex].value;
        menu.selectedIndex = 0;
        HandleCommand(macros, id);
    }
}

/*
 * Press a button.
 */
function ButtonCommand(macros, button)
{
    var id = button.name;
    HandleCommand(macros, id);
}

/*
 * Press the submit button.
 */
function ButtonSubmit()
{
    parent.rule.document.commandform.submit();
}

/*
 * Toggle the kind of input area.
 */
function ToggleInputArea(button)
{
    // Get the current text
    var ruledoc = parent.rule.document;
    var text = ruledoc.commandform.command.value;

    // Reset the input area
    if(button.value == 'Long') {
        button.value = 'Short';
        ruledoc.commandform.innerHTML = '# <textarea name="command" rows="4" cols="100">' + text + '</textarea>';
        ResizeBoxes(150);
    }
    else {
        button.value = 'Long';
        ruledoc.commandform.innerHTML = '# <input type="text" name="command" size="100" border="0" value="' + text + '">';
        ResizeBoxes(70);
    }

    // For convenience, refocus the input area
    ruledoc.commandform.command.focus();
}

/************************************************************************
 * Low-level events.
 */

/*
 * Scroll the history.
 */
function HistoryNavigate(amount)
{
    var history = parent.buttons.document.getElementById('historybox');
    var length = history.options.length;
    var index = history.selectedIndex + amount;
    while(index < 0)
        index += length;
    while(index >= length)
        index -= length;
    var id = history.options[index].value;
    history.selectedIndex = index;
    var text = parent.buttons.macros[id];
    parent.rule.document.commandform.command.value = text;
}

/*
 * Rulebox received a key.
 */
function RuleKey(e)
{
    var code = e.keyCode;
    if(code == 38)  // DownArrow
        HistoryNavigate(-1);
    else if(code == 40) // UpArrow
        HistoryNavigate(1);
}

