/*
 * Return focus to the input area.
 * Why oh why do we have to use a timeout?
 */
function ButtonFocusTimeout()
{
    var ruleframe = parent.ruleframe;
    if(ruleframe) {
        var commandform = ruleframe.document.commandform;
        if(commandform)
            commandform.command.focus();
    }
}

function ButtonFocus()
{
    setTimeout(ButtonFocusTimeout, 100);
}

/*
 * The styles depend on the window size.
 */
var messageheight = 100;
var ruleheight = 70;

function Layout()
{
    this.menuheight    = 35;
    this.buttonsheight = 35;
    this.messageheight = messageheight;
    this.ruleheight    = ruleheight;
    this.contentheight = window_height - this.menuheight - this.messageheight - this.buttonsheight - this.ruleheight;
    if(this.contentheight < 50)
        this.contentheight = 50;
    if(this.messageheight < 50)
        this.messageheight = 50;
    if(this.ruleheight < 50)
        this.ruleheight = 50;

    this.menutop    = 0;
    this.contenttop = this.menutop    + this.menuheight;
    this.messagetop = this.contenttop + this.contentheight;
    this.buttonstop = this.messagetop + this.messageheight;
    this.ruletop    = this.buttonstop + this.buttonsheight;
}

function ResizeInput()
{
    var frame = GetObject(self, 'ruleframe');
    if(frame) {
        var inputarea = GetObject(parent.ruleframe, "inputarea");
        if(inputarea) {
            var height = ruleheight - 20;
            if(height < 70)
                height = 70;
            inputarea.style.height = height + 'px';
        }
    }
}

function ResizeBoxes()
{
    var layout = new Layout();

    var menuframe    = GetObject(self, 'menuframe');
    var contentframe = GetObject(self, 'contentframe');
    var messageframe = GetObject(self, 'messageframe');
    var buttonsframe = GetObject(self, 'buttonsframe');
    var ruleframe    = GetObject(self, 'ruleframe');

    menuframe.style.top       = layout.menutop       + 'px';
    menuframe.style.height    = layout.menuheight    + 'px';
    contentframe.style.top    = layout.contenttop    + 'px';
    contentframe.style.height = layout.contentheight + 'px';
    messageframe.style.top    = layout.messagetop    + 'px';
    messageframe.style.height = layout.messageheight + 'px';
    buttonsframe.style.top    = layout.buttonstop    + 'px';
    buttonsframe.style.height = layout.buttonsheight + 'px';
    ruleframe.style.top       = layout.ruletop       + 'px';
    ruleframe.style.height    = layout.ruleheight    + 'px';

    /* Input area gets resized too */
    ResizeInput();
    ButtonFocus();

    /* Handles */
    var handleheight = 10;
    var handlewidth = 10;
    var handleleft = window_width - 40;

    var messagehandle = GetObject(self, 'messagehandle');
    var messagehandletop = layout.messagetop - handleheight / 2;
    messagehandle.style.left = handleleft + 'px';
    messagehandle.style.top = messagehandletop + 'px';
    messagehandle.style.visibility = 'visible';

    var buttonshandle = GetObject(self, 'buttonshandle');
    var buttonshandletop = layout.buttonstop - handleheight / 2;
    buttonshandle.style.left = handleleft + 'px';
    buttonshandle.style.top = buttonshandletop + 'px';
    buttonshandle.style.visibility = 'visible';
}

function MoveHandles()
{
    var layout = new Layout();

    /* Handles */
    var handleheight = 10;
    var handlewidth = 10;
    var handleleft = window_width - 40;

    var messagehandle = GetObject(self, 'messagehandle');
    var messagehandletop = layout.messagetop - handleheight / 2;
    messagehandle.style.left = handleleft + 'px';
    messagehandle.style.top = messagehandletop + 'px';
    messagehandle.style.visibility = 'visible';

    var buttonshandle = GetObject(self, 'buttonshandle');
    var buttonshandletop = layout.buttonstop - handleheight / 2;
    buttonshandle.style.left = handleleft + 'px';
    buttonshandle.style.top = buttonshandletop + 'px';
    buttonshandle.style.visibility = 'visible';
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
        parent.menuframe.location.reload();
    if(version['content'] != session['content']) {
        if(version['location'] != session['location'])
            parent.contentframe.location.href = session['location'];
        else
            parent.contentframe.location.reload();
    }
    if(version['message'] != session['message'])
        parent.messageframe.location.reload();
    if(version['buttons'] != session['buttons'])
        parent.buttonsframe.location.reload();
    if(version['rule'] != session['rule'])
        parent.ruleframe.location.reload();
    if(session['io'] && version['io'] != session['io']) {
        version['io'] = session['io'];
        parent.window.open('/session/' + session['id'] + '/frame/system/');
    }
    if(session['edit'] && version['edit'] != session['edit']) {
        version['edit'] = session['edit'];
        if(session['editflag']) {
            var filename = '/session/' + session['id'] + '/edit/' + session['file'] + '/info.prl';
            Edit(session['external'], filename);
        }
    }
}

/*
 * Resize event.
 */
function LoadFrame()
{
   GetWindowSize();
   SetWindowCookie();
   ResizeBoxes();
}

function LoadMenu(session)
{
    version['menu'] = session['menu'];
}

function LoadContent(session)
{
    version['location'] = session['location'];
    version['content'] = session['content'];
    Update(session);
}

function LoadMessage(session)
{
    version['message'] = session['message'];
}

function LoadButtons(session)
{
    version['buttons'] = session['buttons'];
}

function LoadRule(session)
{
    version['rule'] = session['rule'];
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
    parent.ruleframe.document.commandform.command.value = cmd;
    ButtonFocus();
}

/*
 * Evaluate a command in the rulebox.
 */
function Command(cmd)
{
    parent.ruleframe.document.commandform.command.value = cmd;
    parent.ruleframe.document.commandform.submit();
}

/*
 * Quit the current session.
 */
function Quit()
{
    Command('exit ()');
    parent.window.close();
}

/*
 * Make a new session in a new window.
 */
function NewWindow()
{
    var url = parent.location.href;
    window.open(url + '/clone');
    parent.menuframe.location.reload();
}

/*
 * Make a new session in this window.
 */
function NewSession()
{
    var url = parent.location.href;
    parent.location.href = url + '/clone';
}

/*
 * Switch to another session.
 */
function Session(id)
{
    var url = parent.location.href;
    parent.location.href = url + '/session/' + id;
}

/*
 * Bring up a URL in another window.
 */
function URL(where)
{
    window.open(where);
}

/*
 * Menu item was selected.
 */
function MenuCommand(s)
{
    eval(s);
}

/*
 * Button command is some text to be evaluated.
 */
function ButtonCommand(s)
{
    eval(s);
}

/*
 * Press the submit button.
 */
function ButtonSubmit()
{
    parent.ruleframe.document.commandform.submit();
}

/*
 * Clear the area.
 */
function ButtonClear()
{
    parent.ruleframe.document.commandform.command.value = '';
    ButtonFocus();
}

/*
 * Toggle the kind of input area.
 */
function ButtonLong()
{
    // Get the current text
    var frame = parent.ruleframe;
    if(frame) {
        var ruledoc = frame.document;
        var text = ruledoc.commandform.command.value;

        // Reset the input area
        ruledoc.commandform.innerHTML = '# <textarea name="command" id="inputarea">' + text + '</textarea>';
        ResizeInput();

        // For convenience, refocus the input area
        ButtonFocus();
    }
}

/************************************************************************
 * Edit a file.
 */
function Edit(ext, filename)
{
    if(ext)
        parent.hiddenframe.location.href = filename;
    else
        window.open(filename);
}
