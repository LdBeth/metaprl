/*
 * The frames holding the standard functions.
 */
var mojave;
var menuserver;

/*
 * Predicate on menu labels.
 */
function menulabel_pred(name)
{
    return name == 'menulabel' || name == 'menulabelhover';
}

function GetMenuTarget(window, event)
{
    var target;

    if(mojave) {
        var event = mojave.GetEvent(window, event);
        target = mojave.FindTarget(event, menulabel_pred);
    }
    else
        target = null;
    return target;
}

/*
 * When the mouse moves over an element,
 * highlight it.
 */
function MenuMouseOver(event)
{
    var src = GetMenuTarget(window, event);
    if(src)
        src.className = 'menulabelhover';
}

/*
 * When the mouse moves away again,
 * unhighlight it.
 */
function MenuMouseOut(event)
{
    var src = GetMenuTarget(window, event);
    if(src)
        src.className = 'menulabel';
}

/*
 * When an element is clicked, paste it into the rule.
 */
function MenuMouseDown(event)
{
    var src = GetMenuTarget(window, event);
    if(src) {
        /* Evaluate any command for this function */
        var command = buttoncommands[src.id];
        if(command)
            mojave.ButtonCommand(command);

        /* Bring up the menu if there is one */
	var items = menus[src.id];
        if(items) {
            var pos = mojave.GetPagePosition(menuserver, self, src);
            menuserver.MenuSetItems(items, menuenabled, menulabels, menucommands);
            menuserver.MenuSetPosition(pos);
            menuserver.MenuShow(true);
        }
    }
}

/*
 * Ignore mouse up events on the menu.
 */
function MenuMouseUp(event)
{
    if(mojave) {
        event = mojave.GetEvent(window, event);
        event.cancelBubble = true;
    }
}

/*
 * Load the menu routines.
 */
function MenuClient(mframe, mserver)
{
    /* Parent holds the utilities and the floating frame */
    mojave = mframe;
    menuserver = mserver;

    /* Add handlers to the frame */
    var menuframe = mojave.GetObject(self, 'menubox');
    if(menuframe) {
        menuframe.onmouseover = MenuMouseOver;
        menuframe.onmouseout  = MenuMouseOut;
        menuframe.onmousedown = MenuMouseDown;
        menuframe.onmouseup   = MenuMouseUp;
    }

    /* Other mouseup events cancel the menu */
    document.onmouseup = menuserver.CancelMenu;
}
