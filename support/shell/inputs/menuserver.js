/*
 * This file implements a floating menu.  The menu is just a div
 * box that is normally hidden.  The div box can be filled in,
 * and then displayed.
 */

/*
 * The frame containing the utility functions.
 */
var mojave;

/*
 * The floating div box.
 */
var menufloat;

/*
 * Commands to be executed when an item is selected.
 */
var menucommands;

/*
 * Maximum height of the menu.
 */
var max_menu_items  = 30;
var max_menu_height = 300;
var menu_slop_y     = 25;
var menuscroll      = false;

/*
 * Build a menu from the array of menuitems.
 */
function MenuSetItems(ids, enabled, labels, commands)
{
    if(menufloat) {
        var text = '';
        for(var i = 0; i < ids.length; i++) {
            var id = ids[i];
            var label = labels[id];
            var is_enabled = enabled[id];
            if(label == '-')
                text += '<div class="menurule"></div>';
            else if(is_enabled)
                text += '<div class="menuitem" id="' + id +  '">' + label + '</div>';
            else
                text += '<div class="menushade" id="' + id +  '">' + label + '</div>';
        }
        menufloat.innerHTML = text;
        menucommands = commands;

        /* Scroll the window if we have to */
        if(ids.length > max_menu_items) {
            menufloat.style.height = max_menu_height + 'px';
            menufloat.style.overflow = 'scroll';
            menuscroll = true;
        }
        else {
            menufloat.style.height = 'auto';
            menufloat.style.overflow = 'visible';
            menuscroll = false;
        }
    }
}

/*
 * Position the menu just below the src element.
 */
function MenuSetPosition(pos)
{
    if(menufloat) {
        var y = pos.y;
        var menu_height = menufloat.offsetHeight;
        if(pos.y + menu_height > window_height)
            y -= menu_height + 10;
        else
            y += menu_slop_y;
        menufloat.style.left = pos.x + 'px';
        menufloat.style.top = y + 'px';
    }
}

/*
 * Show/hide the menu.
 */
function MenuShow(show)
{
    if(menufloat)
        menufloat.style.visibility = show ? 'visible' : 'hidden';
    if(!show)
        menucommands = null;
}

/*
 * Predicate on menu labels.
 */
function menuitem_pred(name)
{
    return name == 'menuitem' || name == 'menuitemhover';
}

function GetMenuItem(window, event)
{
    var event = mojave.GetEvent(window, event);
    return mojave.FindTarget(event, menuitem_pred);
}

/*
 * When the mouse moves over an element,
 * highlight it.
 */
function MenuItemMouseOver(event)
{
    var src = GetMenuItem(window, event);
    if(src)
        src.className = 'menuitemhover';
}

/*
 * When the mouse moves away again,
 * unhighlight it.
 */
function MenuItemMouseOut(event)
{
    var src = GetMenuItem(window, event);
    if(src)
        src.className = 'menuitem';
}

/*
 * If the document gets a mouseup, cancel the menu.
 */
function MenuItemMouseUp(event)
{
    event = GetEvent(window, event);
    var src = GetMenuItem(window, event);
    if(src && menucommands) {
        var command = menucommands[src.id];
        if(command)
            MenuCommand(command);
    }
    else
        event.cancelBubble = true;
}

/*
 * If the document gets a mouseup, cancel the menu.
 */
function CancelMenu(event)
{
    MenuShow(false);
}

/*
 * Load the parent handler.
 */
function MenuServer(mframe)
{
    mojave = mframe;
    menufloat = mojave.GetObject(self, 'menufloat');
    if(menufloat) {
        menufloat.onmouseover = MenuItemMouseOver;
        menufloat.onmouseout  = MenuItemMouseOut;
        menufloat.onmouseup   = MenuItemMouseUp;
        document.onmouseup    = CancelMenu;
    }
}
