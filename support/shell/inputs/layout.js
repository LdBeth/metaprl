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
    var horizontal_border_size = 2;
    var vertical_border_size = 2;
    var box_width = window_width - 2 * horizontal_border_size - 4;

    var menu_height = 30;
    var message_height = 100;
    var content_top = menu_height;
    var content_height = window_height - menu_height - message_height - rulebox_height;
    var message_top = content_top + content_height;
    var rulebox_top = message_top + message_height;

    var menu = document.getElementById('menubox');
    var content = document.getElementById('contentbox');
    var message = document.getElementById('messagebox');
    var rule = document.getElementById('rulebox');

    menu.style.top = "0px";
    menu.style.left = horizontal_border_size + "px";
    menu.style.width = box_width + "px";
    menu.style.height = (menu_height - vertical_border_size) + "px";

    content.style.top = menu_height + "px";
    content.style.left = horizontal_border_size + "px";
    content.style.width = box_width + "px";
    content.style.height = (content_height - vertical_border_size) + "px";

    message.style.top = message_top + "px";
    message.style.left = horizontal_border_size + "px";
    message.style.width = box_width + "px";
    message.style.height = (message_height - vertical_border_size) + "px";

    rule.style.top = rulebox_top + "px";
    rule.style.left = horizontal_border_size + "px";
    rule.style.width = box_width + "px";
    rule.style.height = rulebox_height + "px";
}

/*
 * Save the window size as a cookie, so MetaPRL can get a hold of it.
 */
var window_width_name = 'MetaPRL.width';

function SetWindowCookie()
{
   SetCookie(window_width_name, '' + window_width, null, "/", null, false);
}

/*
 * Resize event.
 */
function Resize(rulebox_height)
{
   GetWindowSize();
   ResizeBoxes(rulebox_height);
   SetWindowCookie();
}

function Load(rulebox_height)
{
    input_is_short = true;
    Resize(rulebox_height);
    document.commandform.command.focus();
    document.getElementById('messagebox').scrollTop += 10000;
}

/*
 * The user selected a command from a menu.
 */
function MenuCommand(menu, submit)
{
    var id = menu.options[menu.selectedIndex].value;
    var text = macros[id];
    document.commandform.command.value = text;
    if(submit)
        document.commandform.submit();
}

/*
 * Press a button.
 */
function ButtonCommand(button)
{
    var id = button.name;
    var text = macros[id];
    document.commandform.command.value = text;
    document.commandform.submit();
}

/*
 * Press the submit button.
 */
function ButtonSubmit()
{
    document.commandform.submit();
}

/*
 * Toggle the kind of input area.
 */
function ToggleInputArea(button)
{
    // Get the current text
    var inputbox = document.getElementById('inputbox');
    var text = document.commandform.command.value;

    // Reset the input area
    if(button.value == 'Long') {
        button.value = 'Short';
        inputbox.innerHTML = '# <textarea name="command" rows="4" cols="100">' + text + '</textarea>';
        ResizeBoxes(150);
    }
    else {
        button.value = 'Long';
        inputbox.innerHTML = '# <input type="text" name="command" size="100" border="0" value="' + text + '">';
        ResizeBoxes(100);
    }

    // For convenience, refocus the input area
    document.commandform.command.focus();
}


