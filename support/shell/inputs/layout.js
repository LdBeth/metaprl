/*
 * Get the size of the window.
 * The code for finding the window size
 * is based on code from www.howtocreate.co.uk.
 */
var window_width = 0;
var window_height = 0;

function GetWindowSize() {
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
function ResizeBoxes(rulebox_height) {
    var horizontal_border_size = 2;
    var vertical_border_size = 2;
    var box_width = window_width - 2 * horizontal_border_size;
    var message_height = 100;
    var content_height = window_height - message_height - rulebox_height;
    var rulebox_top = content_height + message_height;

    var content = document.getElementById('contentbox');
    var message = document.getElementById('messagebox');
    var rule = document.getElementById('rulebox');

    content.style.top = 0;
    content.style.left = horizontal_border_size;
    content.style.width = box_width;
    content.style.height = content_height - vertical_border_size;

    message.style.top = content_height;
    message.style.left = horizontal_border_size;
    message.style.width = box_width;
    message.style.height = message_height - vertical_border_size;

    rule.style.top = rulebox_top;
    rule.style.left = horizontal_border_size;
    rule.style.width = box_width;
    rule.style.height = rulebox_height;
}

/*
 * Save the window size as a cookie, so MetaPRL can get a hold of it.
 */
var window_width_name = 'MetaPRL.width';

function SetWindowCookie() {
   SetCookie(window_width_name, '' + window_width, null, "/", null, false);
}

/*
 * Toggling between the long and short view of the input box.
 */
var input_is_short = true;

function Long() {
    var inputbox = document.getElementById('inputbox');
    var text = document.commandform.command.value;
    document.commandform.togglebutton.value = 'Short';
    inputbox.innerHTML = '# <textarea name="command" rows="4" cols="100" border="0">' + text + '</textarea>';
    ResizeBoxes(150);
    input_is_short = false;
}

function Short() {
    var inputbox = document.getElementById('inputbox');
    var text = document.commandform.command.value;
    document.commandform.togglebutton.value = 'Long';
    inputbox.innerHTML = '# <input type="text" name="command" size="100" border="0" value="' + text + '">';
    ResizeBoxes(100);
    input_is_short = true;
}

function Toggle() {
    if(input_is_short)
        Long();
    else
        Short();
    document.commandform.command.focus();
    return false;
}

/*
 * Resize event.
 */
function Resize(rulebox_height) {
   GetWindowSize();
   ResizeBoxes(rulebox_height);
   SetWindowCookie();
}

function Load(rulebox_height) {
    input_is_short = true;
    Resize(rulebox_height);
    document.commandform.command.focus();
    document.getElementById('messagebox').scrollTop += 10000;
}

