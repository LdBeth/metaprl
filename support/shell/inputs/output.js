/*
 * Scroll the window to the bottom.
 */
var not_finished = true;

function Bottom()
{
    window.scrollTo(0, 1000000);
    if(not_finished)
        setTimeout('Bottom()', 1000);
}

function OnStart()
{
    setTimeout('Bottom()', 1000);
    document.onmouseup = parent.parent.CancelMenu;
}

/*
 * Once the page is loaded, add a button to the parent frame.
 */
function OnLoad()
{
    var abortbutton = parent.parent.GetObject(self, 'abortbutton');
    abortbutton.value = 'Done';
    not_finished = false;
}
