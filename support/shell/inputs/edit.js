/************************************************************************
 * This is the edit page.
 */

/*
 * Height of the frame.
 */
function FrameHeight()
{
    var height = 100;

    if(self.innerHeight)
        height = self.innerHeight;
    else if(document.documentElement && document.documentElement.clientHeight)
        height = document.documentElement.clientHeight;
    else if(document.body)
        height = document.body.clientHeight;
    return height;
}

/*
 * A key was pressed in the text area.
 */
function KeyPress(event)
{
    var editarea = parent.GetObject(self, 'editarea');
    var savebutton = parent.GetObject(self, 'savebutton');
    savebutton.value += " (modified)";
    editarea.onkeypress = null;
}

/*
 * Resize the window.
 */
function OnLoad()
{
    var window_height = FrameHeight();
    var editarea = parent.GetObject(self, 'editarea');
    if(editarea) {
        var height = window_height - 40;
        if(height < 50)
            height = 50;
        editarea.style.height = height + 'px';
        editarea.onkeypress = KeyPress;
    }
    var editform = parent.GetObject(self, 'editform');
    if(editform)
        editform.style.height = window_height + 'px';
    document.onmouseup = parent.CancelMenu;
}
