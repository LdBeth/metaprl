/************************************************************************
 * This is the edit page.
 */

/*
 * Cancel this edit operation.
 */
function Cancel()
{
    window.close();
}

/*
 * Resize the window.
 */
function Resize()
{
    GetWindowSize();
    var editarea = GetObject(this, "editarea");
    if(editarea) {
        var height = window_height - 110;
        if(height < 300)
            height = 300;
        editarea.style.height = height + "px";
    }
}

function OnLoad()
{
    Resize();
    window.onresize = Resize;
}
