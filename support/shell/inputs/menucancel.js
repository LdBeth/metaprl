/*
 * The frames holding the standard functions.
 */
var menuserver;

/*
 * Load the menu routines.
 */
function MenuCancel(mserver)
{
    /* Parent holds the utilities and the floating frame */
    menuserver = mserver;

    /* Other mouseup events cancel the menu */
    document.onmouseup = menuserver.CancelMenu;
}
