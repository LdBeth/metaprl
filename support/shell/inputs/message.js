/*
 * Cancel menu, and load message.
 */
function OnLoad(session)
{
    document.onmouseup = parent.CancelMenu;
    parent.LoadMessage(session);
    scrollTo(0, 100000);
}
