/************************************************************************
 * Rulebox events.
 */

/*
 * Scroll the history.
 */
function HistoryNavigate(amount)
{
    var history = parent.buttons.document.getElementById('history');
    if(history) {
        var length = history.options.length;
        var index = history.selectedIndex + amount;
        while(index < 0)
            index += length;
        while(index >= length)
            index -= length;
        var id = history.options[index].value;
        history.selectedIndex = index;
        var text = parent.buttons.macros[id];
        eval('parent.' + text);
    }
}

/*
 * Rulebox received a key.
 */
function RuleKey(event)
{
    var code = event.keyCode;
    if(code == 38)  // DownArrow
        HistoryNavigate(-1);
    else if(code == 40) // UpArrow
        HistoryNavigate(1);
}

/*
 * On load, cancel menus, and call parent loader.
 */
function OnLoad(session)
{
    document.onmouseup = parent.CancelMenu;
    parent.LoadRule(session);
    document.commandform.command.value = rulebox;
    document.commandform.command.focus();
}
