/************************************************************************
 * Rulebox events.
 */

/*
 * Scroll the history.
 */
var history_index = 0;

function HistoryNavigate(amount)
{
    if(history) {
        var length = command_history.length;
        var index = history_index;
        index += amount;
        while(index < 0)
            index += length;
        while(index >= length)
            index -= length;
        var text = command_history[index];
        history_index = index;
        parent.Prompt(text);
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
function OnLoad(mojave, session)
{
    document.onmouseup = parent.CancelMenu;
    parent.LoadRule(session);
    document.commandform.command.value = rulebox;
    document.commandform.command.focus();
}
