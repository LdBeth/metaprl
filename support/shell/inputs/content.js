/************************************************************************
 * Content events.
 */

/*
 * When the mouse moves over an element,
 * highlight it.
 */
function SlotTarget(event, name)
{
    var src;

    /* IE passes the event in window.event */
    if(!event)
        event = window.event;

    /* Get the current event target */
    if(event.target)
        src = event.target;
    else if(event.currentTarget)
        src = event.currentTarget;
    else if(event.srcElement)
        src = event.srcElement;

    /* Walk up the hierarchy until the actual target is found */
    while(src && src != document) {
        if(src.className == name)
            return src;

        /* Navigate up the tree */
        if(src.parentNode)
            src = src.parentNode;
        else
            src = src.parentElement;
    }
    return null;
}

/*
 * When the mouse moves over an element,
 * highlight it.
 */
function SlotMouseOver(event)
{
    var src = SlotTarget(event, "slot");
    if(src)
        src.className = "slotHover";
}

/*
 * When the mouse moves away again,
 * unhighlight it.
 */
function SlotMouseOut(event)
{
    var src = SlotTarget(event, "slotHover");
    if(src)
        src.className = "slot";
}

/*
 * When an element is clicked, paste it into the rule.
 */
function SlotMouseClick(event)
{
    var src = SlotTarget(event, "slotHover");
    if(src) {
        /* Paste the value into the rule bar */
        var text = parent.ruleframe.document.commandform.command.value;
        parent.ruleframe.document.commandform.command.value = text + '%%' + src.id + '%%';
	parent.ruleframe.document.commandform.submit();
    }
}

/*
 * Load the content.
 */
function OnLoad(session)
{
    document.onmouseover = SlotMouseOver;
    document.onmouseout = SlotMouseOut;
    document.onmouseup = parent.CancelMenu;
    document.onclick = SlotMouseClick;
    parent.LoadContent(session);
}

