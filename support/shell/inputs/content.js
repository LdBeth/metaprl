/************************************************************************
 * Content events.
 */

/*
 * When the mouse moves over an element,
 * highlight it.
 */
function SlotMouseOver(event)
{
    var src;
    if(event.srcElement)
        src = event.srcElement;
    else if(event.currentTarget)
        src = event.currentTarget;
    if(src) {
        src.className = "slotHover";
        event.cancelBubble = true;
    }
}

/*
 * When the mouse moves away again,
 * unhighlight it.
 */
function SlotMouseOut(event)
{
    var src;
    if(event.srcElement)
        src = event.srcElement;
    else if(event.currentTarget)
        src = event.currentTarget;
    if(src) {
        src.className = "slot";
        event.cancelBubble = true;
    }
}

/*
 * When an element is clicked, paste it into the rule.
 */
function SlotMouseClick(event)
{
    var src;
    if(event.srcElement)
        src = event.srcElement;
    else if(event.currentTarget)
        src = event.currentTarget;
    if(src) {
        src.className = "slotClick";
        event.cancelBubble = true;

        /* Paste the value into the rule bar */
        var text = parent.rule.document.commandform.command.value;
        parent.rule.document.commandform.command.value = text + '%%' + src.id + '%%';
	parent.rule.document.commandform.submit();
    }
}
