/*
 * Handles.
 */
var messagehandle;
var buttonshandle;
var dragbox;

/*
 * Drag a handle.
 */
var dragstate = null;

function DragStart(event, state)
{
    if(dragstate == null) {
        event = GetEvent(window, event);
        state.startx = event.screenX;
        state.starty = event.screenY;
        dragstate = state;
        dragbox.style.height = window_height + 'px';
        dragbox.style.visibility = 'visible';
    }
}

function DragMove(event)
{
    event = GetEvent(window, event);
    if(dragstate) {
        var diffx = event.screenX - dragstate.startx;
        var diffy = event.screenY - dragstate.starty;
        dragstate.mousemove(diffx, diffy);
    }
}

function DragUp(event)
{
    event = GetEvent(window, event);
    if(dragstate) {
        var diffx = event.screenX - dragstate.startx;
        var diffy = event.screenY - dragstate.starty;
        dragbox.style.visibility = 'hidden';
        dragstate.mouseup(diffx, diffy);
        dragstate = null;
    }
}

function MessageDragStart(event)
{
    var state = new Object();
    state.height = messageheight;
    state.mousemove = function (diffx, diffy) {
        messageheight = this.height - diffy;
        MoveHandles();
    }
    state.mouseup = function (diffx, diffy) {
        messageheight = this.height - diffy;
        ResizeBoxes();
    }

    DragStart(event, state);
}

function ButtonsDragStart(event)
{
    var state = new Object();
    state.height = ruleheight;
    state.mousemove = function (diffx, diffy) {
        ruleheight = this.height - diffy;
        MoveHandles();
    }
    state.mouseup = function (diffx, diffy) {
        ruleheight = this.height - diffy;
        ResizeBoxes();
    }

    DragStart(event, state);
}

/*
 * Cancel menu, and load message.
 */
function OnLoad()
{
    document.onmouseup = CancelMenu;
    MenuServer(self);
    LoadFrame();

    /* Add handlers for move boxes */
    dragbox = GetObject(self, 'dragbox');
    if(dragbox) {
        dragbox.onmousemove = DragMove;
        dragbox.onmouseup   = DragUp;
    }

    messagehandle = GetObject(self, 'messagehandle');
    if(messagehandle)
        messagehandle.onmousedown = MessageDragStart;

    buttonshandle = GetObject(self, 'buttonshandle');
    if(buttonshandle)
        buttonshandle.onmousedown = ButtonsDragStart;

    /* Resize the window */
    window.onresize = LoadFrame;
}
