/*
 * Browser-independent routines.
 * Well, at least we try to make them browser-indepdendent.
 */

/*
 * Get the size of the window.
 * The code for finding the window size
 * is based on code from www.howtocreate.co.uk.
 */
var window_width = 0;
var window_height = 0;

function GetWindowSize()
{
    if(typeof(window.innerWidth) == 'number') {
        // Non-IE
        window_width = window.innerWidth;
        window_height = window.innerHeight;
    }
    else if(document.documentElement && (document.documentElement.clientWidth || document.documentElement.clientHeight)) {
        // IE 6+ in 'standards compliant mode'
        window_width = document.documentElement.clientWidth;
        window_height = document.documentElement.clientHeight;
    }
    else if(document.body && (document.body.clientWidth || document.body.clientHeight)) {
        // IE 4 compatible
        window_width = document.body.clientWidth;
        window_height = document.body.clientHeight;
    }
    else {
        alert("Can't figure out window size");
        window_width = 800;
        window_height = 800;
    }
}

/*
 * Get the position of a menu element in the current frame.
 */
function GetPositionAux(pos, obj)
{
    if(obj) {
        if(obj.offsetParent) {
            while(obj.offsetParent)	{
                pos.x += obj.offsetLeft;
                pos.y += obj.offsetTop;
                obj = obj.offsetParent;
            }
        }
        else if(obj.x || obj.y) {
            pos.x += obj.x;
            pos.y += obj.y;
        }
    }
}

function GetPosition(obj)
{
    var pos = new Object();
    pos.x = 0;
    pos.y = 0;
    GetPositionAux(pos, obj);
    return pos;
}

/*
 * Get the position of the current window relative to
 * some parent frame.
 */
function GetWindowPositionAux(pos, frame, current)
{
    var src;

    /* Walk up to the parent */
    while(current != frame && current != top) {
        var name = current.name;
        var parent = current.parent;

        /* Get the window as viewed by the parent */
        var doc = parent.document;
        if(doc.getElementById)
            src = doc.getElementById(name);
        else if(doc.all)
            src = doc.all[name];
        else if(doc.layers && doc.layers[name])
            src = doc.layers[name];
        if(!src)
            break;

        /* Get the position in the parent */
        GetPositionAux(pos, src);
        current = parent;
    }
}

function GetPagePosition(frame, current, obj)
{
    var pos = GetPosition(obj);
    GetWindowPositionAux(pos, frame, current);
    return pos;
}

/*
 * Get an object by id, in the current frame, or
 * any enclosing one.
 */
function GetObject(current, name)
{
    var src, doc;

    doc = current.document;
    while(!src) {
        if(doc.getElementById)
            src = doc.getElementById(name);
        else if(doc.all)
            src = doc.all[name];
        else if(doc.layers && doc.layers[name])
            src = doc.layers[name];

        /* Try the parent next */
        if(current == top)
            break;
        current = current.parent;
        doc = current.document;
    }
    return src;
}

/*
 * Get the real event.  IE passes the event attached to
 * the window.
 */
function GetEvent(window, event)
{
    if(!event)
        event = window.event;
    return event;
}

/*
 * Get the targeted element.
 * The target can be in several places.
 */
function GetTarget(event)
{
    var src;

    if(event.target)
        src = event.target;
    else if(event.currentTarget)
        src = event.currentTarget;
    else if(event.srcElement)
        src = event.srcElement;
    return src;
}

/*
 * Find the nearest enclosing target with
 * a classname that matches the predicate.
 */
function FindTarget(event, pred)
{
    /* Get the target */
    var src = GetTarget(event);

    /* Walk up the hierarchy until the actual target is found */
    while(src) {
        if(src.className && pred(src.className))
            return src;

        /* Navigate up the tree */
        if(src.parentNode)
            src = src.parentNode;
        else
            src = src.parentElement;
    }
    return null;
}

