/*
 * Figure out the resources.
 */
var is_dhtml = document.all ? 1 : 0;
var is_layered = document.layers ? 1 : 0;

/*
 * Terms are surrounded by this much extra.
 */
var width_extra = 16;
var height_extra = 4;

/************************************************************************
 * UTILITIES                                                            *
 ************************************************************************/

/*
 * Get the head of a string.
 */
function stringhead(str, suffix)
{
    if(str.length >= suffix.length) {
        var suffix2 = str.substr(str.length - suffix.length, suffix.length);
        if(suffix2 == suffix)
            return str.substr(0, str.length - suffix.length);
    }
    return null;
}

/*
 * NAme of an applet.
 */
function appletname(applet)
{
    return stringhead(applet.name, "_applet");
}

/*
 * See if a string is in an array.
 */
function member(name, set)
{
    var length = set.length;
    for(var i = 0; i != length; i++) {
        if(name == set[i])
            return true;
    }
    return false;
}

/************************************************************************
 * EVENT HANDLING                                                       *
 ************************************************************************/

/**
 * Periodically ask each applet whether it should be resized
 * and whether it has the focus.
 */
var resized = new Array();

function resizeTimeout()
{
    var applets = document.applets;
    var length = applets.length;
    for(var i = 0; i != length; i++) {
        var applet = applets(i);
        var name = appletname(applet);
        if(name != null && !member(name, resized)) {
            var dw = applet.desiredWidth();
            var dh = applet.desiredHeight();
            var w = applet.style.pixelWidth;
            var h = applet.style.pixelHeight;
            if(dw > 0 && dh > 0 && (dw < w || dh < h)) {
                var box = document.all(name);
                if(dw < w) {
                    applet.style.pixelWidth = dw;
                    box.style.pixelWidth = dw + width_extra;
                }
                if(dh < h) {
                    applet.style.pixelHeight = dh;
                    box.style.pixelHeight = dh + height_extra;
                }
                resized[resized.length] = name;
            }
        }
    }
}

// window.setInterval("resizeTimeout()", 10000);

/*
 * Highlight the box when the mouse passes over it.
 */
function setBoxHighlight(flag)
{
    var event = window.event;
    var box = event.srcElement;

    if(flag)
        box.style.background = "black";
    else
        box.style.background = "lightgreen";
}

/*
 * These two functions handle resizing of a box.
 * On the initial click, the mouse position is saved,
 * and as the mouse is moved around, the box is resized.
 * We keep track of the mouse position globally.
 */
var initX = 0;
var initY = 0;
var initW = 0;
var initH = 0;
var resizeX = 0;
var resizeY = 1;
var resize_box;
var resizing = 0;

/*
 * Each box sets its own onmousedown, so we know that
 * srcElement will be set to the caller.
 */ 
function startResize()
{
    var event = window.event;
    var box = event.srcElement;
   
    // Get resize parameters
    initX = event.x;
    initY = event.y;
    initW = box.style.pixelWidth;
    initH = box.style.pixelHeight;
    resize_box = box;

    // Which dimensions to resize
    resizeX = event.offsetX + 16 > resize_box.style.pixelWidth ? 1 : 0;
    resizeY = event.offsetY + 16 > resize_box.style.pixelHeight ? 1 : 0;

    // Hide the applet
    var applet_name = resize_box.id + "_applet";
    var applet = document.applets(applet_name);
    // applet.style.visibility = "hidden";
    // applet.style.pixelWidth = 32;
    applet.style.pixelHeight = 18;

    resizing = 1;
}

/*
 * The resize function is called whenever the mouse is moved in
 * the document.  So filter out events where their is no drag being collected.
 */
function evalResize()
{
    if(resizing) {
        var event = window.event;
        var width = initW + event.x - initX;
        var height = initH + event.y - initY;
        if(width < 32)
            width = 32;
        if(height < 12)
            height = 12;
        if(resizeX)
            resize_box.style.pixelWidth = width;
        if(resizeY)
            resize_box.style.pixelHeight = height;
    }
}

/*
 * The onmouseup is collected by the document.
 * Stop the resize if it is going on.
 */
function stopResize()
{
    if(resizing) {
        // Adjust the container size
        var event = window.event;
        var width = initW + event.x - initX;
        var height = initH + event.y - initY;
        if(width < 32)
            width = 32;
        if(height < 24)
            height = 24;
        if(resizeX)
            resize_box.style.pixelWidth = width;
        if(resizeY)
            resize_box.style.pixelHeight = height;

        // Adjust the applet size
        var applet_name = resize_box.id + "_applet";
        var applet = document.applets(applet_name);
        if(resizeX)
            applet.style.pixelWidth = width - width_extra;
        if(resizeY)
            applet.style.pixelHeight = height - height_extra;
        applet.style.visibility = "visible";

	resizing = 0;
    }
}

/************************************************************************
 * ITEM PLACEMENT							*
 ************************************************************************/
/*
 * Define a function to display an applet with percent
 * width of the screen.
 */
var name_index = 0

function term(term_name, width, height)
{
    // Generate a new name for this applet
    var name = "applet_" + name_index++;

    // Enclose it in a resize box
    if(is_dhtml) {
        var fwidth = width + width_extra;
        var fheight = height + height_extra;
        document.write(
            "<div id=" + name
            + " + class=appletframe style='width:" + fwidth
            + "; height:" + fheight
            + "' onmouseover='setBoxHighlight(1)' onmouseout='setBoxHighlight(0)'"
            + " onmousedown='startResize()'>"); 
    }

    // The applet itself
    document.write(
        "<applet align=top name=" + name + "_applet code='ActiveApplet' width="
        + width + " height=" + height
        + " style='position:relative; left:0; top:0'>"
        + "   <param name='ApplicationClass' value='NuprlTerm' mayscript>"
        + "   <param name='term' term value=" + term_name + ">"
        + "</applet>");

    // Close the div box
    if(is_dhtml)
        document.write("</div>");
}

/*
 * A display is a centered term.
 */
function disp(name, height)
{
    document.write("<center>");
    term(name, document.body.scrollWidth, height);
    document.write("</center>");
}
