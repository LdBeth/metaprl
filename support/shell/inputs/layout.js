/*
 * Get the size of the window.
 * The code for finding the window size
 * is based on code from www.howtocreate.co.uk.
 */
var window_width = 0;
var window_height = 0;

function GetWindowSize() {
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
}

/*
 * The styles depend on the window size.
 */
function WriteWindowStyle(rulebox_height) {
    var horizontal_border_size = 2;
    var vertical_border_size = 2;
    var box_width = window_width - 2 * horizontal_border_size;
    var message_height = 100;
    var content_height = window_height - message_height - rulebox_height;
    var rulebox_top = content_height + message_height;

    document.write('<style type="text/css" media="screen"><!-- ');
    document.write('#contentbox {'
                   + ' position: absolute'
		   + '; top: 0'
                   + '; left: ' + horizontal_border_size
		   + '; width: ' + box_width
		   + '; height: ' + (content_height - vertical_border_size)
		   + '; display: block'
		   + '; border: none'
		   + '; padding: none'
		   + '; overflow: auto'
		   + '} ');
    document.write('#messagebox {'
                   + ' position: absolute'
		   + '; top: ' + content_height
                   + '; left: ' + horizontal_border_size
		   + '; width: ' + box_width
		   + '; height: ' + (message_height - vertical_border_size)
		   + '; display: block'
		   + '; border: none'
		   + '; padding: none'
		   + '; overflow: auto'
                   + '; background: #f8f8ff'
		   + '} ');
    document.write('#rulebox {'
                   + ' position: absolute'
		   + '; top:' + rulebox_top
                   + '; left: ' + horizontal_border_size
		   + '; width: ' + box_width
		   + '; height: ' + rulebox_height
		   + '; display: block'
		   + '; border: none'
		   + '; padding: none'
		   + '; overflow: auto'
		   + '} ');
    document.write(' --></style>');
}

/*
 * Save the window size as a cookie, so MetaPRL can get a hold of it.
 */
var window_width_name = 'MetaPRL.window_width';

function SetWindowCookie() {
   SetCookie(window_width_name, '' + window_width, null, "/", null, false);
}

