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
    document.write('<style type="text/css" media="screen"><!-- ');
    document.write('#contentbox {'
                   + ' position: absolute'
		   + '; top: 0'
                   + '; left: 0'
		   + '; width: 100%'
		   + '; height: ' + (window_height - rulebox_height)
		   + '; display: block'
		   + '; border: none'
		   + '; padding: none'
		   + '; overflow: auto'
		   + '} ');
    document.write('#rulebox {'
                   + ' position: absolute'
		   + '; top:' + (window_height - rulebox_height)
                   + '; left: 0'
		   + '; width: 100%'
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

