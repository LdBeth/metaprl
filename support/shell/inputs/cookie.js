/************************************************************************
 * Cookies.
 */

/*
 * This script came from the 24 hour JavaScripts Site
 * located at http://www.javascripts.com.  It is brought to
 * you by Eric Jarvies, Lewis Sellers, Giuseppe Lombardo,
 * Kurt Anderson, and David Medinets.
 */

/*
 * Get the value of a cookie.
 */
function getCookieVal(offset) {
    var endstr = document.cookie.indexOf(";", offset);
    if (endstr == -1)
        endstr = document.cookie.length;
    return unescape(document.cookie.substring(offset, endstr));
}
function GetCookie(name) {
    var arg = name + "=";
    var alen = arg.length;
    var clen = document.cookie.length;
    var i = 0;
    while (i < clen) {
        var j = i + alen;
        if (document.cookie.substring(i, j) == arg)
            return getCookieVal (j);
        i = document.cookie.indexOf(" ", i) + 1;
        if (i == 0)
            break;
    }
    return null;
}

/*
 * Set the value of a cookie.
 */
function SetCookie(name, value, expires, path, domain, secure) {
    document.cookie =
        name + "=" + escape (value) +
           ((expires == null) ? "" : ("; expires=" + expires.toGMTString())) +
           ((path == null)    ? "" : ("; path=" + path)) +
           ((domain == null)  ? "" : ("; domain=" + domain)) +
           ((secure == true)  ? "; secure" : "");
}
