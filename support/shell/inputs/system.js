/************************************************************************
 * This page displays the result of an output command.
 */

/*
 * Load the content.
 */
function OnLoad()
{
    GetWindowSize();
    var outputframe = GetObject(self, 'outputframe');
    outputframe.style.height = (window_height - 50) + 'px';
    outputframe.src = '/session/' + session['id'] + '/output';
}

