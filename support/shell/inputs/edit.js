/*
 * Height of the frame.
 */
function FrameHeight()
{
    var height = 100;

    if(self.innerHeight)
        height = self.innerHeight;
    else if(document.documentElement && document.documentElement.clientHeight)
        height = document.documentElement.clientHeight;
    else if(document.body)
        height = document.body.clientHeight;
    return height;
}


/*
 * A key was pressed in the text area.
 */
function SetSaveButton()
{
    var savebutton = parent.GetObject(self, 'savebutton');
    var text = 'Save';
    if(editinfo.isnew) {
        if(editinfo.ismodified)
            text += ' (new, modified)';
        else
            text += ' (new)';
    }
    else if(editinfo.ismodified)
        text += ' (modified)';
    if(editinfo.ischanged)
        text += ' *';
    savebutton.value = text;

    /* Show the cancel button if modified */
    if(editinfo.ismodified) {
        var cancelbutton = parent.GetObject(self, 'cancelbutton');
        cancelbutton.style.visibility = 'visible';
    }
}

/*
 * Get the current point.
 */
function GetPoint()
{
    var editarea = parent.GetObject(self, 'editarea');
    var start = 0;

    /* Mozilla */
    if(typeof(editarea.selectionStart) == "number")
        start = editarea.selectionStart;

    /*
     * IE doesn't give us a character position,
     * so use a binary search to find the position.
     */
    else if(document.selection && editarea.createTextRange) {
        editarea.focus();
        var sel = document.selection;
        if(sel) {
            /* Perform a binary search */
            var left = 1000000;
            var right = 0;
            while(left - right > 1) {
                var selrange = sel.createRange();
                var middle = parseInt((left + right) / 2);
                selrange.move("character", -middle);
                if(selrange.parentElement() == editarea)
                    right = middle;
                else
                    left = middle;
            }
            start = right;
        }
    }
    return start;
}

/*
 * Set the current point.
 */
function SetPoint()
{
    var editarea = parent.GetObject(self, 'editarea');
    var point = editinfo.point;

    /* Mozilla */
    if(typeof(editarea.selectionStart) == "number") {
        editarea.selectionStart = point;
        editarea.selectionEnd = point;
        editarea.focus();
    }

    /* IE */
    else if(editarea.createTextRange) {
        var range = editarea.createTextRange();
        range.move("character", -1000000);
        range.move("character", editinfo.point);
        range.select();
        range.scrollIntoView(true);
    }
}

/*
 * Save the buffer.
 */
function EditSave()
{
    var editarea = parent.GetObject(self, 'editarea');
    var pointbutton = parent.GetObject(self, 'pointbutton');
    var typebutton = parent.GetObject(self, 'typebutton');
    pointbutton.value = GetPoint();
    typebutton.value = 'save';
    document.editform.submit();
}

function EditBackup()
{
    if(editinfo.ischanged) {
        var editarea = parent.GetObject(self, 'editarea');
        var pointbutton = parent.GetObject(self, 'pointbutton');
        var typebutton = parent.GetObject(self, 'typebutton');
        pointbutton.value = GetPoint();
        typebutton.value = 'backup';
        document.editform.submit();
        editinfo.ischanged = false;
    }
}

function EditCancel()
{
    var editarea = parent.GetObject(self, 'editarea');
    var pointbutton = parent.GetObject(self, 'pointbutton');
    var typebutton = parent.GetObject(self, 'typebutton');
    pointbutton.value = GetPoint();
    typebutton.value = 'cancel';
    document.editform.submit();
}

/*
 * A key was pressed in the text area.
 */
function CheckUpdate(event)
{
    var editarea = parent.GetObject(self, 'editarea');
    if(editarea && editarea.value.length != editinfo.length) {
        editarea.onkeypress = null;
        editinfo.ismodified = true;
        editinfo.ischanged = true;
        SetSaveButton();
    }
    else
       Check();
}

function Check()
{
    setTimeout('CheckUpdate()', 1000);
}

/*
 * Backup on blur.
 */
function OnBlur(event)
{
    EditBackup();
}

/*
 * Resize the window.
 */
function OnLoad()
{
    /* Keep track of whether this object has been changed */
    editinfo.ischanged = false;

    /* Resize the windows */
    var window_height = FrameHeight();
    var editarea = parent.GetObject(self, 'editarea');
    if(editarea) {
        var height = window_height - 40;
        if(height < 50)
            height = 50;
        editarea.style.height = height + 'px';
        editinfo.length = editarea.value.length;
        editarea.onblur = OnBlur;
    }
    var editform = parent.GetObject(self, 'editform');
    if(editform)
        editform.style.height = window_height + 'px';
    document.onmouseup = parent.CancelMenu;

    /* Set the state */
    Check();
    SetSaveButton();
    SetPoint();
}
