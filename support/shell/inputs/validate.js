/************************************************************************
 * Validation.
 */
var password_name = 'MetaPRL.password';
var response_name = 'MetaPRL.response';

/*
 * Get the password, and check it against the challenge.
 */
function ValidatePassword()
{
    var encrypted;

    // Try to get it from the browser
    var cleartext = document.passform.pass.value;
    document.passform.pass.value = "";

    // If the new password is null, abort
    if(cleartext == null) {
        alert('Password was not entered');
        return false;
    }

    // Encrypt the challenge
    encrypted = md5(cleartext + challenge);

    // Check the result
    if(encrypted.substring(8, 16) != response) {
        alert('Password is not correct');
        return false;
    }

    // Save the cookies, expiring tomorrow
    var expDate = new Date();
    expDate.setTime(expDate.getTime() + (1 * 24 * 3600 * 1000));

    // Save the password if it is new
    SetCookie(password_name, cleartext, expDate, "/", null, true);

    // Validated cookie
    SetCookie(response_name, encrypted, expDate, "/", null, false);

    // Allow the user to return to work
    location.href = startpage;

    return false;
}

/*
 * Set the response cookie.
 */
function SetResponseCookie(cookie)
{
   // Save the cookies, expiring tomorrow
   var expDate = new Date();
   expDate.setTime(expDate.getTime() + (1 * 24 * 3600 * 1000));
   SetCookie(response_name, cookie, expDate, "/", null, false);
}

/*
 * Just in case, check the current password.  If it works, then
 * don't bother with the rest of this page.
 */
function ValidateOnLoad()
{
    var cleartext = GetCookie(password_name);
    if(cleartext != null) {
        encrypted = md5(cleartext + challenge);
        if(encrypted.substring(8, 16) == response) {
            // Save the validation cookie
            SetResponseCookie(encrypted);

            // Allow the user to return to work
            location.reload();
        }
    }
}
