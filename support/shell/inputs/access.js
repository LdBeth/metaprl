function OnLoad()
{
    GetWindowSize();
    SetWindowCookie();
    SetResponseCookie('%%RESPONSE%%');
    location.href = '/';
}
