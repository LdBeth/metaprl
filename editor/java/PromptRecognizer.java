/**
 * This class recognizes a prompt.
 * Possible prompts:
 *    [A-z0-9:_\\/]*[%#>:]
 *    <[A-z0-9:_/ ]*>
 *
 * Wish we had regular expressions.
 */

package edu.cornell.cs.jyh.nuprl;

class PromptRecognizer
implements StringIntFunction
{
    public int apply(String s)
    {
        int length = s.length();
        if(length == 0)
            return 0;
        boolean space_allowed = s.charAt(0) == '<';
        int i;
    out:
        for(i = 0; i < length; i++) {
            char c = s.charAt(i);
            if(Character.isLetterOrDigit(c))
                continue;
            switch(c) {
            case ':':
            case '/':
            case '\\':
            case '_':
                continue;
            case '%':
            case '#':
            case '>':
                return i + 1;
            case ' ':
                if(space_allowed)
                    continue;
                return 0;
            default:
                return 0;
            }
        }
        return i;
    }
}
