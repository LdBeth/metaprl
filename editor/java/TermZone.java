/**
 * Commands to enter or leave a break zone.
 * There are three types of zones:
 *    1. linear: no breaks are ever taken
 *    2. soft: all soft breaks in this zone are tken, or none are
 *    3. hard: all soft breaks in this zone are taken
 */

class TermZone
extends TermDisplay
{
    /**
     * Linear zone indicator.
     */
    static final int LINEAR_ZONE      = 0;

    /**
     * Soft break zone.
     */
    static final int SOFT_ZONE        = 1;

    /**
     * Hard break zone.
     */
    static final int HARD_ZONE        = 2;

    /**
     * End the current zone, and resume the last one.
     */
    static final int END_ZONE         = 3;

    /**
     * The zone command.
     */
    final int command;

    /**
     * Make a zone command.
     */
    public TermZone(int kind)
    {
        command = kind;
    }
}
        
