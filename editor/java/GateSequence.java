/**
 * This is animated image of the Nuprl icon.
 */

import netscape.application.*;

public class GateSequence
extends ImageSequence
{
    /**
     * The sequence is loaded from the names "gate_1" ... "gate_9".
     */
    public GateSequence()
    {
        super();
        addImagesFromName("gate_1.jpg", 9);
        setPlaybackMode(BOUNCE);
        setFrameRate(100);
        setCurrentImageNumber(5);
    }

    /**
     * Intercept stop, and reset to image #5.
     */
    public void stop()
    {
        super.stop();
        setCurrentImageNumber(5);
    }
}

