/**
 * This is a special pushbutton that we use in
 * the control panel.  It contains some extra handlers
 * for enableing and disabling the button.
 */
import netscape.application.*;

/*
 * Keep extra info about buttons.
 */
class GrayButton
implements Marshalable
{
    Button button;
    Image enabled, disabled;

    /**
     * Traverse this object.
     */
    public void Marshal(MarshalInfo info)
    {
        info.Marshal((View) button);
        info.Marshal(enabled);
        info.Marshal(disabled);
    }

    /**
     * Create a pushbutton.
     */
    GrayButton(String enabled, String disabled, int x, int y, int w, int h)
    {
        button = Button.createPushButton(x, y, w, h);
        this.enabled = Bitmap.bitmapNamed(enabled);
        this.disabled = Bitmap.bitmapNamed(disabled);
        button.setImage(this.enabled);
    }

    /**
     * When the button enabled state is changed,
     * swap the bitmaps.
     */
    public void setEnabled(boolean flag)
    {
        Image image = flag ? enabled : disabled;
        button.setImage(image);
        button.setEnabled(flag);
        button.setDirty(true);
    }
}
        
