/**
 * A font base performs the map between font charactersitics
 * and actual fonts.
 */

import netscape.application.*;

/*
 * Keep the font info in a linked list for easy snapshots.
 */
class FontLink
{
    /* Keep the style and size */
    int style, size;

    /* Next item in stack */
    FontLink next;

    /* Make the link */
    FontLink(int style, int size, FontLink next)
    {
        this.style = style;
        this.size = size;
        this.next = next;
    }
}

/*
 * Font basemaps the commands to actual fonts.
 */
public class FontBase
{
    /**
     * Nop command can be used for either the style or the size.
     */
    public static final int FONT_NOP            = 0;

    /**
     * Plain font style.
     */
    public static final int FONT_PLAIN          = 10;

    /**
     * Italic font.
     */
    public static final int FONT_EMPHASIS       = 11;

    /**
     * Bold font.
     */
    public static final int FONT_BOLD           = 12;

    /**
     * Symbol font.
     */
    public static final int FONT_SYMBOL         = 13;

    /**
     * Smallest font.
     */
    public static final int FONT_EXTRA_SMALL    = 20;

    /**
     * Smallest font.
     */
    public static final int FONT_SMALL          = 21;

    /**
     * Normal font.
     */
    public static final int FONT_NORMAL         = 22;

    /**
     * Larger font.
     */
    public static final int FONT_LARGE          = 23;

    /**
     * Largest font.
     */
    public static final int FONT_EXTRA_LARGE    = 24;

    /**
     * This is the actual font family we are using.
     */
    protected String family;

    /**
     * Symbol font family.
     */
    protected String symbol_family;

    /**
     * Keep a stack of font info.
     */
    protected FontLink stack;

    /**
     * This is the actual font we are using currently.
     */
    protected Font font;

    /**
     * Also keep the metrics.
     */
    protected FontMetrics metrics;

    /**
     * Create an initial font base.
     */
    public FontBase()
    {
        font = Font.defaultFont();
        family = font.family();
        symbol_family = "symbol";
        pushFont(FONT_PLAIN, FONT_NORMAL);
    }

    /**
     * Get the current font.
     */
    public Font getFont()
    {
        return font;
    }

    /**
     * Compute the width of a string in the current font.
     */
    public int width(String s)
    {
        return metrics.stringWidth(s);
    }

    /**
     * Set a font with a logical command.
     */
    public Font pushFont(int style, int size)
    {
        // Adjust attributes
        if(style == FONT_NOP)
            style = stack.style;
        if(size == FONT_NOP)
            size = stack.size;

        // Push them and set the font
        stack = new FontLink(style, size, stack);
        return setFont();
    }

    /**
     * Set font fron current attributes.
     */
    protected Font setFont()
    {
        String phy_family;
        int phy_size, phy_style;

        // Get the family and style
        switch(stack.style) {
        case FONT_EMPHASIS:
            phy_family = family;
            phy_style = Font.ITALIC;
            break;
        case FONT_BOLD:
            phy_family = family;
            phy_style = Font.BOLD;
            break;
        case FONT_SYMBOL:
            phy_family = symbol_family;
            phy_style = Font.PLAIN;
            break;
        default:
            phy_family = family;
            phy_style = Font.PLAIN;
            break;
        }

        // Get the point size
        switch(stack.size) {
        case FONT_EXTRA_SMALL:
            phy_size = 8;
            break;
        case FONT_SMALL:
            phy_size = 10;
            break;
        case FONT_LARGE:
            phy_size = 16;
            break;
        case FONT_EXTRA_LARGE:
            phy_size = 24;
            break;
        default:
            phy_size = 12;
            break;
        }

        // Now try to load the font
        font = Font.fontNamed(phy_family, phy_style, phy_size);
        if(font == null) {
            font = Font.fontNamed("helvetica", phy_style, phy_size);
            if(font == null)
                font = Font.defaultFont();
        }

        // Get the metrics
        metrics = font.fontMetrics();
        return font;
    }

    /**
     * Pop the font stack.
     */
    public Font popFont()
    {
        // Pop a nonempty stack
        if(stack.next != null)
            stack = stack.next;

        // Set the font
        return setFont();
    }

    /**
     * Get the font stack.
     */
    public Object getFontStack()
    {
        return stack;
    }

    /**
     * Restore the font stack.
     */
    public Font setFontStack(Object stack)
    {
        this.stack = (FontLink) stack;
        return setFont();
    }        
}

