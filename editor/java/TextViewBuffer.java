/**
 * Define a TextBuffer that displays directly into a TextView.
 * Every line gets its own ParagraphFormat where we specify the
 * margins, etc.
 */

import netscape.application.*;
import netscape.util.*;

/*
 * This contains a snapshot of the formatting state
 */
class TextViewState
{
    /*
     * We also need to keep track of the current text column.
     * Strange that TextView won't do it for us...
     */
    int column;

    /**
     * We save the max column size to calculate maxSize.
     */
    int max_column;

    /*
     * Keep the font info.
     */
    Object font_stack;

    /*
     * Attributes for inserted text.
     */
    Hashtable attributes;

    /*
     * Collect the info.
     */
    TextViewState(int column, int max_column, Object stack, Hashtable attributes)
    {
        this.column = column;
        this.max_column = column;
        this.font_stack = stack;
        this.attributes = attributes;
    }
}

/*
 * Our buffer is defined as a TextView,
 * where we adapt the TextView into a TextBuffer interface.
 */
class TextViewBuffer
extends TextView
implements TextBuffer
{
    /**
     * We compute margins in hundredths of a point.
     */
    static final int POINTS_PER_INCH = 7227;
    static final int PIXELS_PER_INCH = 96;

    /**
     * This is the desired right margin in pixels.
     */
    protected int margin;

    /**
     * We also need to keep track of the current text column.
     * Strange that TextView won't do it for us...
     */
    protected int column;

    /**
     * Keep track of the totalmaximum width of the window.
     */
    protected int max_column;

    /**
     * The font base is used for choosing new fonts.
     */
    protected FontBase font_base;

    /**
     * We clone this paragraph format for each line
     * that is inserted.
     */
    protected TextParagraphFormat format;

    /**
     * Since defaultAttributes are propagated on insertion,
     * and we want inserted text to override the attributes,
     * we keep them and apply them on every insertion.
     */
    protected Hashtable attributes;

    /**
     * Create the buffer.  Pass the font base
     * that we will be choosing fonts from.
     */
    TextViewBuffer(FontBase base, int x, int y, int w, int h)
    {
        super(x, y, w, h);
        margin = w;
        column = 0;
        max_column = 0;
        font_base = base;

        /* Initialize paragraph format from default attributes */
        Hashtable table = defaultAttributes();
        TextParagraphFormat fmt = (TextParagraphFormat) table.get(PARAGRAPH_FORMAT_KEY);
        format = (TextParagraphFormat) fmt.clone();
        format.clearAllTabPositions();
        format.setJustification(Graphics.LEFT_JUSTIFIED);

        /* Copy default attributes */
        attributes = (Hashtable) table.clone();
        attributes.put(PARAGRAPH_FORMAT_KEY, format);
        attributes.put(FONT_KEY, base.getFont());
    }

    /**
     * Convert from hundredths of a point to pixels.
     * We assume 96 pixels per inch, and 72.27 points per inch.
     */
    int pixelsOfPoints(int points)
    {
        return (PIXELS_PER_INCH * points) / POINTS_PER_INCH;
    }

    /**
     * Convert from a pixel position into a points metric.
     */
    int pointsOfPixels(int pixels)
    {
        return (POINTS_PER_INCH * pixels) / PIXELS_PER_INCH;
    }

    /**
     * Append some text to the buffer.
     */
    public void append(String value)
    {
        setAttributesForRange(attributes, appendString(value));
        column += font_base.width(value);
        if(column > max_column)
            max_column = column;
    }

    /**
     * Make a new line.
     */
    public void nl()
    {
        appendString("\n");
        column = 0;
    }

    /**
     * Space a given amount of space.
     * This space is in hundredths of a point.
     */
    public void tab(int points)
    {
        column = pixelsOfPoints(points);
        if(column > max_column)
            max_column = column;
        TextParagraphFormat new_format = (TextParagraphFormat) format.clone();
        new_format.addTabPosition(column);
        attributes = (Hashtable) attributes.clone();
        attributes.put(PARAGRAPH_FORMAT_KEY, new_format);
        setAttributesForRange(attributes, appendString("\t"));
    }

    /**
     * Set the current insertion font.
     */
    public void pushFont(int style, int size)
    {
        Font font = font_base.pushFont(style, size);
        attributes = (Hashtable) attributes.clone();
        attributes.put(FONT_KEY, font);
    }

    /**
     * Restore the last font.
     */
    public void popFont()
    {
        Font font = font_base.popFont();
        attributes = (Hashtable) attributes.clone();
        attributes.put(FONT_KEY, font);
    }

    /**
     * Get the current column in points.
     */
    public int getColumn()
    {
        return pointsOfPixels(column);
    }

    /**
     * Get the right margin in points.
     */
    public int getMargin()
    {
        return pointsOfPixels(margin);
    }

    /**
     * Have we overflowed the right margin?
     */
    public boolean overflow()
    {
        return column > margin;
    }

    /**
     * Truncate the buffer at a certain index.
     */
    public void truncate(int index)
    {
        Range range = new Range(index, length() - index);
        replaceRangeWithString(range, "");
    }

    /**
     * Return the formatting attributes.
     */
    public Object getAttributes()
    {
        return new TextViewState(column,
                                 max_column,
                                 font_base.getFontStack(),
                                 attributes);
    }

    /**
     * Restore the formatting attributes.
     */
    public void setAttributes(Object s)
    {
        TextViewState state = (TextViewState) s;
        column = state.column;
        max_column = state.max_column;
        font_base.setFontStack(state.font_stack);
        attributes = state.attributes;
        format = (TextParagraphFormat) attributes.get(PARAGRAPH_FORMAT_KEY);
    }
}

