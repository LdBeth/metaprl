/*
 * Stack of numbers.
 * This is to save space from a Vector.
 * This stack is never empty.
 */
class IntStack
{
    /* Keep the stops in an array */
    private int[] stops;

    /* Current index into the stack */
    private int index;

    /* New stack */
    IntStack(int base)
    {
        stops = new int[100];
        stops[0] = base;
        index = 0;
    }

    /* Current value in the stack */
    int top()
    {
        return stops[index];
    }

    /* Add a value to the stack */
    void push(int stop)
    {
        if(index == stops.length) {
            // Expand the tabstop array
            int[] new_stops = new int[stops.length << 1];
            for(int i = 0; i != index; i++)
                new_stops[i] = stops[i];
            stops = new_stops;
        }

        stops[++index] = stop;
    }

    /* Pop the top value, no underflow */
    int pop()
    {
        int value = stops[index];
        if(index != 0)
            index--;
        return value;
    }
}

