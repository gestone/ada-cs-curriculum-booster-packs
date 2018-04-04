/**
 * Welcome to your implementation of your very own BitSet! :)
 *
 * After you're done implementing your methods, run BitSetTest.java and submit a PR with this implementation
 * so we can review it.
 *
 * This will verify your implementation of your BitSet is correct so that we can use it
 * in trying to solve a memory intensive problem. 
 */
public class BitSet {

    private static final int SIZE_OF_LONG = 8;

    /** 
     * Woah, wait aren't we dealing with raw bytes?
     *
     * Not quite! Rather than having a byte[] array, what we're going to do here is use an array of longs as it gives us a
     * more compact representation.
     * 
     * Remember, there are 8 bytes for every long and 8 bits in every byte. So each long represents 64 bits!
     *
     * Be sure to keep this in mind when deciding how many longs to allocate given the number of spaces the client wants.
     *
     * As an example, on successful initialization, we want something that looks like:
     *
     * BitSet bitSet = new BitSet(5); // Client only wants 5 spaces so allocate 1 long
     *
     * What the bit set should look like:
     *
     *      BYTE 1             BYTE 2            BYTE 3             BYTE 4            BYTE 5            BYTE 6          BYTE 7            BYTE 8
     * --------------------------------------------------------------------------------------------------------------------------------------------------
     * | 0 0 0 0 0 0 0 0 | 0 0 0 0 0 0 0 0 | 0 0 0 0 0 0 0 0 | 0 0 0 0 0 0 0 0 | 0 0 0 0 0 0 0 0 | 0 0 0 0 0 0 0 0 | 0 0 0 0 0 0 0 0 | 0 0 0 0 0 0 0 0  |
     * --------------------------------------------------------------------------------------------------------------------------------------------------
     *
     * Note that we allocated 1 long which has 64 bits even though the client only asked for 5 spots. Ideally, we'd allocate only 1 byte for this scenario (since we can't issue partial bytes or longs)
     * for 8 bits, but since we're choosing to represent our BitSet with longs, we'll do the best we can and allocate a single long instead.
     *
     * As an aside, we'll want to keep this field private so we don't let clients do something like:
     *
     * bitSet.allBytes[3] = 582957L; // Now all our bits in the long are messed up! :(
     */
    private long[] allBytes;
    private long numSpaces;

    public BitSet(long numSpaces) {
        this.numSpaces = numSpaces;
        // TODO: Allocate enough longs in the allBytes array to hold enough spaces.
    }

    /**
     * Flips the number at a particular index in the bit set.
     *
     * Think hard about how to exactly index into the BitSet given the index.
     *
     * As a hint, you'll first need to determine the index into allBytes to get the correct long,
     * then use bitshifting magic with operators (<<, >>, ~, |, &, ^) to flip just that bit that you want to set.
     *
     * If the index given is negative, throw a new IllegalArgumentException. See exceptions here:
     * If the index given is larger than the number of spaces, resize allBytes, copy all the existing longs 
     * into a new array, then flip the index in the resized BitSet.
     *
     * @param index the index to flip. If the bit at that index is set, then flip it to 0, otherwise flip it to 1.
     */
    public void flip(long index) {

    }

    /**
     * Returns whether or not the bit at index is set in the BitSet.
     *
     * @param index 
     */
    public boolean contains(long index) {

    }

    public void clear() {

    }

}
