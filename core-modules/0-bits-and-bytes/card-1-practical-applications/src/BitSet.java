import sun.reflect.generics.reflectiveObjects.NotImplementedException;

/**
 * Welcome to your implementation of your very own BitSet! :)
 *
 * After you're done implementing your methods, run BitSetTest.java and submit a PR with this implementation
 * so we can review it.
 *
 * BitSetTest.java will verify your implementation of your BitSet is correct so that we can use it
 * in trying to solve a more interesting problem.
 */
public class BitSet {

    private byte[] allBytes;

    /**
     *
     * Constructor for the BitSet. It should take in a number of spaces and allocate that many spaces.
     *
     * BitSet bitSet = new BitSet(16); // Client only wants 16 spaces. We should allocate 2 bytes because we need 16 bits
     *                                 // and each byte has 8 bits.
     *
     * What the bit set should look like:
     *
     *      BYTE 1            BYTE 2
     * -------------------------------------
     * | 0 0 0 0 0 0 0 0 | 0 0 0 0 0 0 0 0 |
     * -------------------------------------
     *
     * As an aside, we'll want to keep this field private so we don't let clients do something like:
     *
     * bitSet.allBytes[0] = 7; // Now all our bits in the byte are messed up! :(
     *
     * Just for reference, a byte in Java is like an integer, but only has 8 bits rather than 32.
     *
     * Hint: When initializing byte arrays in Java, all the bytes in the array are already zeroed out.
     */
    public BitSet(long numSpaces) {
        // TODO: Allocate enough bytes to fit numSpaces
    }

    /**
     * Flips the number at a particular index in the bit set.
     *
     * Think hard about how to exactly index into the BitSet given the index. A good way to think about what this method
     * is supposed to do is to think of this as the unary not (~) operator but instead of working on a long or an integer,
     * this should be flipping a single bit.
     *
     * As a hint, you'll first need to determine the index into allBytes to get the correct long,
     * then use bitshifting magic with operators (<<, >>, ~, |, &, ^) to flip just that bit that you want to set.
     *
     * If the index given is negative, throw a new IllegalArgumentException. See exceptions here: https://www.geeksforgeeks.org/throw-throws-java/
     * If the index given is larger than the number of spaces, resize allBytes, copy all the existing bytes
     * into a new array that has a size of twice the capacity, then flip the index in the resized BitSet.
     *
     * @param index the index to flip. If the bit at that index is set, then flip it to 0, otherwise flip it to 1.
     */
    public void flip(int index) {
        throw new NotImplementedException();
    }

    /**
     * Returns whether or not the bit at index is set in the BitSet.
     *
     * If the index passed in does not exist, return false.
     *
     * @param index the index to look at
     */
    public boolean contains(int index) {
        throw new NotImplementedException();
    }

    /**
     * Clears the BitSet.
     */
    public void clear() {
        throw new NotImplementedException();
    }
}
