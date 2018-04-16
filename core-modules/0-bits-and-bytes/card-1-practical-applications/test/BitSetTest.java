import org.junit.Before;
import org.junit.Test;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

/**
 * The associated tests to ensure that your implementation of your BitSet works as intended.
 *
 * After these tests pass, move onto taking a look at the implementation of HomeTracker to see how to use it!
 */
public class BitSetTest {

    private static final int INITIAL_SIZE = 1000;
    private BitSet bitSet;

    @Before
    public void setup() {
        bitSet = new BitSet(INITIAL_SIZE);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testFlipWithNegativeIndex() {
        bitSet.flip(-1);
    }

    @Test
    public void testThatAllBitsNotSetOnCreation() {
        for (int index = 0; index < INITIAL_SIZE; index++) {
            assertFalse(bitSet.contains(index));
        }
    }

    @Test
    public void testFlipWithinBounds() {
        bitSet.flip(5);
        // We just set a bit! It should be in the bit set now
        assertTrue(bitSet.contains(5));
    }

    @Test
    public void testFlipTwiceWithinBounds() {
        bitSet.flip(5); // Flip the bit at index 5 to be 1
        bitSet.flip(5); // Back to 0 so it should return false

        assertFalse(bitSet.contains(5));
    }

    @Test
    public void testFlipOutsideBounds() {
        // Let's try resizing by flipping the 1001st bit!
        bitSet.flip(INITIAL_SIZE + 1);

        // Resize the array to accommodate the new element

        assertTrue(bitSet.contains(INITIAL_SIZE + 1));
    }

    @Test
    public void testClear() {
        // Setup the BitSet by flipping on all the bits
        for (int index = 0; index < INITIAL_SIZE; index++) {
            bitSet.flip(index);
        }

        // Clear it!
        bitSet.clear();

        // All the bits should be set to be off
        for (int index = 0; index < INITIAL_SIZE; index++) {
            assertFalse(bitSet.contains(index));
        }
    }

}
