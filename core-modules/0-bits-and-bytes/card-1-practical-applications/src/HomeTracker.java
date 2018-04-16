import java.util.HashSet;
import java.util.Set;

/**
 * Redfin is a popular site used to easily search for homes on the market for when people want to either buy a home or sell
 * their own home.
 *
 * There are hundreds of thousands of homes on the market with some homes being more popular than others.
 * The question that we're going to try to answer today is if all the homes that are on Redfin have been seen at least once.
 *
 * We want to support two operations, addHome and hasSeenHome. This is perfect actually, we can just use a HashSet!
 *
 * However, this season's housing inventory is crazy and there's far more homes than our HomeTracker can handle.
 *
 * Try running HomeTrackerMain.java with this implementation of HomeTracker to see for yourself. We can't even get an answer
 * because there are just too many home ids for our set to handle!
 *
 * The reason why we're running out of memory is because each Integer object actually takes up 16 bytes which is equivalent
 * to 128 bits.
 *
 * For a little context, Java makes a distinction between int and Integer.
 *
 * An 'int' in Java takes up 4 bytes just like we discussed in the previous card. 'int's are not objects, but rather
 * primitives. They take up less space, but you cannot do things like:
 *
 * int x = 10;
 * x.toString(); // Whoops this isn't an object, it's just a primitive so we don't have any methods with it!
 *
 * In contrast, Integer is actually an object meaning that the Integer object equivalent will work:
 *
 * Integer y = new Integer(10); // Technically, 'Integer y = 10;' would work too since Java does the wrapping
 * y.toString(); // This works!
 *
 * Although we get magical object capabilities with Integer, they end up taking more space than primitives which can
 * be a big deal when we don't need all the other functionality especially in our application where we just want to see
 * whether or not we've seen a particular value.
 *
 * For more information about specifics about why Integer takes 16 bytes, check out: https://www.javamex.com/tutorials/memory/object_memory_usage.shtml
 *
 * In any case, since Java Integer takes up 16 bytes or 128 bits, rather than using 128 bits to indicate that we've seen
 * a single integer, now that we've constructed our own BitSet, we can use a single bit to do so! 127 bit savings might
 * not sounds like a lot, but when we're dealing with potentially billions of values, these memory savings really add up.
 *
 * Replace this implementation from using a Set<Integer> to using your BitSet that you created so that when HomeTrackerMain.java
 * is run, it won't run out of memory!
 */
class HomeTracker {

    private Set<Integer> allHomeIds;

    HomeTracker() {
        allHomeIds = new HashSet<>();
    }

    void addHome(int id) {
        allHomeIds.add(id);
    }

    boolean hasSeenHome(int id) {
        return allHomeIds.contains(id);
    }

}
