/**
 * This is a nice little driver class so we can actually see how to use this HomeTracker!
 *
 * Try running this file as is without any modifications to HomeTracker.java. How much memory are we using?
 *
 * Note that your changes will be to HomeTracker.java and not to this file.
 */
public class HomeTrackerMain {

    public static void main(String[] args) {
        printOutMemoryUsed();
        HomeTracker homeTracker = new HomeTracker();

        System.out.println("Adding: " + Integer.MAX_VALUE + " home ids into the HomeTracker. We're adding over 2 billion homes!");
        for (int homeId = 0; homeId < Integer.MAX_VALUE; homeId++) {
            // In a real system, this data would come from a data source like a database,
            // but for simplicity sake, we'll just put in all the integers in.
            homeTracker.addHome(homeId);
        }

        // To ensure that we've seen all of the homes
        if (allHomesBeenSeen(homeTracker)) {
            System.out.println("Alert! All the homes on the website have been seen!");
        }

        // We've added a *ton* of new homes! How much memory are we using now?
        printOutMemoryUsed();
    }

    private static boolean allHomesBeenSeen(HomeTracker homeTracker) {
        for (int homeId = 0; homeId < Integer.MAX_VALUE; homeId++) {
            if (!homeTracker.hasSeenHome(homeId)) {
                return false;
            }
        }
        return true;
    }

    private static void printOutMemoryUsed() {
        Runtime runtime = Runtime.getRuntime();
        long usedMemory = runtime.totalMemory() - runtime.freeMemory();
        System.out.println("Total memory used: " + usedMemory + " bytes.");
    }
}
