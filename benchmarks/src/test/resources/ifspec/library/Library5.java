import java.util.ArrayList;

class Library5 {
    public static void main(String[] args) {
        int value = 5;
        System.out.println("Running simpleListToArraySize");
        System.out.println("Secret value:   " + value);
        System.out.println("Returned value: " + Library5.listArraySizeLeak(value));
    }

    /**
     * Returns the number that was given, by passing
     * adding elements to a list, converting to an array
     * and returning its size.
     *
     * @param h secret value
     * @return value given
     */
    public static int listArraySizeLeak(int h) {
        ArrayList<Integer> list = new ArrayList<Integer>();

        for (int i = 0; i < h; i++) {
            list.add(42);
        }

        Object[] array = list.toArray();

        return array.length;
    }
}
