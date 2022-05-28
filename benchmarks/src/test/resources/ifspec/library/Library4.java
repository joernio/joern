import java.util.ArrayList;

class Library4 {

    public static void main(String[] args) {
        int value = 5;
        System.out.println("Running simpleListSize");
        System.out.println("Secret value:   " + value);
        System.out.println("Returned value: " + Library4.listSizeLeak(value));
    }

    /**
     * Returns the number that was given, by passing
     * adding elements to a list and returning its size.
     *
     * @param h secret value
     * @return value given
     */
    public static int listSizeLeak(int h) {
        ArrayList<Integer> list = new ArrayList<Integer>();

        for (int i = 0; i < h; i++) {
            list.add(42);
        }

        return list.size();
    }
}
