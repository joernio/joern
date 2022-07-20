import java.util.Random;

class Arrays1 {
    public static int f(int h, int l, int[] a) {
        for (int i = 0; i < h; i++) {
            l += a[i];
        }
        return l;
    }

    public static void main(String args[]) {
        int h = randInt();
        int l = randInt();
        int[] arr = randIntArray(h);
        f(h, l, arr);
    }

    /** Helper method to obtain a random integer */
    static int randInt() {
        return 42;
    }

    /**
     * Helper method to obtain an array of random
     * integers with the given length
     **/
    static int[] randIntArray(int length) {
        int[] ret = new int[length];
        for (int i = 0; i < length; i++) {
            ret[i] = randInt();
        }
        return ret;
    }
}