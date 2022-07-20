import java.lang.ArithmeticException;

class Exceptions1 {
    public static int divide(int l, int h) {
        int z = 0;
        try {
            z = l / h;
        } catch (ArithmeticException e) {
            System.out.println(h + " is not defined");
        }
        return z;
    }

    public static void main(String[] args) {
        divide(randInt(), randInt());
    }

    /** Helper method to obtain a random integer */
    static int randInt() {
        return 42;
    }
}
