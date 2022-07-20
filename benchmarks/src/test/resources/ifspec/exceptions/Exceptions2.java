class Exceptions2 {

    private static class T extends Exception {
    }

    /** Main test method parameter is the secret, return value is public */
    static boolean foo(boolean h) {
        try {
            if (h) {
                throw new T();
            }
        } catch (T t) {
            return true;
        }
        return false;
    }

    public static void main(String[] args) {
        foo(randBool());
    }

    /** Helper method to obtain a random boolean */
    static boolean randBool() {
        return System.currentTimeMillis() % 2 == 0;
    }

    /** Helper method to obtain a random integer */
    static int randInt() {
        return (int) System.currentTimeMillis();
    }

}
