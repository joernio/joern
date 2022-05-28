class Exceptions4 {

    private static class T extends Exception {
    }

    /** Main test method parameter is the secret, return value is public */
    static boolean foo(boolean h) {
        boolean x = false;
        try {
            if (h) {
                throw new T();
            } else {
                throw new T();
            }
        } catch (T t) {
            x = true;
        }
        return x;
    }

    public static void main(String[] args) {
        foo(randBool());
    }

    /** Helper method to obtain a random boolean */
    static boolean randBool() {
        return true;
    }

    /** Helper method to obtain a random integer */
    static int randInt() {
        return 42;
    }

}
