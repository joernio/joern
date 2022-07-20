class HighConditional10 {

    public static void main(String args[]) {
        computeSecretly(12);
    }

    // compare the secret input to 0, set the return value to some
    // intermediate value, but set the return value to 5 eventually
    private static int computeSecretly(int h) {
        int a = 42;

        if (h > 0) {
            a = 5;
        } else {
            a = 3;
        }

        if (h <= 0) {
            a = 5;
        }

        return a;
    }
}
