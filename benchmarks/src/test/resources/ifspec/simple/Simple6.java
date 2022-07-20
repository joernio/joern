class Simple6 {

    public static void main(String[] args) {
        leakyMethod(randInt());
    }

    public static int leakyMethod(int high) {
        return high;
    }

    /** Helper method to obtain a random integer */
    static int randInt() {
        return 42;
    }
}
