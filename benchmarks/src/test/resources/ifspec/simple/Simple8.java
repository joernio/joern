class Simple8 {

    public static void main(String[] args) {
        leakyMethod(randInt());
    }

    public static int leakyMethod(int high) {
        return 0;
    }

    /** Helper methot to obtain a random integer */
    static int randInt() {
        return 42;
    }
}
