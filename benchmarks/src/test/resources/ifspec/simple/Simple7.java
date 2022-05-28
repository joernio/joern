class Simple7 {
    public static void main(String args[]) {
        f(randInt(), randInt());
    }

    public static int f(int h, int l) {
        l = h;
        return l;
    }

    /** Helper methot to obtain a random integer */
    static int randInt() {
        return 42;
    }

}
