class Library7 {
    public static void main(String[] args) {
        int value = 5;
        noLeak(value);
    }

    static long inThePast = 1456223086265L; // 23 Feb. 2016 11:24

    public static int noLeak(int h) {
        long curr = System.currentTimeMillis();
        if (curr < inThePast) {
            return h;
        }
        return 0;
    }
}
