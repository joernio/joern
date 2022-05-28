class Simple9 {
    public static int low;
    private static int high;

    public static void main(String[] args) {
        Simple9 ifm = new Simple9();
        ifm.secure_if_high_n5_n1();
    }

    void secure_if_high_n5_n1() {
        if (high > 0) {
            low = n5(high);
        } else {
            high = -high;
            low = n5(high + low);
        }

    }

    int n5(int x) {
        high = 2 * x;
        return 15;
    }

}
