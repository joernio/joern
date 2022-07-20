class Simple10 {
    public int low;
    private int high;

    public static void main(String[] args) {
        Simple10 ifm = new Simple10();
        ifm.insecure_if_high_n1(42);
    }

    int insecure_if_high_n1(int high) {
        int low;
        if (high > 0) {
            low = n5(high);
        } else {
            low = 7;
        }
        low = n1(high);
        return low;
    }

    int n1(int x) {
        return 27;
    }

    int n5(int x) {
        return 15;
    }

}
