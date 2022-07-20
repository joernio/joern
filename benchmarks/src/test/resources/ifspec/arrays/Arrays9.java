class Arrays9 {
    public int low;
    private int high;

    private static int h;
    private static int l;

    private int[] transaction;

    public static void main(String[] args) {
        Arrays9 w = new Arrays9();
        w.buyProduct(l, h);
    }

    /*
     * Customer buys the product and wants to pay with the given credit card number.
     * Afterwards, the store returns the bought product
     */
    public int buyProduct(int prod, int cc) {
        this.transaction = new int[2];
        this.transaction[0] = prod;
        this.transaction[1] = cc;

        return this.transaction[0];
    }
}
