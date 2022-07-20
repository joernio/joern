class Simple11$A {
    private int i;

    public Simple11$A(int i) {
        this.i = i;
    }

    public int doPrint() {
        return out(this.i);
    }

    public static int out(int i){
        return i;
    }
}

class Simple11 {

    public static int high = 0;
    public static int low = 1;

    public static void main(String[] args) {
        test(high, low);
    }

    public static int test(int h, int l) {
        Simple11$A a1 = new Simple11$A(l);
        Simple11$A a2 = new Simple11$A(h);

        return a1.doPrint();
    }

}
