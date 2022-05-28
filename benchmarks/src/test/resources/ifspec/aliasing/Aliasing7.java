class Aliasing7 {

    static class A {
        int i;
    }

    static void set(A v1, A v2, int h) {
        v1.i = h;
    }

    static int getNumber() {
        return 42;
    }

    static int test(int i) {
        A v1 = new A();
        A v2 = new A();
        v2 = v1;
        set(v1, v2, i);
        return v2.i;
    }

    public static void main(String args[]) throws Exception {
        test(getNumber());
    }
}
