class Exceptions6 {
    static int g(int a) throws Exception {
        if (a < 0)
            throw new Exception();
        return 1;
    }

    static int f(int a) {
        int x;
        try {
            x = g(a);
            x++;
        } catch (Exception e) {
            x = 0;
        }
        return x;
    }

    public static void main(String args[]) throws Exception {
        f(42);
    }
}
