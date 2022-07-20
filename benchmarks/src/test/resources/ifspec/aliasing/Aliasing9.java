class Aliasing9 {

    static class A {
        int val;

        A(int val) {
            this.val = val;
        }
    }

    static int secret = 42;

    public static void main(String[] arg) {
        A a = new A(secret);
        A b = new A(5);
        A c = b;

        b = a;

        a.val = 2;

        System.out.println(c.val);
    }
}
