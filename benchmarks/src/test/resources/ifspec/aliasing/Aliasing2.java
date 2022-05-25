class Aliasing2 {

    static class A {
        int val;

        A(int val) {
            this.val = val;
        }
    }

    static private int secret = 42;

    public static void main(String[] args) {
        A a = new A(1);
        A b = a;

        if (secret == 42) {
            a.val = 2;
        } else {
            a.val = 2;
        }

        System.out.println(b.val);
    }
}