class Aliasing5 {

    static class A {
        B b;

        A(B b) {
            this.b = b;
        }
    }

    static class B {
        int val;

        B(int val) {
            this.val = val;
        }
    }

    static int secret = 42;

    public static void main(String[] args) {
        B b = new B(1);
        A a = new A(b);

        b.val = secret;

        System.out.println(a.b.val);
    }
}
