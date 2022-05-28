class ClassInitializer6 {

    static String l = "Foo";
    static String h = "Top Secret";
    static String x = "Foo";

    static class A {
        static int f = 17;
        static {
            l = x;
            System.out.println("Ainit");
        }
    }

    static class B {
        static {
            x = h;
            System.out.println("Binit");
        }
    }

    static void f(Object a, Object b) {
    }

    public static void main(String[] args) {
        f(A.f, new B());
        /*
         * int x = A.f;
         * new B();
         */
        System.out.println(l);
    }

}
