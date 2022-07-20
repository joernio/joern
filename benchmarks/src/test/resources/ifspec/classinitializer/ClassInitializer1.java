class ClassInitializer1 {

    private static String secret = "secret";
    private static String[] vals = { "a", "b", "c" };

    static class A {
        static {
            vals[0] = secret;
        }

        void leak() {
            System.out.println(vals[0]);
        }
    }

    public static void main(String[] args) {
        A a = new A();
        a.leak();
    }

}
