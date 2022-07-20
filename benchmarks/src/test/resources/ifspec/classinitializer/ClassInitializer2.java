class ClassInitializer2 {

    private static String secret = "secret";
    private static String[] vals = { "a", "b", "c" };

    static class A {
        static {
            vals[1] = secret;
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
