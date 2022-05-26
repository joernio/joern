class ClassInitializer3 {

    private static String secret = "secret";

    static class A {
        static {
            System.out.println(secret);
        }

        int add(int a, int b) {
            return a + b;
        }
    }

    public static void main(String[] args) {
        A a = new A();
        a.add(1, 2);
    }

}
