class ClassInitializer7 {

    private static String secret = "secret";

    static class A {
        static {
            System.out.println(secret);
        }
    }

    public static void main(String[] args) {
        System.out.println("nothing here.");
    }
}
