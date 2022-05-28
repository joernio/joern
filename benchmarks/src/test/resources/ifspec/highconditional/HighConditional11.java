class HighConditional11$A {
}

class HighConditional11$B extends HighConditional11$A {
}

class HighConditional11$C extends HighConditional11$A {
}

class HighConditional11 {
    static boolean secret = false;

    public static void main(String[] args) {
        test();
    }

    public static boolean test() {
        HighConditional11$A obj;

        if (secret) {
            obj = new HighConditional11$B();
        } else {
            obj = new HighConditional11$C();
        }

        boolean reconstructed = obj instanceof HighConditional11$B;
        return reconstructed;
    }
}
