class Casting2 {
    static boolean secret = true;

    public static void main(String[] args) {
        test();
    }

    public static boolean test() {
        Casting2$A obj;

        if (secret) {
            obj = new Casting2$B();
        } else {
            obj = new Casting2$C();
        }

        boolean reconstructed = true;

        try {
            Casting2$A test = ((Casting2$B) obj);
        } catch (Exception e) {
            reconstructed = false;
        } finally {
            return reconstructed;
        }
    }


}

class Casting2$A {
}

class Casting2$B extends Casting2$A {
}

class Casting2$C extends Casting2$A {
}