import java.util.Random;

class Simple15 {
    private static int secret = 42;

    public static void main(String[] args) {
        int output = secret;

        Random random = new Random();
        if (random.nextBoolean()) {
            output += random.nextInt(Integer.MAX_VALUE - secret);
        } else {
            output -= random.nextInt(-(Integer.MIN_VALUE + secret));
        }

        System.out.println(Integer.toString(output));
    }
}
