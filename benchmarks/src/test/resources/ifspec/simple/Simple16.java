import java.util.Random;

class Simple16 {
    private static int secret = 42;

    public static void main(String[] args) {
        int output = secret;

        Random random = new Random();
        output += random.nextInt(Integer.MAX_VALUE) - secret;

        System.out.println(Integer.toString(output));
    }
}
