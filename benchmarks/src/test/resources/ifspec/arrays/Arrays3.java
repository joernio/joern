class Arrays3 {

    static int secret = 42;

    public static void main(String[] args) {
        int[] arr = new int[secret];

        for (int i = 0; i < Integer.MAX_VALUE; i++) {
            try {
                int j = arr[i];
            } catch (Exception e) {
                System.exit(0);
            }
        }
    }
}
