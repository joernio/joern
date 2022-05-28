class HighConditional1 {
    public static void main(String args[]) {
        int h = 5;
        int l = 1;
        f(h, l);
    }

    public static int f(int h, int l) {
        while (h > 0) {
            h--;
            l++;
        }
        return l;
    }
}
