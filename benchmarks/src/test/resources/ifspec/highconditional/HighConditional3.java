class HighConditional3 {

    public static void main(String[] args) {
        HighConditional3 ifl = new HighConditional3();
        ifl.secure_ifl(17);
    }

    public int secure_ifl(int high) {
        int x = 0;
        int y = 0;
        int low = 23;
        // @ loop_invariant 0 <= y && y <= 10;
        // @ determines low, y, (y < 10 ? x : 0) \by \itself;
        // @ assignable low;
        // @ decreases 10 - y;
        while (y < 10) {
            low = x;
            if (y == 5) {
                x = high;
                y = 9;
            }
            x++;
            y++;
        }
        return low;
    }
}
