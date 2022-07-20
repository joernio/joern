class Simple17 {

    int h, l;

    int lsink, hsink;

    public void f() {
        if (l == 1)
            set((long) h);
        else
            set(h);
    }

    public void set(long a) {
        lsink = (int) a;
    }

    public void set(int a) {
        hsink = a;
    }

    public static void main(String[] args) {
        Simple17 sd = new Simple17();
        sd.f();
    }
}
