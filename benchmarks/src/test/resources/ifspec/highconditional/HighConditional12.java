class HighConditional12$Webstore {
    public int low;
    private int high;

    private static int h;
    private static int l;

    private int[] transaction;

    public static void main(String[] args) {

        HighConditional12$Webstore w = new HighConditional12$Webstore();
        w.reinit(true);
    }

    private HighConditional12$VideoSet[] vids;

    public HighConditional12$Video seePreview(int i) {
        if (vids != null && 0 <= i && i < vids.length) {
            return vids[i].vFree;
        } else {
            return null;
        }
    }

    public HighConditional12$Video seePrime(int i) {
        if (vids != null && 0 <= i && i < vids.length) {
            return vids[i].vPrime;
        } else {
            return null;
        }
    }

    public void reinit(boolean h) {
        if (h) {
            if (vids != null && vids.length > 0 && vids[0] != null) {
                HighConditional12$VideoSet v = new HighConditional12$VideoSet();
                v.vFree = vids[0].vFree;
                v.vPrime = vids[0].vPrime;
                vids[0] = v;
            }
        }
    }

    public static class HighConditional12$VideoSet {
        public HighConditional12$Video vFree;
        public HighConditional12$Video vPrime;
    }

    public static class HighConditional12$Video {
    }
}
