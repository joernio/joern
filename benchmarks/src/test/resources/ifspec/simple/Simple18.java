class Simple18 {
    public int low;
    private int high;

    private static int h;
    private static int l;

    private int[] transaction;

    public static void main(String[] args) {

        Simple18 w = new Simple18();
        w.setBillingAdr(l, l);
        w.setDeliveryAdr(h, h);
        w.getBillAdr();
        w.getDeliverAdr();
    }

    private Address bill;
    private DAddress delivery;

    public void setBillingAdr(int name, int street) {
        bill = new Address();
        bill.name = name;
        bill.street = street;
    }

    public void setDeliveryAdr(int name, int street) {
        delivery = new DAddress();
        delivery.name = name;
        delivery.street = street;
    }

    public int getBillAdr() {
        return this.bill.street;
    }

    public int getDeliverAdr() {
        return this.delivery.street;
    }

    public static class Address {
        public int name;
        public int street;
    }

    public static class DAddress extends Address {
        public int name;
        public int street;
    }
}
