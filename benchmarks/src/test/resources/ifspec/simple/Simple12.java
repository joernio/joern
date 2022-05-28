import java.util.*;
import java.lang.*;
import java.io.*;
import java.math.BigInteger;

class Simple12 {
    public static BigInteger compute(BigInteger h, BigInteger l) {
        /* x^6 + 3*x^4 + 3*x^2 + 1 ?= 0 */
        if ((h.pow(6).add(h.pow(4).multiply(BigInteger.valueOf(3))).add(h.pow(2).multiply(BigInteger.valueOf(3)))
                .add(BigInteger.valueOf(1))).compareTo(BigInteger.valueOf(0)) == 0) {
            return h;
        } else {
            return l;
        }
    }

    public static void main(String[] args) throws java.lang.Exception {
        Random r = new Random(System.currentTimeMillis());
        BigInteger h = new BigInteger(32, r);
        BigInteger l = new BigInteger(32, r);
        System.out.println(h.toString());
        System.out.println(l.toString());
        BigInteger c = compute(h, l);
        System.out.println(c.toString());
    }
}
