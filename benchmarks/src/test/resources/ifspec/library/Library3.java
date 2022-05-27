import java.util.*;
import java.io.*;
import java.lang.*;

class Library3 {
    static int countAlphanumeric(String passwd) {
        int count = 0;
        for (int i = 0; i < passwd.length(); i++)
            if (Character.isDigit(passwd.charAt(i)) || Character.isLetter(passwd.charAt(i)))
                count++;

        return count;
    }

    static int countCaps(String passwd) {
        int count = 0;
        for (int i = 0; i < passwd.length(); i++)
            if (passwd.charAt(i) >= 'A' && passwd.charAt(i) <= 'Z')
                count++;
        return count;
    }

    static int passwordstrength(String passwd) {
        int strength = 0;

        if (countAlphanumeric(passwd) < 3)
            strength = 1;

        if (countCaps(passwd) < 3)
            strength = 2;
        else
            strength = 3;

        return strength;
    }

    public static void main(String args[]) {
        Scanner input = new Scanner(System.in);
        String passwd = input.nextLine();
        int strength = passwordstrength(passwd);
        System.out.println(strength);
    }
}
