import java.io.*;

/**
 * Created by neville on 07/11/2016.
 */
public class Basic1 {
    public static void main(String[] args) throws IOException {
        BufferedReader reader = new BufferedReader(new FileReader("source"));
        PrintWriter writer = new PrintWriter(new FileWriter("sink"));
        String input = reader.readLine(); // source

        Object[] stuff = new Object[] { input, "inkling", 23 };


        writer.println((String) stuff[0]); // bad

        writer.println((Integer) stuff[0]); // ok, ClassCastException
    }

}
