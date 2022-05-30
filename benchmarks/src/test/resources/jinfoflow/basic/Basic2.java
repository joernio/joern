import java.io.*;

/**
 * Created by neville on 07/11/2016.
 */
public class Basic2 {
    public static void main(String[] args) throws IOException {
        BufferedReader reader = new BufferedReader(new FileReader("source"));
        PrintWriter writer = new PrintWriter(new FileWriter("sink"));
        String input = reader.readLine(); // source
        String out1 = (input.length() < 4) ? "xyz" : "xyw";
        writer.println(out1.substring(0, 1)); // ok
        writer.println(input.substring(0, 10).substring(0, 5)); // bad
    }

}
