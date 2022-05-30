import java.io.*;

/**
 * Created by neville on 07/11/2016.
 */
public class Ctx5 {
    public static void main(String[] args) throws IOException {
        BufferedReader reader = new BufferedReader(new FileReader("source"));
        PrintWriter writer = new PrintWriter(new FileWriter("sink"));
        writer.println(id(reader.readLine())); // source and sink
        writer.println(id("Hello")); // ok
    }

    public static String id(String o) {
        return o;
    }
}
