import java.io.*;

/**
 * Created by neville on 07/11/2016.
 */
public class Ctx4 {
    static class CensoredBufferedReader extends BufferedReader {
        public CensoredBufferedReader(Reader in) {
            super(in);
        }

        public String readLine() {
            return "Safe string";
        }
    }

    static class UpgradedBufferedReader extends BufferedReader {
        public UpgradedBufferedReader(Reader in) {
            super(in);
        }

        public String readLine() throws IOException {
            return ">> " + super.readLine();
        }
    }

    public static void main(String[] args) throws IOException {
        BufferedReader reader = new CensoredBufferedReader(new FileReader("source"));
        BufferedReader reader2 = new UpgradedBufferedReader(new FileReader("source"));

        PrintWriter writer = new PrintWriter(new FileWriter("sink"));
        writer.println(reader.readLine()); // ok

        writer.println(reader2.readLine()); // bad
    }
}
