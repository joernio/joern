import java.io.*;

// Reference code for Foo.bin
public class Foo {

    static String parse(BufferedReader reader) throws IOException {
        String line = reader.readLine();
        String output = "";
        while (line != null) {
            String newLine = "";
            newLine = newLine + foo1(line);
            newLine = newLine + foo2(line);
            output = output + newLine;
            line = reader.readLine();
        }
        return line;
    }

    static String foo1(String in) {
        int randomOperation = 1 + 3 * 5;
        String out = foo3(in);
        return out;
    }

    static String foo2(String in) {
        int randomOperation = 3 + 6 / 5;
        String out = foo4(in);
        return in;
    }

    static String foo3(String in) {
        String randomOperation = "Foo " + in;
        return in;
    }

    static String foo4(String in) {
        String randomOperation = "Bar " + in;
        return in;
    }

    public static void main(String[] args) throws IOException {
        BufferedReader reader = new BufferedReader(new FileReader("source"));
        PrintWriter writer = new PrintWriter(new FileWriter("sink"));
        String out1 = parse(reader);
        String out2 = parse(reader);
        String out3 = parse(reader);
        String out4 = parse(reader);
        writer.println(out1);
        writer.println(out2);
        writer.println(out3);
        writer.println(out4);
    }

}
