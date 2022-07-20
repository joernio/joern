import java.io.*;

/**
 * Created by neville on 07/11/2016.
 */
public class Ctx1 {


    static class Box {
        String o;

        Box(String o) {
            this.o = o;
        }

        String get() {
            return get1();
        }

        String get1() {
            return get2();
        }

        String get2() {
            return get3();
        }

        String get3() {
            return o;
        }

    }

    public static String boxGetter(Box box) {
        return box.get();
    }

    public static void main(String[] args) throws IOException {
        BufferedReader reader = new BufferedReader(new FileReader("source"));
        PrintWriter writer = new PrintWriter(new FileWriter("sink"));

        Box box = new Box(reader.readLine()); // source
        Box box2 = new Box("Hello world!");

        writer.println(boxGetter(box)); // bad
        writer.println(boxGetter(box2)); // ok

    }
}
