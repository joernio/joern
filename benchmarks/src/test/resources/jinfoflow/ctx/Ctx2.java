import java.io.*;

/**
 * Created by neville on 07/11/2016.
 */
public class Ctx2 {
    static class Box1 {
        private Object o;
        Box1(Object o) {
            this.o = o;
        }
        Object get() { return o; }
    }

    static class Box2 {
        private Box1 box1;
        Box2(Object o) {
            box1 = new Box1(o);
        }

        Object get() {
            return get1();
        }

        private Object get1() {
            return box1.get();
        }
    }

    public static void main(String[] args) throws IOException {
        BufferedReader reader = new BufferedReader(new FileReader("source"));
        PrintWriter writer = new PrintWriter(new FileWriter("sink"));
        Box2 box2 = new Box2(reader.readLine()); // source
        String out = (String) box2.get();
        writer.println(out); // sink

    }
}
