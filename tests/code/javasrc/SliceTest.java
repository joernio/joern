

public class SliceTest {

    public void foo(boolean b) {
        String s = new Foo("MALICIOUS");
        if (b) {
            s.setFoo("SAFE");
        }
        bar(b);
    }

    public void bar(String x) {
        System.out.println(s);
    }
}