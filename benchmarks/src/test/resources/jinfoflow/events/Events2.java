import eventframework.Event;
import eventframework.EventDriver;
import eventframework.EventHandler;
import events.NewEntityEvent;
import events.NewTransactionEvent;

import java.io.*;

/**
 * Created by neville on 01/11/2016.
 */
public class Events2 implements EventHandler{

    private final String tainted;
    PrintWriter writer = null;
    EventDriver driver = null;

    public Events2(EventDriver driver, String tainted) throws IOException{
        this.driver = driver;
        this.tainted = tainted;
        writer = new PrintWriter(new FileWriter("sink"));
        driver.registerAsEventHandler(this);

    }

    public static void main(String[] args) throws IOException{
        BufferedReader reader = new BufferedReader(new FileReader("source"));

        EventDriver driver = new EventDriver();
        Events2 app = new Events2(driver, reader.readLine());
        Event event = driver.newEvent("NewEntity");
        driver.raiseEvent(event);
    }

    public void handleNewEntity(NewEntityEvent e) throws IOException {
        Event e1 = driver.newEvent("NewTransaction");
        e1.setMetaData(tainted);
        driver.raiseEvent(e1);
    }

    public void handleNewTransaction(NewTransactionEvent e) throws IOException {
        writer.println(e.getMetaData()); // bad
    }

}
