import eventframework.Event;
import eventframework.EventDriver;
import eventframework.EventHandler;
import events.NewEntityEvent;
import events.NewTransactionEvent;
import events.UnusedEvent;

import java.io.*;

/**
 * Created by neville on 01/11/2016.
 */
public class Events4 implements EventHandler{

    private final String _tainted;

    private String tainted;

    PrintWriter writer = null;
    EventDriver driver = null;

    public Events4(EventDriver driver, String tainted) throws IOException{
        this.driver = driver;
        this._tainted = tainted;
        writer = new PrintWriter(new FileWriter("sink"));
        driver.registerAsEventHandler(this);
    }

    public static void main(String[] args) throws IOException{
        BufferedReader reader = new BufferedReader(new FileReader("source"));
        EventDriver driver = new EventDriver();
        Events4 app = new Events4(driver, reader.readLine());
        Event event = driver.newEvent("NewEntity");
        event.setMetaData("NewTransaction");
        driver.raiseEvent(event);
        driver.raiseEvent(event);
    }

    public void handleNewEntity(NewEntityEvent e) throws IOException {
        driver.raiseEvent(driver.newEvent(e.getMetaData()));
        writer.println(tainted); // not ok second time round
    }

    public void handleNewTransaction(NewTransactionEvent e) throws IOException {
        this.tainted = _tainted; // ok
    }

}
