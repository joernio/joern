import eventframework.*;
import events.NewEntityEvent;
import events.NewTransactionEvent;

import java.io.*;

/**
 * Created by neville on 01/11/2016.
 */
public class Events3 implements EventHandler{

    private final EventDriver driver;
    PrintWriter writer = null;

    public Events3(EventDriver driver) throws IOException{
        this.driver = driver;
        writer = new PrintWriter(new FileWriter("sink"));
    }

    public static void main(String[] args) throws IOException{
        BufferedReader reader = new BufferedReader(new FileReader("source"));

        EventDriver driver = new EventDriver();
        Events3 app = new Events3(driver);

        driver.registerAsEventHandler(app);
        NewEntityEvent event = (NewEntityEvent) driver.newEvent("NewEntity");
        event.setId(reader.readLine());
        driver.raiseEvent(event);
    }

    public void handleNewEntity(NewEntityEvent e) throws IOException {
        Event event = driver.newEvent("NewTransaction");
        event.setMetaData(e.getAggregateId());
        driver.raiseEvent(event);
    }

    public void handleNewTransaction(NewTransactionEvent e) throws IOException {
        writer.println(e.getMetaData()); // bad
        writer.println(e.getAggregateId()); // ok
    }

}
