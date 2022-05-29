package eventframework;

import java.lang.reflect.InvocationTargetException;

/**
 * Created by neville on 01/11/2016.
 */
public class UninitialisedEvent {
    private final Event event;
    private final EventDriver driver;

    public UninitialisedEvent(EventDriver driver, Event event) {
        this.driver = driver;
        this.event = event;
    }

    public UninitialisedEvent apply(String name, Object value) {
        try {
            event.getClass().getMethod(name, value.getClass()).invoke(event, value);
        } catch (IllegalAccessException e) {
            e.printStackTrace();
        } catch (InvocationTargetException e) {
            e.printStackTrace();
        } catch (NoSuchMethodException e) {
            e.printStackTrace();
        }
        return this;
    }

    public void raiseEvent() {
        driver.raiseEvent(event);
    }
}
