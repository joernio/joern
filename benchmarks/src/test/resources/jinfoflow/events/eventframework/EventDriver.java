package eventframework;

import events.NewEntityEvent;
import events.NewTransactionEvent;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Collection;

/**
 * Created by neville on 01/11/2016.
 */
public class EventDriver {

    private Collection<EventHandler> eventHandlers = new ArrayList<EventHandler>();


    public Event newEvent(String eventName) {
        Event event;
        try {
            event = (Event) Class.forName("events." + eventName + "Event").newInstance();
            event.setMetaData(metaData);
        } catch (InstantiationException e1) {
            e1.printStackTrace();
            return null;
        } catch (IllegalAccessException e1) {
            e1.printStackTrace();
            return null;
        } catch (ClassNotFoundException e1) {
            e1.printStackTrace();
            return null;
        }
        return event;
    }

    public UninitialisedEvent createEvent(String eventName) {
        return new UninitialisedEvent(this, newEvent(eventName));
    }

    public void raiseEvent(Event event) {
        String eventName = "handle" + event.getEventName();
        for (EventHandler eventHandler : eventHandlers) {
            Boolean hasHandler = false;
            for (Method method: eventHandler.getClass().getMethods()) {
                if (method.getName().equals(eventName)) {
                    hasHandler = true;
                    break;
                }
            }
            // a fancy way to calling the appropriate event handler

            try {
                if (hasHandler) {
                    Method handlerMethod = eventHandler.getClass().getMethod(eventName, event.getClass());
                    handlerMethod.invoke(eventHandler, new Object[] { event });
                }
            } catch (IllegalAccessException e) {
                e.printStackTrace();
            } catch (InvocationTargetException e) {
                e.printStackTrace();
            } catch (NoSuchMethodException e) {
                e.printStackTrace();
            }
        }
    }

    public void registerAsEventHandler(EventHandler eventHandler) {
        eventHandlers.add(eventHandler);
    }

    private String metaData;

}
