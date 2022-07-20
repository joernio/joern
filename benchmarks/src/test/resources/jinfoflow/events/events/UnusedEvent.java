package events;

import eventframework.Event;

/**
 * Created by neville on 08/11/2016.
 */
public class UnusedEvent extends Event {

    public String getEventName() {
        return null;
    }

    public String getAggregateId() {
        return null;
    }

    public Event getCopy() {
        return null;
    }
}
