package eventframework;

import java.io.Serializable;

/**
 * Created by neville on 01/11/2016.
 */
public abstract class Event {
    public abstract String getEventName();

    private String metaData;

    public abstract String getAggregateId();

    public void setMetaData(String metaData) {
        this.metaData = metaData;
    }

    public String getMetaData() {
        return metaData;
    }

    public abstract Event getCopy();


}
