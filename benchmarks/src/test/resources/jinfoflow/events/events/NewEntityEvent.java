package events;

import eventframework.Event;

import java.math.BigDecimal;

/**
 * Created by neville on 01/11/2016.
 */
public class NewEntityEvent extends Event {

    private String id;

    private String name;

    private BigDecimal balance;

    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public BigDecimal getBalance() {
        return balance;
    }

    public void setBalance(BigDecimal balance) {
        this.balance = balance;
    }

    @Override
    public String getEventName() {
        return "NewEntity";
    }

    @Override
    public String getAggregateId() {
        return getId();
    }

    public Event getCopy() {
        return null;
    }
}
