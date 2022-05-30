package events;

import eventframework.Event;

import java.math.BigDecimal;

/**
 * Created by neville on 01/11/2016.
 */
public class NewTransactionEvent extends Event {

    public static final String EVENT_NAME = "NewTransaction";

    private String originatorEntityId;

    private String destinationEntityId;

    private BigDecimal amount;

    private String reason;

    public BigDecimal getAmount() {
        return amount;
    }

    public void setAmount(BigDecimal amount) {
        this.amount = amount;
    }

    public String getReason() {
        return reason;
    }

    public void setReason(String reason) {
        this.reason = reason;
    }

    @Override
    public String getEventName() {
        return "NewTransaction";
    }

    @Override
    public String getAggregateId() {
        return originatorEntityId;
    }

    public Event getCopy() {
        return null;
    }

    public String getOriginatorEntityId() {
        return originatorEntityId;
    }

    public void setOriginatorEntityId(String originatorEntityId) {
        this.originatorEntityId = originatorEntityId;
    }

    public String getDestinationEntityId() {
        return destinationEntityId;
    }

    public void setDestinationEntityId(String destinationEntityId) {
        this.destinationEntityId = destinationEntityId;
    }
}
