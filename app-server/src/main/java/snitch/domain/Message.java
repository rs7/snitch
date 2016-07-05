package snitch.domain;

import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.ManyToOne;

@Entity
public class Message {
    @ManyToOne
    private Customer customer;

    @ManyToOne
    private Event event;

    @GeneratedValue
    @Id
    private Long id;

    private String status;

    protected Message() {
    }

    public Customer getCustomer() {
        return customer;
    }

    public Event getEvent() {
        return event;
    }

    public Long getId() {
        return id;
    }

    public String getStatus() {
        return status;
    }
}
