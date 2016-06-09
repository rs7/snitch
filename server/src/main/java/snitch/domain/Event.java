package snitch.domain;

import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.ManyToOne;

@Entity
public class Event {
    private String data;

    @GeneratedValue
    @Id
    private Long id;

    @ManyToOne
    private Target target;

    protected Event() {
    }

    public String getData() {
        return data;
    }

    public Long getId() {
        return id;
    }

    public Target getTarget() {
        return target;
    }
}
