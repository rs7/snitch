package snitch.domain;

import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import java.util.Set;

@Entity
public class Event {
    private String data;

    @GeneratedValue
    @Id
    private Long id;

    @OneToMany(mappedBy = "event")
    private Set<Message> messages;

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

    public Set<Message> getMessages() {
        return messages;
    }

    public Target getTarget() {
        return target;
    }
}
