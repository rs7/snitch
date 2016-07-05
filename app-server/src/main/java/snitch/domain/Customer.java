package snitch.domain;

import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.ManyToMany;
import javax.persistence.OneToMany;
import java.util.Set;

@Entity
public class Customer {
    @GeneratedValue
    @Id
    private Long id;

    @OneToMany(mappedBy = "customer")
    private Set<Message> messages;

    @ManyToMany
    private Set<Target> targets;

    protected Customer() {
    }

    public Long getId() {
        return id;
    }

    public Set<Message> getMessages() {
        return messages;
    }

    public Set<Target> getTargets() {
        return targets;
    }
}
