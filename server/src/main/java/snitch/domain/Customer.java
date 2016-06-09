package snitch.domain;

import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.ManyToMany;
import java.util.Set;

@Entity
public class Customer {
    @GeneratedValue
    @Id
    private Long id;

    @ManyToMany
    private Set<Target> targets;

    protected Customer() {
    }

    public Long getId() {
        return id;
    }

    public Set<Target> getTargets() {
        return targets;
    }
}
