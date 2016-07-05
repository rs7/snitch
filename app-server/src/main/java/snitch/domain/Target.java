package snitch.domain;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.ManyToMany;
import javax.persistence.OneToMany;
import java.util.List;
import java.util.Set;

@Entity
public class Target {
    @ManyToMany(mappedBy = "targets")
    private Set<Customer> customers;

    @OneToMany(mappedBy = "target")
    private List<Event> events;

    @GeneratedValue
    @Id
    private Integer id;

    @Column(unique = true)
    private Integer vkID;

    protected Target() {
    }

    public Set<Customer> getCustomers() {
        return customers;
    }

    public List<Event> getEvents() {
        return events;
    }

    public Integer getId() {
        return id;
    }

    public Integer getVkID() {
        return vkID;
    }
}
