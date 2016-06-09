package snitch.domain;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.ManyToMany;
import java.util.Set;

@Entity
public class Target {
    @ManyToMany(mappedBy = "targets")
    private Set<Customer> customers;

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

    public Integer getId() {
        return id;
    }

    public Integer getVkID() {
        return vkID;
    }
}
