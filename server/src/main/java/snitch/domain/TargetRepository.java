package snitch.domain;

import org.springframework.data.repository.CrudRepository;
import org.springframework.data.repository.query.Param;
import org.springframework.data.rest.core.annotation.RestResource;

import java.util.List;

public interface TargetRepository extends CrudRepository<Target, Integer> {
    @RestResource(path = "vk")
    List<Target> findByVkID(@Param("id") Integer vkID);
}
