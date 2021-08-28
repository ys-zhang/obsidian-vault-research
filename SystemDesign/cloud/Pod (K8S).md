A pod is a **collection of containers and volumes** that are bundled and scheduled together because they **share a common resource**—usually a file system or IP address.

> Kubernetes schedules and orchestrates things at the pod level, not the container level.

![[Pasted image 20210821190205.png]]

[[Kubernetes]] simplifies this scheme by **assigning a shared IP address to the pod**. The containers in the pod all share the same address and communicate with one another via `localhost`. In this way, **you can think of a pod a little like a VM because it basically emulates a logical host to the containers in it**.