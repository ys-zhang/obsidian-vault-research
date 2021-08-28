# Chapter 1. In The Beginning…

1. Why container?
		people started to really feel the pain of managing their applications at the VM layer.
2. Why orchestration tools? 
	1. containers are not full VM, need to orchestrate them to **run efficiently and resiliently**.
	2. execution needs to be **scheduled and managed**
	3. When they die (and they do), they need to be seamlessly **replaced and re-balanced.** (containers are designed to be short-lived and fragile)


> VMs are a lot like commercial passenger jets. They contain full operating systems—including firewalls and other protective systems—and can be super resilient. Containers, on the other hand, are like the LM. They’re optimized for weight and therefore are a lot less forgiving.
> **In the real world, individual containers fail a lot more than individual virtual machines.**


#  Chapter 2. Go Big or Go Home!

> Using that lens, the challenge shifts from configuration management to orchestration, scheduling, and isolation. 
> - A failure of one computing unit cannot take down another (**isolation**)
> - resources should be reasonably **well balanced geographically to distribute load** (**orchestration**)
> - and you need to **detect and replace failures near instantaneously** (**scheduling**).

## thinking in terms of services (and sometimes _micro-services_) instead of _applications_

A **service** is a process that:
1.  is designed to do a small number of things (often just one).
2.  has no user interface and is invoked solely via some kind of API.

An **application**, on the other hand, is pretty much the opposite of that. It has a user interface (even if it’s just a command line) and often performs lots of different tasks. It can also expose an API, but that’s just bonus points in my book.


## K8S layout

![[Pasted image 20210821190008.png]]

Bunches of machines sit networked together in lots of data centers. Each of those machines is hosting one or more Docker containers. Those worker machines are called **_nodes_**.


Other machines run special coordinating software that schedule containers on the nodes. These machines are called ***masters***.

Collections of **_masters_** and ***nodes*** are known as **_clusters_**.

### Master
runs the following:

1. **API Server**: handle API calls.
2. **[[etcd]]**:  a service whose job is to **keep and replicate the current configuration and run state** of the cluster.
3. **Scheduler and Controller Manager**—These processes **schedule containers onto target nodes.**


### Node

runs the following
1. **Kubelet**—A [daemon](http://bit.ly/1z9iphC) that runs on each node whose job is to **respond to commands from the master to create, destroy, and monitor the container**s on that host.
2. **Proxy**—This is a simple network proxy that’s used to separate the IP address of a target container from the name of the service it provides.
3. **cAdvisor** (optional)—[Container Advisor (cAdvisor)](http://bit.ly/1izYGLi) is a special daemon that **collects, aggregates, processes, and exports information about running containers.** This information includes information about *resource isolation, historical usage, and key network statistics*.


### Other concepts

- [[Pod (K8S) | Pod]] Kubernetes schedules and orchestrates things at the pod level, not the container level.
- [[Volume]]: A K8S volume is defined at the **pod level**.
	- (**durability**) If the volume is bound to the pod, on the other hand, then the data will survive the death and rebirth of any container in that pod.
	- (**communication**) containers can talk through volume.
	
> You are responsible for preserving the state of your application. 
> Instead of storing your state in memory in some non-durable way, you should think about using a shared data store like Redis, Memcached, Cassandra, etc.

# Chapter 3. Organize, Grow, and Go

## Naming service for [[Pod (K8S)]].

###  Label

> A label is a key/value pair* that you assign to a Kubernetes object (a pod in this case) as identifiers.

```yaml
“labels”: {
	“key”: “value”
}
```

- Keys are a combination of zero or more prefixes followed by a “/” character followed by a name string.
- Values have the same rules but cannot be any longer than 63 characters.


###  Label selector

- equality selector: `k1 = v1, k2 != v2`  (comma is interpreted as `and` )
- set selector: `k1 in (v1, v2), k2 notin (v3,v4), k3`

### Annotation

annotation can be used to store auxiliary information on pods. 


##  Replication Controllers

> Multiple copies of a pod are called **_replicas_**.

> The process that manages these replicas is the _replication controller_. **Specifically, the job of the replication controller is to make sure that the correct number of replicas are running at all times**.


The way the controller does this is by following a set of rules you define in a **pod template**.

You can usually count on a replication controller to be more long-lived than any one pod (and certainly more than any one container), but you should not think of them as invincible.


The basic idea of scaling is that requests coming from your users will hit a front end load balancer and be directed to your cluster. As the traffic increases to the load balancer—commonly measured in queries per second (QPS)—the balancer will change the replicas value in the controller and more pods will be created. 

## Service


> A **_service_** is a long-lived, well-known endpoint that points to a set of pods in your cluster. It consists of three things—**an external IP address** (known as a _portal_, or sometimes a _portal IP_), **a port, and a label selector**.

![[Pasted image 20210821213251.png]]

> The service is exposed via a small proxy process. When a request comes in for an endpoint you want to expose, the _service proxy_ decides which pod to route it to via a label selector.


### Service Discovery with Environment Variables

When a pod wants to consume another service, it needs a way to do a lookup and figure out how to connect. Kubernetes provides two such mechanisms—**environment variable** and **DNS**.


- When a pod exposes a service on a node, Kubernetes creates a set of environment variables on that node to describe the new service. That way, other pods on the same node can consume it easily.
- As you can imagine, managing discovery via environment variables is not super scalable, so Kubernetes gives us a second way to do it: Cluster DNS.


> **Cluster DNS**: a service for discovering services.

These DNS pods contains three special containers:
1. [[etcd]]—Which will store all the actual lookup information
2. [[SkyDns]]—A special DNS server written to read from [[etcd]]. You can find out more about it at [skynetservices/skydns: DNS service discovery for etcd (github.com)](https://github.com/skynetservices/skydns).
3. [[Kube2sky]]—A Kubernetes-specific program that watches the master for any changes to the list of services and then publishes the information into [[etcd]]. [[SkyDns]] will then pick it up.

## Exposing service outside of the cluster

1. **Direct Access**: The simplest thing for you to do is to configure your firewall to pass traffic from the outside world to the portal IP of your service. this strategy is not particularly fault tolerant. You are limited to just one pod to service the request.
2. **DIY Load Balancing**: The next thing you might try is to put a load balancer in front of your cluster and populate it with the portal IPs of your service
3. **Managed Hosting**
	All the major cloud providers that support Kubernetes also provide a pretty easy way to scale out your load. When you define your service, you can include a flag named `CreateExternalLoadBalancer` and set its value to true.
	When you do this, the cloud provider will automatically add the portal IPs for your service to a fleet of load balancers that it creates on your behalf. The mechanics of this will vary from provider to provider.


## Health Checking

> One of the great things about Kubernetes is that it will evict degraded pods and replace them so that it can make sure you always have a system performing reliably at capacity.

1. (Low-Level Process Checking) The *Kubelet* running on each node will talk to the Docker runtime to **make sure that the containers in your pods are responding**. If they aren’t, they will be killed and replaced.
2. **automatic health checking**: like heart beat
	1. TCP Socket
	2. HTTP GET
	3. Container Exec
3. Manual Application Level Checking

##  The Life of a Client Request

1. The client looks up your endpoint via DNS and attempts a connection
2. More likely than not, that endpoint is some kind of frontend load balancer. This load balancer figures out which cluster it wants to route the request to and then sends the request along to the portal IP for the requested service.
3. The proxy service uses a label selector to decide which pods are available to send the request to and then forwards the query on to be serviced.


> QUESTION: how a service proxy decides which pod is going to service the request if more than one matches the label selector? (at least we have Round Robin )