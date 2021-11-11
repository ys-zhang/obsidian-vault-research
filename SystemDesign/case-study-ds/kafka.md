We introduce Kafka, a ***distributed** messaging system* that we developed for *collecting and delivering **high volumes** of log data with **low latency***. 

# Why Kafka

There is a large amount of “log” data generated at any sizable internet company.
Recent trends in internet applications have made **activity data** a **part of the production data pipeline** used directly in site features.

This production, real-time usage of log data creates new challenges for data systems because its volume is orders of magnitude larger than the “real” data.

Many early systems for processing this kind of data relied on physically scraping log files off production servers for analysis. Those systems are primarily designed for collecting and loading the log data into a data warehouse or Hadoop for offline consumption.

On the one hand, Kafka is **distributed and scalable**, and offers **high throughput**. On the other hand, Kafka provides an **API similar to a messaging system** and allows applications to consume log events in **real time**.

#  Architecture and Design Principles



## Some concepts

A stream of messages of a particular type is defined by a **topic**. A producer can publish messages to a topic.

The published messages are then stored at a set of servers called **brokers**.A consumer can subscribe to one or more topics from the brokers, and consume the subscribed messages by pulling data from the brokers.

## API

```java
// producer
producer = new Producer(…); 
message  = new Message(“test message str”.getBytes()); 
set      = new MessageSet(message); 
producer.send(“topic1”, set);

// consumer
streams[] = Consumer.createMessageStreams(“topic1”, 1);
for (message : streams[0]) { 
	bytes = message.payload(); 
	// do something with the bytes 
}
```

- A **message** is defined to contain just a payload of bytes.
- To subscribe to a **topic**, a **consumer** first creates one or more **message stream**s for the **topic**.
- (**blocking**) Unlike traditional iterators, the message stream iterator never terminates.

## semantics of message passing

1. (**P2P**): multiple consumers jointly consume **a single copy of all messages** in a topic.
2. (**publish/subscribe**): multiple consumers **each retrieve its own copy** of a topic.

## design

![[kafka-arch-fig-1.png]]

a **topic** is divided into **multiple partitions** and each **broker** stores one or more of those partitions.

