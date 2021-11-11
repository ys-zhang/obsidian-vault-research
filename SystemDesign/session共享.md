origin: [分布式session共享常用的几种方式 - 啃不动地大坚果的个人空间 - OSCHINA - 中文开源技术交流社区](https://my.oschina.net/dajianguo/blog/1830585)

## 1、请求精确定位 粘性会话：session sticky

例如基于访问ip的hash策略，即当前用户的请求都集中定位到一台服务器中，这样单台服务器保存了用户的session登录信息， 如果宕机，则等同于单点部署，会话会丢失，会话不复制。

## 2、session复制共享：session replication

如tomcat自带session共享，主要是指集群环境下，多台应用服务器之间同步session，使session保持一致，对外透明。  
如果其中一台服务器发生故障，根据负载均衡的原理，调度器会遍历寻找可用节点，分发请求，由于session已同步，故能保证用户的session信息不会丢失，会话复制。  
此方案的不足之处：  
必须在同一种中间件之间完成(如:tomcat-tomcat之间).  
session复制带来的性能损失会快速增加.特别是当session中保存了较大的对象,而且对象变化较快时, 性能下降更加显著，会消耗系统性能。 这种特性使得web应用的水平扩展受到了限制。 Session内容通过广播同步给成员，会造成网络流量瓶颈，即便是内网瓶颈。  
在大并发下表现并不好。

## 3、基于cache DB缓存的session共享

基于 memcache/redis缓存的 session 共享  
即使用cacheDB存取session信息，应用服务器接受新请求将session信息保存在cache DB中，当应用服务器发生故障时，调度器会遍历寻找可用节点， 分发请求，当应用服务器发现session不在本机内存时，则去cacheDB中查找，如果找到则复制到本机，这样实现session共享和高可用。

[https://www.jb51.net/article/124641.htm](https://www.oschina.net/action/GoToLink?url=https%3A%2F%2Fwww.jb51.net%2Farticle%2F124641.htm)

## 4.一些注意的问题

需要注意的是无论是方案2还是方案3 都需要对要保存到session的对象进行序列化。