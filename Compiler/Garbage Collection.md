# GC
[一文看懂JVM内存布局及GC原理-InfoQ](https://www.infoq.cn/article/3wyretkqrhivtw4frmr3)

## 原理

### Identify whether some obj can be removed from memory
###### Reference counter
![[Pasted image 20210801182343.png]]

###### Reachability Analysis
![[Pasted image 20210801182431.png]]


### GC Algorithm

###### Mark and Sweep
![[Pasted image 20210801182732.png]]

###### Mark and Copy

![[Pasted image 20210801182801.png]]

###### mark-compact

![[Pasted image 20210801182836.png]]


###### generation-collect

> 内存中的对象，大致可以分为两类：*有些生命周期很短*，比如一些局部变量/临时对象，而*另一些则会存活很久*，典型的比如 websocket 长连接中的 connection 对象!

 
![[Pasted image 20210801183002.png]]
纵向 y 轴可以理解分配内存的字节数，横向 x 轴理解为随着时间流逝（伴随着 GC）


![[Pasted image 20210801183047.png]]

- 年青代 (Young Generation)
	- Eden
	- S0
	- S1
- 老年代 (Old Generation)
- 永久代 (Permanent Generation)


## Object Allocation

1. 刚开始时，对象分配在 Eden 区，s0（即：from）及 s1（即：to）区，几乎是空着
	![[Pasted image 20210801183331.png]]
2.  随着应用的运行，越来越多的对象被分配到 Eden 区;
	![[Pasted image 20210801183530.png]]
3. 当 Eden 区放不下时，就会发生 **minor GC** (也被称为 **young GC**)
	1. use mark-sweep on Eden
	2. use mark-copy on $\mathrm{Eden} \to \mathrm{S0}$
	![[Pasted image 20210801183920.png]]
4. Object Aging. 
	> 随着时间推移，Eden 如果又满了，再次触发 minor GC，同样还是先做标记，这时 Eden 和 s0 区可能都有垃圾对象了（下图中的黄色块），注意：这时 s1（即：to）区是空的，**s0 区和 Eden 区的存活对象，将直接搬到 s1 区。然后将 Eden 和 s0 区的垃圾清理掉，这一轮 minor GC 后，Eden 和 s0 区就变成了空的了**。
	![[Pasted image 20210801184311.png]]
> 对于那些比较“长寿”的对象一直在 s0 与 s1 中挪来挪去，一来很占地方，而且也会造成一定开销，降低 gc 效率，于是有了“代龄(age)”及“晋升”。

> 对象在年青代的 3 个区(edge,s0,s1)之间，每次从 1 个区移到另 1 区，年龄+1，在 young 区达到一定的年龄阈值后，将晋升到老年代。下图中是 8，即：挪动 8 次后，如果还活着，下次 minor GC 时，将移动到 Tenured 区。

![[Pasted image 20210801184758.png]]
![[Pasted image 20210801184809.png]]
![[Pasted image 20210801184819.png]]