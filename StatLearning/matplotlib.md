![[Pasted image 20210824220835.png]]
-   Figure `fig = plt.figure()`: 可以解释为画布。
	-   画图的第一件事，就是创建一个画布figure，然后在这个画布上加各种元素。
-   Axes `ax = fig.add_subplot(1,1,1)`: 不想定义，没法定义，就叫他axes！
	-   首先，这个不是你画图的xy坐标抽！
	-   希望当初写这个lib的时候他们用一个更好的名字。。。
	-   可以把axes理解为你要放到画布上的各个物体。比如你要画一个太阳，一个房子，一个车在画布上，那么太阳是一个axes，房子是一个axes，etc。
	-   如果你的figure只有一张图，那么你只有一个axes。如果你的figure有subplot，那么每一个subplot就是一个axes
	-   axes是matlibplot的宇宙中心！axes下可以修改编辑的变量非常多，基本上能包含你的所有需求。
-   Axis `ax.xaxis/ax.yaxis`: 对，这才是你的xy坐标轴。
	-   每个坐标轴实际上也是由竖线和数字组成的，每一个竖线其实也是一个axis的subplot，因此`ax.xaxis也存在axes`这个对象。对这个axes进行编辑就会修改xaxis图像上的表现。

![[Pasted image 20210824220843.png]]