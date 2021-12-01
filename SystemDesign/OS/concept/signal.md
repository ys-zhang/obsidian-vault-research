`signal(7)`,  `signal-safty(7)`

see [[ptrace]], [[fork]], [[exec]].


# signal state: blocking/pending

信号的处理动作称为信号递达（**Delivery**），信号从产生到递达之间的状态，称为信号未决（**Pending**）.

被阻塞(**blocking**)的信号产生时将保持在未决(**pending**)状态，直到进程解除对此信号的阻塞，才执行递达(**delivery**)的动作.

常规信号在递达之前产生多次只计一次，而实时信号(**real time signal**)在递达之前产生多次可以依次放在一个队列里.

阻塞信号集也叫做当前进程的信号屏蔽字（**Signal Mask**），这里的“屏
蔽”应该理解为阻塞而不是忽略。`sigset_t`类型对于每种信号用一个bit表示“有效”或“无效”状态.

调用函数`sigprocmask`可以读取或更改进程的 _signal mask_.
`sigpending`读取当前进程的未决信号集