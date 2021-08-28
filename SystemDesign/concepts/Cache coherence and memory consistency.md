As far as I know, there's no such thing is "cache" consistency. What you are referring to is probably memory consistency.

**Cache coherency is relevant in a multicore system where each processor has a private L1 cache and at least one other level is shared. Memory consistency is relevant whether there are caches or not.**

I like the way my professor taught it so I'll give the same example:

Let's say you are the main memory and you have two friends A and B who are two cores in a multicore environment:

A wants to go for a movie on Saturday. So he texts you asking for a good time. You reply back saying 9 PM is a good time. A gets the text, reads it and he is happy. He is also expecting you to co-ordinate the times with B.

A few minutes later, B texts you asking for the time of the movie. You send him the same reply you sent to A, 9 PM. At this point in time, all three of you are agreeing on the same time, that is to say, all of you have the same copy of data.

Something comes up in B's calendar and he needs to reschedule it. So he texts you saying instead of 9, you all will go for the movie at 10 PM. You get the text and your timing is updated. But you fail to pass on this message to A, who still thinks 9 PM is on.

At this point, you (main memory) and B (one of the cores) have the same data, but A (another core) has stale data, since the "write" to the data by B was never propagated to A. A will still show up at 9 PM for the movie, won't find either of you, get pissed off and watch the movie all alone, live texting you the entire story spoiling everything for you!

**This is what cache coherency is all about. It's about making sure that every private copy is the same everywhere and if any changes are made, those changes are propagated to every private cache.**

We can use the same example to understand memory consistency as well, but in this case, you are also a core:

A wants to go for the movie on Saturday at 9 PM. So he sends out a text_1 saying "Movie at 9" to both, you and B. A few minutes later, his plan changes and now he wants to watch it at 10 PM instead. So he sends out another text_2 saying "Movie at 10". So both, you and B are informed of the time changes, but something funky happens.

B's telephone network is crappy and he receives the text_2 BEFORE text_1. B is thinking the initial plan was 10 PM that got moved to 9 while you are thinking it was 9 PM that got moved to 10. In this case, both of you got updated copies, but at different times in a different order. Either ways, someone is going to get pissed and spoil the movie for the others.

**This is memory consistency. It's not enough that a core sees the write happening to shared data, it's also important that they see the writes in the same order as other cores. Otherwise, data will be inconsistent across cores.**

Hope this helps.