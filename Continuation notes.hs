A value of type Concurrent a is a computation that produces an a.  The Concurrent type must accept a continutation that consumes that a.  Continuation must be given a continuation to extract the result of the computation

Concurrency in this lab is not modeled in timeslices, but in successive actions. It is more a cooperative concurrency model, than a preemptive concurrency model. If a process is a chain of actions, then you can run more than one process concurrently by multiplexing the actions in each chain. You can only interrupt a running process between links, before the successive link is invoked.
process 1: A->B->C->D
process 2: P->Q->R->S
concurrency: A->P->B->Q->C->R->D->S
Continuations are the mechanism for stringing actions together to form individual chains. If action 'A' produces a value 'v1' (it is a "producer"), then the continuation is the way to pass that value to an action that consumes 'v1', and does something useful with it (produces something else, that would be passed on, etc.):
A->v1->B->v2->C->v3->...
When you pass a continuation function to an Action, you're telling it: "When you're done, and you've produced a value, then invoke this continuation function with that value to get your successor". It's like a relay race, except instead of passing around a baton, each "runner" is given her successor's phone number.
Regarding "Fork" keep in mind that essentially, we're only runing a single chain of Actions, which is the result of multiplexing many separate processes. There's a single queue of Actions, and there's only one Action running at any one time. Each Action that runs generates either a single successor Action (in case of Atom), two successor Actions (in case of Fork), or no successor (in case of Stop). Whatever is generated is appended to the end of the run queue.

fork :: Concurrent a1 -> Concurrent () ---3 
fork = undefined
data Concurrent a2 = Concurrent (( a2 -> Action ) -> Action )) ---1 
data Action = Atom ( IO Action ) | Fork Action Action | Stop ---2 
I'll explain in the sense of Just follow the type.
In order to satisfy return type of function fork undefined must be something like
Concurrent ( \ x -> y ) (from 1)
What is the type of x and y? 
x and y must be some kind of ( a2 -> Action ) and Action (from 1) ---4.
How do we know the a2 of ( a2 -> Action )? 
think about Concurrent () in the fork type definition .(from 3)
this means type parameter a of Concurrent a is ()
From that, you can deduce the exact type of x.
Now, what about y?
Obviously, we must create Action by using Fork Action Action (from 2 and 4).
because we must fork! 
so, we need two Action to construct Fork.
One Action can be created from x. 
because, x itself is function which has return type Action.
remember the type of x :: ( a2 -> Action ).
Another Action can be created from a1. I assume that you already solved action
and stop. so you can figure out what the type of a1 is.

I want to spare my fellow students some frustration. I believe it was a poor choice to lift much of the content from the Classen article (which I stopped reading once I realized this, because I was worried it would show answers without providing any understanding) almost word for word in some areas, and not adequately adapted for teaching purposes. For anyone who's gotten this far in the course and understood the content of lessons 5, 7, and 11 this lab should be well within reach. So here are my tips (I invite others who completed it to correct or improve these descriptions if necessary):
General
If you're having trouble internalizing how continuations work, think of it as a type "NextStep" instead of a function when looking at the big picture, which is how I'll refer to it instead of "continuation". Internally, because a NextStep produces an action from its unbound primitive a argument and a Concurrent produces an action from its NextStep argument, the data types are ripe for chaining through a recursively nesting structure. There is nothing stopping one from representing a given (a -> Action) as an abstract c such as in a lambda abstraction \c. Consider in pseudo-Haskell:
(\a -> Action) $ ((\b -> Action) $ ((\c -> Action) $ (\d -> Action) ..... -> Action))).
This means that if each of the lower case variables themselves are allowed to take on this NextStep type, where the a, b, c, etc. can be anything, then we can chain as far as we like. And here's the twist: because of lazy evaluation, you don't actually use the NextStep you feed in until you "fire off" one prior in the chain as you connect them. They simply nest and nest and nest, building up the interleaving actions to be executed after the firing shot is sounded. You build the actual order of operations before doing any of them. One of the major mental leaps to make in Haskell/Hugs is that this nested-ness is not "evil" as it is in strict languages because the lazy evaluation leaves most of the computation untouched in its "thunk". On the contrary; this kind of structure in programs is crucial to doing anything more than trivial. If you stare and think hard enough about how return is implemented in Ex4 of this lab and how you might use this value (consider playing around converting it back and forth with basic IO monad functions in a throwaway script), you may get a better feel for it.
I did not personally find the "hacking" part of the Concurrent wrappers that take Haskell away from "thinking like a fundamentalist" to be the real obstacle here. It's the unclear phrasing of what each question wants you to actually do because there are no sample outputs to test against until you attempt the questions. There really should be a proving grounds on which you can try your attempts without losing your chance to answer them correctly.
That said, here is how I would rephrase every question.
Ex0: Make a function that actually fires off the unique Action which the Concurrent fed in is supposed to embody (this is not the Action embedded in its NextStep argument!). Don't worry about the NextStep at all - that's for some other Concurrent to worry about. Just lazily resolve the outer function of one you're dealing with. (Remember that as a lazily evaluated language by default, Haskell is built to do just this. Get function strictness out of your head.)
Ex 1 - 3 General: Someone wants to call convenience methods to build a tree/chain of Concurrents without making the monad directly. What they really want of course are a chain of Actions underneath the monads, but then they wouldn't get the monadic goodness. Give them those methods for Actions wrapped in Concurrent monads.
Ex1: When this method is called, give back a Concurrent that does nothing with its NextStep; just leave it alone to die and supply a Stop to the chain.
Ex2: You are given an argument that is monad of a different type. How do you change that? Revisit lesson 7 if you aren't sure of that basic concept. Note that you given something useful in Ex4 of this lab as I mentioned before. It's pretty valuable here. The Concurrent you make here needs you to think about using the NextStep argument it may be given as part of some chain. Realize that whatever NextStep that the Concurrent that you creating will eventually receive, the caller is expecting you to have designed the Concurrent to execute that side-effecting behavior of that IO monad they passed in, and to pass along the return value on the other side for potentially later use in that NextStep. Again, I reiterate the parallel to bind for the parser.
Ex3: "fork" gives back a Concurrent using a Fork (surprise surprise). One side of the fork is used to kick off the Action of the Concurrent passed in. But remember, the Concurrent you return itself has a NextStep, and so internally it should ensure that it is set up to carry forward any Action that it might have received. "par": Again, you're giving back a Concurrent that sets up a kind of Forking action, but this time you take in 2 Concurrent arguments. The Concurrent you're giving back has responsibility of carrying out the Action and only Action of firing off each argument with the same NextStep argument so that they 2 copies of the chain are made.
Ex4: This is the hardest problem. Again, look at the Parser monad implementation in lesson 7's lab. Note how bind both carries out the side-effecting behavior of the parser passed in, and the function passed in is captured in closure to affect some later point in the chaining of methods. It's easiest here to start with the function part, which here too will be at the "inner" part of the definition. Again, remember that the key to Action chaining is that NextStep can always be abstracted away as a single variable to a higher order function. I do believe that the easiest way to do this is with two lambdas as hinted. In short, you want to produce a new Concurrentthat will transform an Action from the Concurrent you receive before passing along this Action, to the NextStep that would later be given to this new Concurrent you are making.
Ex5: You need to handle all 3 types of Actions in a FIFO queue style. One will "do" something, and put what it gives back as a return value back on the queue. Another will just add its contents to the back of the queue. Another will simply move the queue forward by ignoring the head because there are no further actions to be taken.