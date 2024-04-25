# 7. Purely Functional Parallelism
## 7.1 Choosing Data Types and Functions

The goal is to create parallel computations.

For example, when using the divide-and-conquer method for summing integers,
```scala worksheet
def sum(ints: IndexedSeq[Int]): Int =          
  if ints.size <= 1 then
    ints.headOption.getOrElse(0)               
  else
    val (l, r) = ints.splitAt(ints.size / 2)   
    sum(l) + sum(r)
```
we want to run the two halves in parallel.

Design tips:
1. start with a simple example, and then add complexity gradually
2. design ideal APIs first, and then work for an implementation


### 7.1.1 A data type for parallel computations
The requirements of the data type to represent parallel computations: 
- must be able to contain a result
- the result has a meaningful type like `Int`
- provide some way to extract this result

For now, we can invent a container type for the result:
```scala worksheet
object Par[A]:
  
  def unit[A](a :=> A): Par[A] = ???
    // takes an unevaluated A and return a parallel computation of A
    // here "unit" means "a unit of parallelism"
  
  def get[A](a :Par[A]): A = ???
    // extract the resulting value from a parallel computation

```

We can use this new data type to solve the integer summing problem:
```scala worksheet
def sum(ints: IndexedSeq[Int]): Int =
  if ints.size <= 1 then
    ints.headOption.getOrElse(0)
  else
    val (l, r) = ints.splitAt(ints.size / 2)
    val sumL: Par[Int] = Par.unit(sum(l))
    val sumR: Par[Int] = Par.unit(sum(r))
    Par.get(sumL) + Par.get(sumR)
```

We are not directly using concurrency primitives like Java.lang.Thread 
and Runnable because they do not return a meaningful value, which hurts
compositionality. Besides, Thread maps directly to actual OS threads, not 
logical threads.

> Choose - the meaning of `unit` and `get`
> 1. `unit` starts evaluation immediately in a separate thread
> 2. `unit` starts evaluation after `get` is called.  

Answer: Choose option 1 

Reason: We cannot choose option 2. This is because function arguments in Scala are 
strictly evaluated from left to right. If we choose option 2, then we will
spawn the parallel computation, wait for it to finish, then spawn the second 
parallel computation. This means the computation is effectively sequential.

But choosing option 1 breaks referential transparency. For
```
Par.get(sumL) + Par.get(sumR)
```
replacing `sumL` and `sumR` with `Par.unit(sum(l))` and `Par.unit(sum(r))` makes the 
computation no longer parallel. We can see `unit` has a side effect but only 
with regard to `get`. So we should avoid calling `get` or at least wait till 
the end.

### 7.1.2 Combining parallel computations
If we do not call `get`, then the `sum` function returns a `Par[Int]`. Rewrite 
the `sum` function to this:
```scala worksheet
def sum(ints: IndexedSeq[Int]): Par[Int] =
  if ints.size <= 1 then
    Par.unit(ints.headOption.getOrElse(0))
  else 
    val (l, r) = ints.splitAt(ints.size / 2)
    Par.map2(sum(l), sum(r))(_ + _)
```
The signature of Par.map2:
```scala worksheet
def map2[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C]
```
We are no longer calling `unit` in the recursive case. Currently, it becomes unclear 
whether `unit` should accept its argument lazily.
> Choose - the laziness of `map2`
> 1. `map2` takes its arguments lazily
> 2. `map2` takes its arguments strictly

Answer: Choose option 1.

Reason: We want `pa` and `pb` to run in parallel. Which choice let us implement this 
meaning? If `map2` is strict, we must execute the left half of the computation before 
constructing the right half (since Scala evaluates arguments from left to right). 
If we don't have `map2` begin execution immediately, we may end up with very heavy 
objects describing the computation. Therefore, we should make `map2` lazy
and have it immediately begin execution of both sides in parallel.

### 7.1.3 Explicit forking
A problem with our latest choice: We may not always want to evaluate the arguments of 
`map2` in parallel. The current API doesn't give user control on when computations get 
forked off the main thread. Invent a `fork` function
```scala worksheet
def fork[A](a: => Par[A]): Par[A]
```
for explicit forking. Use it to rewrite the `sum` function
```scala worksheet
def sum(ints: IndexedSeq[Int]): Par[Int] =
  if ints.size <= 1 then
    Par.unit(ints.headOption.getOrElse(0))
  else
    val (l, r) = ints.splitAt(ints.size / 2)
    Par.map2(Par.fork(sum(l)), Par.fork(sum(r)))(_ + _)
```
With `fork` we can now make `map2` strict. Here `fork` addresses two concerns:
1. how to indicate the combination of two tasks' results
2. should a task be run asynchronously

By keeping these concerns separate, we avoid having a global policy for parallelism of
`map2` and other operations.

And with `fork` we can also make `unit` strict. We can have strict and non-strict `unit`
using `fork`:
```scala worksheet
def unit[A](a: A): Par[A]

def lazyUnit[A](a: => A): Par[A] = fork(unit(a))
```
Here `lazyUnit` is a *derived combinator* as opposed to a *primitive combinator*. 
It won't care about the implementation of `Par` as long as it exposes `fork` and 
`unit`.

Should fork starts the evaluation immediately, or when the computation is forced 
later? In other words,
> Choose - the responsibility of evaluation
> 1. it belongs to `fork`
> 2. it belongs to `get`
 

Answer: Choose option 2.

Reason: Think about the required information to implement `fork` and `get`. 
If `fork` starts the evaluation immediately, it must know things about threads 
or thread pools. This means the resource for parallelism (i.e. the thread pool) 
must be accessible and initialized wherever fork is called. To have more 
fine-grained control, we give this responsibility to `get`.

With this design, `Par` becomes a description of a parallel computation that
can be run later, rather than a container of a value which can be got
later.

We rename `get` to `run`, and it needs some means to implement parallelism.
```scala worksheet
extension [A](pa: Par[A]) def run: A
```

## 7.2 Picking a representation
Make use of the Java Standard Library: java.util.concurrent.ExecutorService. 
We can submit a Callable to ExecutorService and obtain a Future.

For `run` we want it to return a `Future[A]` rather than `A`, so that the caller
of run can decide things like how long to wait for a computation, or whether
to cancel it.

```scala worksheet
opaque type Par[A] = ExecutorService => Future[A]
extension [A](pa: Par[A]) def run(s: ExecutorService): Future[A] = pa(s)
```

### 7.2.1 Refining the API
In reality, there's no clear boundaries between designing the API and choosing a 
representation.

The first version of implementation:
```scala worksheet
object Par:
  def unit[A](a: A): Par[A] = es => UnitFuture(a)
  
  private case class UnitFuture[A](get: A) extends Future[A]:
    def isDone = true
    def get(timeout:Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false

  extension [A](pa: Par[A])
    def map2[B, C](pb: Par[B])(f: (A, B) => C): Par[C] =
      (es: ExecutorService) =>
        val futureA = a(es)
        val futureB = b(es)
        UnitFuture(f(futuerA.get, futuerB.get))
        
  def fork[A](a :=> Par[A]): Par[A] = 
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })
```

`Future` does not have a purely functional interface, but the `Par` API remains pure.

We can define an `asyncF` function to convert a function to an asynchronous one:
```scala worksheet
def asyncF[A, B](f: A=> B): A => Par[B] = 
  a => lazyUnit(f(a))
```

What else can the existing combinators represent? The author gives another example 
on sorting the list resulted from a `Par`. The function signature is 
```scala worksheet
def sortPar(parList: Par[List[Int]]): Par[List[Int]]
```
A simple idea is to run the `Par`, sort the list, and wrap it in another `Par`. But 
we can use `map2` to avoid calling `run`.
```scala worksheet
def sortPar(parList: Par[List[Int]]): Par[List[Int]] =
  parList.map2(unit(()))((a, _) => a.sorted)
```
In general, we can lift any function of type `A => B` to `Par[A] => Par[B]` with this 
`map` function:
```scala worksheet
extension [A](pa: Par[A]) def map[B](f: A => B): Par[B] =
  pa.map2(unit(()))((a, _) => f(a))
```
and rewrite `sortPar`:
```scala worksheet
def sortPar(parList: Par[List[Int]]) =
  parList.map(_.sorted)
```
It's ok to pass a bogus value `unit(())` to `map2` to implement `map`, and this shows
that `map2` is more powerful.
We can further define a `parMap` function to `map` over a list in parallel, which is 
even more generic than `map2`. A naive solution:
```scala worksheet
def sequence[A](pas: List[Par[A]]): Par[List[A]] =
  pas.foldRight(unit(List.empty[A]))((pa, acc) => pa.map2(acc)(_::_))
  
def parMap[A, B](ps: List[A])(f: A=> B): Par[List[B]] = fork:
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
```

It's also possible to filter the elements in parallel using the existing methods:
```scala worksheet
def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = fork:
    val pars: List[Par[List[A]]] = l.map(asyncF(a => if f(a) then List(a) else List()))
    sequence(pars).map(_.flatten) 
```

## 7.3 The algebra of an API
Treat the API as an algebra or an abstract set of operations, along with a set of 
laws or properties assumed to be true.

A law places constraints on operations' meanings, determines the feasibility of
implementations, and affects what properties can be true.
### 7.3.1 The law of mapping
We can start making a law of identity (equivalence). For example,
```scala worksheet
unit(1).map(_ + 1) == unit(2)

// or in general,
unit(x).map(f) == unit(f(x))
```
This leads to the question of what "equivalent" means here. Currently, we see two 
`Par` objects are equivalent if for any valid ExecutorService argument, their 
`Future` results have the same value. This means `map` and `unit` cannot use
downcasting or `isInstanceOf` checks, otherwise `f` may receive a different result.

Since this law holds for any `x` and `f`, we have this special case:
```scala worksheet
y.map(id) == y
```

This is even better since the new and simpler law is only about `map`. We now can
see `map` cannot throw an exception before applying the function to the result. It 
can only apply `f` to `y`.

### 7.3.2 The law of forking & 7.3.3 Breaking the law: A subtle bug
It may look obvious that `fork` shouldn't affect the result of the computation:
```scala worksheet
fork(x) == x
```
This should hold true for any choice of `x` and any choice of `ExecutorService`.
But we have a counterexample:
```scala worksheet
val a = lazyUnit(42 + 1)
val es = Executors.newFixedThreadPool(1)
println(Par.eq(es)(a, fork(a)))
```
This will result in deadlocking. Because we implement `fork` as 
```scala worksheet
def fork[A](a :=> Par[A]): Par[A] = 
  es => es.submit(new Callable[A] { def call = a(es).get })
```
We are submitting the `Callable` first, and within it, we're submitting another 
`Callable` and blocking on its result. Since there's only one thread in the 
thread pool we are having a deadlock.  

We can try to fix `fork` with a different implmentation
```scala worksheet
def fork[A](fa: => Par[A]): Par[A] =
  es => fa(es)
```
but it actually isn't creating a separate logical thread. It's still a useful
combinator. We can call it `delay` since it delays the instantiation of a 
computation.

### 7.3.4 A fully non-blocking Par implementation using actors
(Skipping this section temporarily)

## 7.4 Refining combinators to their most general form
Suppose we want a function to choose between two forking computations based on
the result of an initial computation:
```scala worksheet
def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A]
  es =>
    if cond.run(es).get then t(es)
    else f(es)
```