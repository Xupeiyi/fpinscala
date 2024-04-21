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

> Design tips:
>1. start with a simple example, and then add complexity gradually
>2. design ideal APIs first, and then work for an implementation


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

Now we must choose the meaning of `unit` and `get`:
1. `unit` starts evaluation immediately in a separate thread
2. `unit` starts evaluation after `get` is called.

We cannot choose option 2. This is because function arguments in Scala are 
strictly evaluated from left to right. If we choose option 2, then we will
spawn the parallel computation, wait for it to finish, then spawn the second 
parallel computation. This means the computation is effectively sequential.

But choosing option 1 breaks referential transparency. For
```
Par.get(sumL) + Par.get(sumR)
```
replacing sumL and sumR with Par.unit(sum(l)) and Par.unit(sum(r)) makes the 
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

Should `map2` take its arguments lazily? We want `pa` and `pb` to run in parallel. Which
choice let us implement this meaning? If `map2` is strict, we must execute the left half 
of the computation before constructing the right half (since Scala evaluates arguments 
from left to right). If we don't have `map2` begin execution immediately, we may end up 
with very heavy objects describing the computation. Therefore, we should make `map2` lazy
and have it immediately begin execution of both sides in parallel.

### 7.1.3 Explicit forking
A problem with our lastest choice: We may not always want to evaluate the arguments of 
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

 

