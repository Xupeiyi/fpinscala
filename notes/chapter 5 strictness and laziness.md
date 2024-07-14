# 5. Strictness and Laziness
## 5.1 Strict and nonstrict functions
To say that a function is nonstrict just means the function may choose not to 
evaluate one or more of its arguments. For example, `&&` and `||` are nonstrict.
`if` is also nonstrict.

We can explicitly write nonstrict function by passing in functions are arguments:
```scala worksheet
def if2[A](cond: Boolean, onTrue: () => A, onFalse: () =>A): A =
  if cond then onTrue() else onFalse()

if2(
  a < 22,
  () => println("a"),
  () => println("b")
)
```
The unevaluated form of an expression is called a thunk.
Scala provides a syntax sugar to wrap expression in thunks:
```scala worksheet
def if2[A](cond: Boolean, onTrue: => A, onFalse: => A): A =
  if cond then onTrue else onFalse

// if2(false, sys.error("fail"), 3) will return 3. 
```
An argument that's passed unevaluated to a function will be evaluated once
for each place it's referenced in the body of the function. Use the `lazy` keyword
to cache it explicitly.


## 5.3 Separating program description from evaluation
Laziness let us separate the description of an expression from the evaluation of it.
```scala worksheet
def foldRight[B](acc: => B)(f: (A, => B) => B): B =
  this match
    case Cons(h, t) => f(h(), t().foldRight(acc)(f))
    case _ => acc
```

## 5.4 Infinite lazy lists and corecursion
```scala worksheet
val ones: LazyList[Int] = LazyList.cons(1, ones)
```
It turns out that many lazylist methods can be generalized with:
```scala worksheet
def unfold[A, S](state: S)(f: S => Option[(A, S)]): LazyList[A] =
  f(state) match
    case Some((result, new_state)) => cons(result, unfold(new_state)(f))
    case None => empty
```
For example, we can generate an infinite lazy list starting from n:
```scala worksheet
def from(n: Int): LazyList[Int] =
  unfold(n)(n => Some((n, n + 1)))
```
We can also implement `map` with `unfold`, using the lazy list itself as the
state:
```scala worksheet
def map[B](f: A => B): LazyList[B] =
  unfold(this):
    case Cons(h, t) => Some((f(h(), t())))
    case Empty => None
```