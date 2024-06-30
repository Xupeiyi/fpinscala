# 3. Functional Data Structures
## 3.1 Defining functional data structures
```scala worksheet
enum List[+A]:
  case Nil
  case Cons(head: A, tail: List[A])
  
object List:
  def apply[A](as: A*): List[A] =
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail*))
```
The author then explained the scala syntax involved in this piece of code. For
example, `+A` means `A` is a covariant parameter - for example, if `Dog` is a 
subtype of `Animal`, then `List[Dog]` is a subtype of `List[Animal]`.

## 3.2 Pattern Matching
The author used pattern matching to implement `sum` and `product`. A pattern may contain
literals or data constructors.

## 3.3 Data Sharing in functional data structures
Sharing immutable data makes the code more efficient and spares us from copying before 
modification to prevent from data corruption.
We implemented tail and setHead as examples.

### 3.3.1 The efficiency of data sharing
We implemented more functions that take advantage of data sharing like
drop(as, n), dropWhile(as, f) and init.

### 3.3.2 Recursion over lists and generalizing to higher-order functions
We extracted the general pattern `foldRight` from `product` and `sum`, in which
`right` means that the collapsing begins at the right and goes back to the start
of the list.
```scala worksheet
def foldRight[A, B](as: List[A], acc: B, f: (A, B) => B): B =
  as match
    case Nil => acc
    case Cons(x, xs) => f(x, foldRight(xs, acc, f))
```
We will tackle with early termination with foldRight at chapter 5.
We can also implement a `foldLeft` and use it implement `product` and `sum`.
In the exercise we implemented some other helper functions with `foldRight/Left`.

### 3.3.3 More functions for working with lists
```scala worksheet
def map[A, B](l: List[A], f: A=> B): List[B] =
  foldRight(l, Nil: List[B], (h, t) => Cons(f(h), t))
  
def flatMap[A, B](as: List[A], f: A => List[B]): List[B] =
  foldRight(as, Nil: List[B], (a, acc) => append(f(a), acc))
```

### 3.3.4 Loss of efficiency when assembling list functions from simpler components
Implemented hasSequence.

# 3.4 Trees
The author introduced algebraic data type.
Introduced extension methods.
Implemented some (extension) methods, including map.
flatMap is for nested operations - generate a List, and then use it to generate 
another List.