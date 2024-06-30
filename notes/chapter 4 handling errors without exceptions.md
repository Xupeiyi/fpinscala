# 4. Handling errors without exceptions

## 4.1 The good and bad aspects of exceptions
Exception breaks referential transparency. It's context dependent
(means different things in different try blocks (or not in a try
block at all)). That's why it's often suggested that exceptions should
only be used for error handling, not for control flow.

Exceptions are not type safe. The function signature won't tell us
about the exceptions that may occur.

## 4.2 Possible alternatives to exceptions
Consider a function that calculates the mean of a `Seq[Double]`. It's clearly erroneous when 
the sequence is empty, thus causing a "division by zero" error. We call it a partial function
because it's not defined for some inputs. Instead of using exception for error handling, we 
have some other options. 
1. Return a sentinel value like `Double.NaN`. It's not ideal because the error can silently 
propagate; It results in lots of boilerplate code. Sometimes we may not even have a sentinel 
value.
2. Forcing the caller to supply an argument that deals with the error. But it requires the
caller to have direct knowledge on handling the undefined cases. Besides the type of the 
argument is limited.

## 4.3 The Option data type
The solution is explicitly express that a function may not always have an answer in the 
return type.
```scala worksheet
enum Option[+A]:
  case Some(get: A)
  case None

def mean(xs: Seq[Double]): Option[Double] =
  if xs.isEmpty then None
  else Some(xs.sum / xs.length)
```

### 4.3.1 Usage patterns for Option
We can implement these APIs for `Option`.
```scala worksheet
enum Option[+A]:
  case Some(get: A)
  case None
  
  def map[B](f: A => B): Option[B]
  def flatMap[B](F: A => Option[B]): Option[B]
  def getOrElse[B >: A](default: => B): B
  def orElse[B >: A](ob: => Option[B]): Option[B]
  def filter(f: A => Boolean): Option[A]
```
We can chain together possibly failing computations without having to check for 
failure at each step.

### 4.3.2 Option composition, lifting and wrapping exception-oriented APIs
We can implement a `lift` function to enable any ordinary function to operate on an `Option`,
without having to modify the original function.

```scala worksheet
def lift[A, B](f: A => B): Option[A] => Option[B] =
  _.map(f)
```

We then implemented a `map2` function to lift an ordinary function that takes in 2 parameters.
```scala worksheet
def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
  a.flatMap(aa => b.map(bb => f(aa, bb)))

// Rewrite `map2` using for-comprehension, a syntax sugar provided by Scala:
def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
  for 
    aa <- a
    bb <- b
  yield f(aa, bb)
```

We also implemented a `traverse` function to combine a list of `Option`s generated from
`f` to one `Option` containing a list of all the `Some` values.
```scala worksheet
def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
  as.foldRight[Option[List[B]]](Some(Nil))((x, acc) => map2(f(x), acc)(_::_))
```

## 4.4 The Either data type
`Option` only gives us `None` whenever something failed. We can use the `Either` data 
type if we want to track the reason of the failure, and like `Option`, implement 
a suite of useful APIs.

```scala worksheet
enum Either[+E, +A]:
  case Left(value: E)
  case Right(value: A)
  
  def map[B](f: A => B): Either[E, B]
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B]
  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B]
  def map2[EE >: E, B, C](that: Either[EE, B])(f: (A, B) => C): Either[EE, C]
```
Like for `Option`, we implemented `sequence` and `traverse` for `Either`.

### 4.4.1 Accumulating errors
Our current implementation of `map2` only report one error when both validation fail.
```scala worksheet
enum Either[+E, +A]:
  
  def map2[EE >: E, B, C](that: Either[EE, B])(f: (A, B) => C): Either[EE, C]:
    for 
      aa <- this
      bb <- that
    yield f(aa, bb)
```
To present both errors we can change the implementation to this:
```scala worksheet
def map2Both[E, A, B, C](
    a: Either[E, A], 
    b: Either[E, B], 
    f: (A, B) => C
): Either[List[E], C] =
  (a, b) match
    case (Right(aa), Right(bb)) => Right(f(aa, bb))
    case (Left(e), Right(_)) => Left(List(e))
    case (Right(_), Left(e)) => Left(List(e))
    case (Left(e1), Left(e2)) => Left(List(e1, e2))
```
But this will cause nested list of errors when map2Both is nested. We can solve this by
further requiring the input values to already have a List[E] on the left side:
```scala worksheet
def map2All[E, A, B, C](
    a: Either[List[E], A],
    b: Either[List[E], B],
    f: (A, B) => C
): Either[List[E], C] =
    (a, b) match
    case (Right(aa), Right(bb)) => Right(f(aa, bb))
    case (Left(es), Right(_)) => Left(es)
    case (Right(_), Left(es)) => Left(es)
    case (Left(es1), Left(es2)) => Left(es1 ++ es2)
```
### 4.4.2 Extracting a Validated type
We can define another type specifically for this error accumulation behavior:
```scala worksheet
enum Validated[+E, +A]:
  case Valid(get: A)
  case Invalid(error: E)
  
  def map2[EE >: E, B, C](
    b: Validated[EE, B],
    f: (A, B) => C,
    combineErrors: (EE, EE) => EE
  ): Validated[EE, C] =
    (this, b) match
      case (Valid(aa), Valid(bb)) => Valid(f(aa, bb))
      case (Invalid(e), Valid(_)) => Invalid(e)
      case (Valid(_), Invalid(e)) => Invalid(e)
      case (Invalid(e1), Invalid(e2)) => Invalid(combineErrors(e1, e2))
```
Instead of defining `Invalid(error: List[E]]`, we do not assume the type of the error,
and explicitly pass in a function `combineErrors` for error accumulation.