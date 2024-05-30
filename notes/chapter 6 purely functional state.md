# 6 Purely functional state
## 6.1 Generating random numbers using side effects
Using side effects for status update make the functions not referentially 
transparent, and hurt testability.

## 6.2 Purely functional random number generation
We can recover referential transparency by returning the new state along with the
generated value. This separates the concern of computing what the next state is from the concern of
communicating the new state to the rest of the program.
```scala worksheet
trait RNG:
  def nextInt: (Int, RNG)
```

## 6.3 Making stateful APIs pure
Having the API compute the next state is a general solution to make stateful APIs
pure. When using this pattern, we make the caller responsible for passing the computed 
next state through the rest of the program.
For example, for generating two distinct numbers:
```scala worksheet
def randomPair(rng: RNG): ((Int, Int), RNG) =
  val (i1, rng2) = rng.nextInt
  val (i2, rng3) = rng2.nextInt
  ((i1, i2), rng3)
```

## 6.4 A better API for state actions
We can notice a common pattern of our implementations: they are in the form of 
`RNG => (A, RNG)`. They are called state actions/state transitions. These state 
actions can be combined to generate new state actions, and we want the states
are passed from one action to the next automatically.  
For simplification purposes we can make a type alias for the RNG state action:
```scala worksheet
type Rand[+A] = RNG => (A, RNG)
```
We'll create a kind of domain-specific language that let us combine `Rand` 
functions, while avoiding explicitly passing alone the `RNG` state. The 
simplest component is the `unit` function, which just pass through the `RNG`
state without using it at all:
```scala worksheet
def unit[A](a: A): Rand[A] =
  rng => (a, rng)
```
Another component is `map`. It transforms the output without further modifying
the state.
```scala worksheet
def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
  rng => 
    val (a, rng2) = s(rng)
    (f(a), rng2)
```
### 6.4.1 Combining state actions
Some other ones in the exercises of this chapter:
```scala worksheet
def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C]

def sequence[A](rsa: List[Rand[A]]): Rnad[List[A]]
```
### 6.4.2 Nesting state actions
`map` is for functions that returns fixed values.
But sometimes we need to work with functions that returns random values 
(functions of type `A => Rand[B]`). It feels like the randomness
in `map` only happens once: we randomly generate value `a` and 
deterministically transform it to a value `b`. But in flatMap, we randomly 
generate a value `a`, and use it to randomly generate another value `b`.

Consider the `nonNegativeLessThan` function, which generates an integer 
between 0 (inclusive) and n (exlcusive). A first implementation might be 
like this:
```scala worksheet
def nonNegativeLessThan(n: Int): Rand[Int] =
  map(nonNegativeInt)(_ % n)
```
This implementation would skew the distribution of the generated random
value, because Int.MaxValue may not be exactly divisible by n. We want
to have a retry when `nonNegativeInt` generates a number higher than
the largest multiple of n. If we are using map we will have a trouble:
```scala worksheet
def nonNegativeLessThan(n: Int): Rand[Int] =
  map(nonNegativeInt): i =>
    val mod = i % n
    // the condition here checks if i is less than
    // the largest multiple of n so as not to cause 
    // (i - mod) + (n - 1) to overflow
    if i - mod + (n - 1) >= 0 then mod  // this is an Int 
    else nonNegativeLessThan(n)         // but this is an Rand[Int]
```
We want to chain things together to pass the `RNG` returned by 
`nonNegativeInt` to the recursive call of `nonNegativeLessThan`
```scala worksheet
def nonNegativeLessThan(n: Int): Rand[Int] =
  rng => 
    val (i, rng2) = nonNegativeInt(rng)
    val mod = i % n
    if i + (n-1) - mod >= 0 then (mod, rng2)
    else nonNegativeLessThan(n)(rng2)
```
This becomes the motivation for us to define a `flatMap`.
```scala worksheet
def flatMap[A, B](r: Rand[A])(f: A => Rand[B]): Rand[B]
  rng => 
    val (a, rng1) = r(rng)
    f(a)(rng1)
```
Then we can use it for a new implementation of `nonNegativeLessThan`:
```scala worksheet
def nonNegativeLessThan(n: Int): Rand[Int] =
  flatMap(nonNegativeInt): i =>
    val mod = i % n
    if i + (n-1) - mod >= 0 then unit(mod) else nonNegativeLessThan(n)
```
## 6.5 A general state action data type
The logic of functions like `map`, `flatMap` etc. can be applied to other 
cases to work with state actions. We can give them more general signatures, 
for example:
```scala worksheet
def map[S, A, B](action: S => (A, S))(f: A => B): S => (B, S)
```
And come up with a more general type than `Rand` for handling states:
```scala worksheet
type State[S. +A] = S => (A, S)
```
## 6.6 Purely functional imperative programming
Imperative and functional programming absolutely are not opposites.

