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
pure. When using this patter, we make the caller responsible for passing the computed 
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

