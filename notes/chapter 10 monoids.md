# 10 Monoids
## 10.1 What is a monoid?
A monoid is a type together with the monoid operations and a set of laws. 
It is an algebra.  
A monoid consists of:
- Some type `A`
- an associative binary operation `combine`
- a value `empty: A`

It can be expressed in the form of trait:
```scala worksheet
trait Monoid[A]:
  def combine(a1: A, a2: A): A
  def empty: A
```
for example, the `String` monoid:
```scala worksheet
val stringMonoid: Monoid[String] = new:
  def combine(a1: String, a2: String) = a1 + a2
  val empty = ""
```

## 10.2 Folding lists with monoids
The components of a monoid can fit the arguments of the fold methods of lists very well.
```scala worksheet
def combineAll[A](as: List[A], m: Monoid[A]): A =
  as.foldLeft(m.empty)(m.combine)
```
It wouldn't change the result if we are using `foldRight` because of the laws of
associativity and identity.

## 10.3 Associativity and parallelism
The associativity nature of `combine` gives us freedom to choose how we fold a list.
Instead of 
```scala worksheet
combine(a, combine(b, combine(c, d)))
```
we can do
```scala worksheet
combine(combine(a, b), combine(c, d))
```

```scala worksheet
def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A=> B): B =
  if as.isEmpty then
    m.empty
  if as.length == 1 then
    f(as(0))
  else
    val (l, r) = as.splitAt(as.length / 2)
    m.combine(foldMapV(l, m)(f), foldMapV(r, m)(f))
```

## 10.4 Example: Parallel parsing
Suppose we want to count the number of words in a very long string (perhaps too big
for a single machine's memory). Instead of doing it sequentially, we want to break
it into pieces and combine the results. We would want the combination method to be
associative, so that we won't need to care if we are looking at the beginning, 
middle or the end of the string.

We can an algebraic data structure to represent the partial result of the word count:
```scala worksheet
enum WC:
  case Stub(chars: String)  // a part of the string that belongs to a single complete word
  case Part(lStub: String, 
            words: Int,  // the number of complete words we've seen
            rStub: String)
```
A monoid instance for WC would be:
```scala worksheet
val wcMonoid: Monoid[WC] = new Monoid[WC]:
  // for 
  val empty = WC.Stub("")
  
  def combine(wc1: WC, wc2: WC) = (wc1, wc2) match
    // 1 . if wc1 and wc2 are both stubs, we can't say their combination would be
    // Part("", 1, "") because we aren't sure if a + b will make a full word
    // it might be the middle part of a super long word
    case (WC.Stub(a), WC.Stub(b)) => WC.Stub(a + b)  
    
    // 2. extend the lStub/rStub of the Part if the other WC is a stubß
    case (WC.Stub(a), WC.Part(l, w, r)) => WC.Part(a + l, w, r)
    case (WC.Part(l, w, r), WC.Stub(b)) => WC.Part(l, w, r + b)
    
    // 3. preserve the leftmost and rightmost stubs
    //    concatenate the stubs in the middle 
    //    and update the word counts accordingly
    case (WC.Part(l1, w1, r1), WC.Part(l2, w2, r2)) => WC.Part(
      l1,
      // if r1 and l2 are both "", they won't make an additional full word
      // otherwise they will
      w1 + w2 + (if (r1 + l2).isEmpty then 0 else 1),
      r2
    )
```
We can't define `empty` as `Part("", 0, "")` under the current law of combine,
because combining a `Stub` with `empty` will result in a `Part`, not the `Stub`
itself.

Then we can convert every character in the string to a `WC` and combine them.
```scala worksheet
def count(s: String): Int =
  def wc(c: Char): WC =
    if c.isWhitespace then
      WC.Part("", 0, "")
    else
      WC.Stub(c.toString)
  
  def unstub(s: String) = if s.isEmpty then 0 else 1
    
  foldMapV(s.toIndexedSeq, wcMonoid)(wc) match
    case WC.Stub(x) => unstub(x)
    case WC.Part(l, count, r) => unstub(l) + count + unstub(r)
```

A monoid homomorphism `f` between monoids `M` and `N` obeys the following 
general law for all values `x` and `y`:
```scala worksheet
M.combine(f(x), f(y)) == f(N.combine(x, y))
```
A monoid isomorphism between `M` and `N` has two homomorphisms `f` and `g`, where
both `f andThen g` and `g andThen f` are an identity function.

## 10.5 Typeclasses
The current `foldMap` takes monoids as arguments:
```scala worksheet
def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
  as.foldRight(m.empty)((a, acc) => m.combine(f(a), acc))
```
Introduction to context parameters and the keyword `using`.  
Use the `given` keyword to indicate that an instance should be passed 
as a context parameter by scala.  
Introduction to the typeclass pattern - define an interface independent of
a type, then adapt that type to the interface. `Monoid` can be a typeclass.

## 10.6 Foldable data structures

