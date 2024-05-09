# 8.  Property-based testing
## 8.1 A brief tour of property-based testing
This is an example using ScalaCheck, a property-based testing library.

We can define a *property* by combining a generator with some predicates. 
```scala worksheet
import org.scalacheck.{Gen, Prop}

val intList: Gen[List[Int]] = Gen.listOf(Gen.choose(0, 100))
val prop = (
  Prop.forAll(intList)(ns => ns.reverse.reverse == ns) 
  && Prop.forAll(intList)(ns => headOption == ns.reverse.lastOption)
)
val failingProp = Prop.forAll(intList)(ns => ns.reverse == ns)
```
Then check the properties as 
>scala> prop.check  
>scala> failingProp.check

they can either pass or fail.

Other ideal features for property-based testing libraries:
- Testcase minimization - find the smallest test case that fails
- Exhaustive test case generation - test all values in a domain

### 8.1.1 Choosing data types and functions
This will be a messy and iterative process of discovery.

### 8.1.2 Initial snippets of an API
We can start with `Gen`.Either `Gen.choose` or `Gen.listOf` should be parametric 
in some type. In other words, they shouldn't care about the type of their inputs.
```scala worksheet
def listOf[A](a: Gen[A]): Gen[List[A]]
```

By examining this signature, we noticed that we're not specifying the size of the 
list to generate. The generator need to either assume it, or be informed about it.
It makes more sense to inform the generator about it, since having the generator
to assume it is too inflexible.

One way to inform the generator about the size is to pass it to the function as an
argument:
```scala worksheet
def listOfN[A](n: Int, a: Gen[A]): Gen[List[A]]
```

Another way is to wait for the function that runs the test to specify it. This might
be helpful to the testcase minimization feature.
