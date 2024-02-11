package fpinscala.exercises.datastructures

import scala.annotation.tailrec

/** `List` data type, parameterized on a type, `A`. */
enum List[+A]:
  /** A `List` data constructor representing the empty list. */
  case Nil
  /** Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
    which may be `Nil` or another `Cons`.
   */
  case Cons(head: A, tail: List[A])

object List: // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.

  def product(doubles: List[Double]): Double = doubles match
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail*))

  @annotation.nowarn // Scala gives a hint here via a warning, so let's disable that
  val result = List(1,2,3,4,5) match
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))

  def foldRight[A,B](as: List[A], acc: B, f: (A, B) => B): B = // Utility functions
    as match
      case Nil => acc
      case Cons(x, xs) => f(x, foldRight(xs, acc, f))

  def sumViaFoldRight(ns: List[Int]): Int =
    foldRight(ns, 0, (x,y) => x + y)

  def productViaFoldRight(ns: List[Double]): Double =
    foldRight(ns, 1.0, _ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def tail[A](l: List[A]): List[A] =
    l match
      case Nil => sys.error("tail of empty list")
      case Cons(_, xs) => xs

  def setHead[A](l: List[A], h: A): List[A] =
    l match
      case Nil => sys.error("empty list")
      case Cons(x, xs) => Cons(h, xs)

  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] =
    if n <= 0 then l
    else l match
      case Nil => l
      case Cons(_, xs) => drop(xs, n - 1)

  @annotation.tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match
      case Nil => l
      case Cons(x, xs) => if f(x) then dropWhile(xs, f) else l


  def init[A](l: List[A]): List[A] =
    l match
      case Nil => sys.error("empty list")
      case Cons(_, Nil) => Nil
      case Cons(x, xs) => Cons(x, init(xs))


  def length[A](l: List[A]): Int =
    foldRight(l, 0, (_, acc) => 1 + acc)

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], acc: B, f: (B, A) => B): B =
    l match
      case Nil => acc
      case Cons(x, xs) => foldLeft(xs, f(acc, x), f)

  def sumViaFoldLeft(ns: List[Int]): Int =
    foldLeft(ns, 0, (x, y) => x + y)

  def productViaFoldLeft(ns: List[Double]): Double =
    foldLeft(ns, 1.0, (x, y) => x * y)

  def lengthViaFoldLeft[A](l: List[A]): Int =
    foldLeft(l, 0, (acc, _) => acc + 1)

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, List[A](), (acc, h) => Cons(h, acc))

  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] =
    foldRight(l, r, (h, acc) => Cons(h, acc))

  def concat[A](l: List[List[A]]): List[A] =
    foldLeft(l, List[A](), (acc, h) => appendViaFoldRight(acc, h))

  def incrementEach(l: List[Int]): List[Int] =
    l match
      case Nil => Nil
      case Cons(h, t) => Cons(h + 1, incrementEach(t))

  def doubleToString(l: List[Double]): List[String] =
    l match
      case Nil => Nil
      case Cons(h, t) => Cons(h.toString, doubleToString(t))

  def map[A,B](l: List[A], f: A => B): List[B] =
    l match
      case Nil => Nil
      case Cons(h, t) => Cons(f(h), map(t, f))

  def filter[A](as: List[A], f: A => Boolean): List[A] =
    as match
      case Nil => Nil
      case Cons(h, t) => if f(h) then Cons(h, filter(t, f)) else filter(t, f)

  def flatMap[A,B](as: List[A], f: A => List[B]): List[B] =
    as match
      case Nil => Nil
      case Cons(_, _) => concat(map(as, f))

  def filterViaFlatMap[A](as: List[A], f: A => Boolean): List[A] =
    flatMap(as, x => if f(x) then List[A](x) else List[A]())

  def addPairwise(a: List[Int], b: List[Int]): List[Int] =
    (a, b) match
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(ha, ta), Cons(hb, tb)) => Cons(ha + hb, addPairwise(ta, tb))

  def zipWith[A, B, C](a: List[A], b: List[B], f: (A, B) => C): List[C] =
    (a, b) match
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(ha, ta), Cons(hb, tb)) => Cons(f(ha, hb), zipWith(ta, tb, f))

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = ???
