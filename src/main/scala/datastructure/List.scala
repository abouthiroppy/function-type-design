package datastructure

/**
  * Created by about_hiroppy on 2016/03/12.
  */
sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def tail[A](as: List[A]): List[A] = {
    as match {
      case Nil => Nil
      case Cons(x, xs) => xs
    }
  }

  def setHead[A](as: List[A], item: A): List[A] = {
    as match {
      case Nil => Nil
      case Cons(x, xs) => Cons(item, xs)
    }
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    (l, n) match {
      case (Nil, _) => Nil
      case (_, n) if n == 0 => l
      case _ => drop(tail(l), n - 1)
    }
  }

  def dropwhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Nil => Nil
      case Cons(x, xs) => if (f(x)) dropwhile(xs, f) else l
    }
  }
}


