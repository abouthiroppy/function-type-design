package gettingstarted

/**
  * Created by about_hiroppy on 2016/03/12.
  */
object MyModule {
  def fib(n: Int): Int = {
    def loop(n: Int, first: Int, second: Int): Int = {
      if (n <= 1) first
      else loop(n - 1, second, first + second)
    }
    loop(n, 0, 1)
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    if (as.tail.length == 0) true
    else if (!ordered(as.head, as.tail.head)) false
    else isSorted(as.tail, ordered)
  }

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    (a: A) => ((b: B) => (f(a, b)))
  }

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
    // val hoge: B => C = f(a)
    // hoge(b)
    // f(a)(b)
    // かりー化されたやつは型推論が働きやすい
  }

  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    (a: A) => f(g(a))
  }
}