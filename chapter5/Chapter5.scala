
object Chapter5 extends App {

  import Stream._

  trait Stream[+A] {

    def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

    def exists(p: A => Boolean): Boolean =
      foldRight(false)((a, b) => p(a) || b)

    @annotation.tailrec
    final def find(f: A => Boolean): Option[A] = this match {
      case Empty => None
      case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
    }

    // 5.1
    def toList: List[A] = this match {
      case Cons(h, tail) => h() :: tail().toList
      case _ => List()
    }

    // 5.2
    def take(n: Int): Stream[A] = this match {
      case Cons(h, tail) if n > 1 => cons(h(), tail().take(n - 1))
      case Cons(h, _) if n == 1 => cons(h(), empty)
      case _ => empty
    }

    // 5.2
    def drop(n: Int): Stream[A] = this match {
      case Cons(_, tail) if n > 0 => tail().drop(n - 1)
      case _ => this
    }

    // 5.3
    def takeWhile(p: A => Boolean): Stream[A] = this match {
      case Cons(h, tail) if (p(h())) => cons(h(), tail().takeWhile(p))
      case _ => empty
    }

    // 5.4
    def forAll(p: A => Boolean): Boolean =
      foldRight(true)((x, y) => p(x) && y)

    // 5.5
    def takeWhileFoldRight(p: A => Boolean): Stream[A] =
      foldRight(empty[A])((x, y) => if (p(x)) cons(x, y) else empty)

    // 5.6
    def headOption: Option[A] =
      foldRight(None: Option[A])((x, _) => Some(x))

    // 5.7
    def map[B](f: A => B): Stream[B] =
      foldRight(empty[B])((x, y) => cons(f(x), y))

    def filter(p: A => Boolean): Stream[A] =
      foldRight(empty[A])((x, y) => if (p(x)) cons(x, y) else y)

    def append[AA >: A](s: Stream[AA]): Stream[AA] =
      foldRight(s)((x, y) => cons(x, y))

    def flatMap[B](f: A => Stream[B]): Stream[B] =
      foldRight(empty[B])((x, y) => f(x).append(y))

    // 5.8
    def constant[A](v: A): Stream[A] = cons(v, constant(v));


    // 5.13
    def map1[B](f: A => B): Stream[B] =
      unfold(this)((x) => x match {
        case Cons(v, s) => Some((f(v()), s()))
        case _ => None
      })

    def take1(n: Int): Stream[A] =
      unfold((n, this))({
        case (n, Cons(h, t)) if (n > 1) => Some((h(), (n - 1, t())))
        case (n, Cons(h, _)) if (n == 1) => Some((h(), (n - 1, empty)))
        case _ => None
      })

    def takeWhile1(p: A => Boolean): Stream[A] =
      unfold(this)({
        case Cons(h, t) if (p(h())) => Some((h(), t()))
        case _ => None
      })

    def zipWith[B, C](s: Stream[B])(f: (A, B) => C): Stream[C] =
      unfold((this, s))({
        case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
        case (_, _) => None
      })

    def zipAll[B](s: Stream[B]): Stream[(Option[A], Option[B])] =
      unfold((this, s))({
        case (Cons(h1, t1), Cons(h2, t2)) => Some(((Some(h1()), Some(h2())), (t1(), t2())))
        case (Cons(h1, t1), _) => Some(((Some(h1()), None), (t1(), empty[B])))
        case (_, Cons(h2, t2)) => Some((None, (Some(h2()))), (empty[A], t2()))
        case (_, _) => None
      })

    // 5.14
    def startsWith[A](s: Stream[A]): Boolean =
      unfold((this, s))({
        case (Cons(h1, t1), Cons(h2, t2)) => Some(if (h1() == h2()) 1 else 0, (t1(), t2()))
        case (Cons(_, t1), _) => Some((2, (t1(), empty)))
        case (_, Cons(_, t2)) => Some((3, (empty, t2())))
      }).takeWhile(_ < 2).forAll(_ == 1)

    // 5.15
    def tails: Stream[Stream[A]] =
      cons(this, unfold(this)({
        case Cons(_, t) => Some((t(), t()))
        case _ => None
      }))

    // 5.16
    def scanRight[B](z: => B)(f: (A, => B) => B) =

  }

  case object Empty extends Stream[Nothing]

  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) empty
      else cons(as.head, apply(as.tail: _*))

    val ones: Stream[Int] = Stream.cons(1, ones)

    // 5.9
    def from(n: Int): Stream[Int] = cons(n, from(n + 1))

    // 5.10
    def fibs(): Stream[Int] = {
      def f(m: Int, n: Int): Stream[Int] = cons(n, f(n, m + n))

      f(0, 1)
    }

    // 5.11
    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
      case Some((v, x)) => cons(v, unfold(x)(f))
      case None => empty
    }

    // 5.12
    def fibs1() =
      unfold((0, 1))((x) => x match {
        case (m, n) => Some((n, (n, m + n)))
      })

    def from1(n: Int) =
      unfold(n)((x) => Some((x, x + 1)))

    def constant1(v: Int) =
      unfold(v)(_ => Some((v, v)))

    def ones1() = constant1(1)


  }

}
