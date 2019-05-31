
object Chapter6 extends App {

  trait RNG {
    def nextInt: (Int, RNG)
  }

  object RNG {

    // NB - this was called SimpleRNG in the book text

    case class Simple(seed: Long) extends RNG {
      def nextInt: (Int, RNG) = {
        val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
        val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
        val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
        (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
      }
    }

    type Rand[+A] = RNG => (A, RNG)

    val int: Rand[Int] = _.nextInt

    def unit[A](a: A): Rand[A] =
      rng => (a, rng)

    def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
      rng => {
        val (a, rng2) = s(rng)
        (f(a), rng2)
      }

    // 6.1
    def nonNegativeInt(rng: RNG): (Int, RNG) = {
      val (v, r) = rng.nextInt
      (if (v < 0) -(v + 1) else v, r)
    }

    // 6.2
    def double(rng: RNG): (Double, RNG) = {
      val (v, r) = nonNegativeInt(rng)
      (v / (Int.MaxValue.toDouble + 1), r)
    }

    // 6.3
    def intDouble(rng: RNG): ((Int, Double), RNG) = {
      val (i, r1) = rng.nextInt
      val (d, r2) = double(r1)
      ((i, d), r2)
    }

    def doubleInt(rng: RNG): ((Double, Int), RNG) = {
      val ((i, d), r) = intDouble(rng)
      ((d, i), r)
    }

    def double3(rng: RNG): ((Double, Double, Double), RNG) = {
      val (d1, r1) = double(rng)
      val (d2, r2) = double(r1)
      val (d3, r3) = double(r2)
      ((d1, d2, d3), r3)
    }

    // 6.4
    def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
      if (count > 0) {
        val (v, r1) = rng.nextInt
        val rest = ints(count - 1)(r1)
        (v :: rest._1, rest._2)
      } else (Nil, rng)
    }

    // 6.5
    def double1(rng: RNG): Rand[Double] =
      map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

    // 6.6
    def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
      val (v1, r1) = ra(rng)
      val (v2, r2) = rb(r1)
      (f(v1, v2), r2)
    }

    // 6.7
    def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = rng => {
      fs.foldRight((List[A](), rng))((x, s) => {
        val (v, r) = x(s._2)
        (v :: s._1, r)
      })
    }

    def ints1(count: Int)(rng: RNG): Rand[List[Int]] =
      sequence(List.fill(count)(int))

    // 6.8
    def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
      val (v, r) = f(rng)
      g(v)(r)
    }

    def nonNegativeLessThan(n: Int): Rand[Int] =
      flatMap(nonNegativeInt) { i =>
        val mod = i % n
        if (i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
      }

    // 6.9
    def mapViaFlatMap[A, B](r: Rand[A])(f: A => B): Rand[B] =
      flatMap(r)(v => unit(f(v)))

    def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
      flatMap(ra)(a => map(rb)(b => f(a, b)))
  }

  import State._

  case class State[S, +A](run: S => (A, S)) {
    // 6.10
    def map[B](f: A => B): State[S, B] =
      flatMap(v => unit(f(v)))

    def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
      flatMap(a => sb.map(b => f(a, b)))

    def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
      val (v, s1) = run(s)
      f(v).run(s1)
    })
  }

  sealed trait Input

  case object Coin extends Input

  case object Turn extends Input

  case class Machine(locked: Boolean, candies: Int, coins: Int)

  object State {
    // 6.10
    def unit[S, A](a: A): State[S, A] = State(s => (a, s))

    // 6.10
    def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] =
      fs.foldRight(unit[S, List[A]](List[A]()))((x, s) => x.map2(s)(_ :: _))

    type Rand[A] = State[RNG, A]

    // 6.11 :-/
  }
}