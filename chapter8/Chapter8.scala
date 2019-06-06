import Chapter6._

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

object Chapter8 extends App {

  trait Prop {
    def check: Boolean

    // 8.3
    def &&(p: Prop): Prop = new Prop {
      def check = this.check && p.check
    }
  }

  object Prop {
    def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
  }

  object Gen {
    def unit[A](a: => A): Gen[A] = ???
  }

  trait Gen[A] {
    def map[A, B](f: A => B): Gen[B] = ???

    def flatMap[A, B](f: A => Gen[B]): Gen[B] =

  }

  case class Gen1[A](sample: State[RNG, A]) {
    // 8.4
    def choose(start: Int, stopExclusive: Int): Gen1[Int] = {
      Gen1[Int](State(RNG.nonNegativeLessThan(stopExclusive - start)).map(v => v + start))
    }

    // 8.5
    def unit[A](a: => A): Gen1[A] = Gen1(State(s => (a, s)))

    def boolean: Gen1[Boolean] = Gen1(State(RNG.nonNegativeLessThan(2)).map(_ == 1))

    // 8.6
    def flatMap[A, B](f: A => Gen1[B]): Gen1[B] =
      Gen1(sample.flatMap(x => f(x).sample))

    def listOfN[A](n: Int, g: Gen1[A]): Gen1[List[A]] =
      Gen1(State.sequence(List.fill(n)(g.sample)))

    // 8.7
    def union1[A](g1: Gen1[A], g2: Gen1[A]): Gen1[A] =
      boolean.flatMap(if (_) g1 else g2)

    // 8.8
    def weighted[A](g1: (Gen1[A], Double), g2: (Gen1[A], Double)): Gen1[A] = {
      val l = g1._2 + g2._2
      val p = g1._2 / l
      Gen1(State(RNG.double).flatMap(x => if (x <= p) g1._1.sample else g2._1.sample))
    }

    // 8.9

  }

  object Gen1 {
  }

  trait SGen[+A] {

  }

}
