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

    def flatMap[A, B](f: A => Gen[B]): Gen[B] = ???
  }


  // 8.4
  case class Gen1[A](sample: State[RNG, A])

  def choose(start: Int, stopExclusive: Int): Gen1[Int] = {
    Gen1[Int](State(RNG.nonNegativeLessThan(stopExclusive - start)).map(v => v + start))
  }

  object Gen1 {
    // 8.5
    def unit[A](a: => A): Gen1[A] = Gen1(State(s => (a, s)))

    def boolean: Gen1[Boolean] = Gen1(State(RNG.nonNegativeLessThan(2)).map(_ == 1))

    def listOfN[A](n: Int, g: Gen1[A]): Gen1[List[A]] =
      Gen1(State.sequence(List.fill(n)(g.sample)))
  }

  trait SGen[+A] {

  }

}
