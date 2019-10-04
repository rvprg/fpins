import Chapter8.{Gen, Prop}

import language.higherKinds
import scala.util.matching.Regex

object Chapter9 extends App {

  trait Parsers[Parser[+ _]] {
    self => // so inner classes may call methods of trait

    def map[A,B](a: Parser[A])(f: A => B): Parser[B]

    def map2[A,B,C](p: Parser[A], p2: Parser[B])(f: (A,B) => C): Parser[C]

    def many[A](p: Parser[A]): Parser[List[A]]

    // 9.1
    def product[A,B](p: Parser[A], p2: Parser[B]): Parser[(A,B)] =
      map2(p, p2)((_, _))

    def many1[A](p: Parser[A]): Parser[List[A]] =
      map2(p, many(p))(_ :: _)

    // 9.2
    // product(p1, product(p2, p3)) and product(product(p1, p2), p3) would
    // parse the string equally.

    // 9.3
    def many_0[A](p: Parser[A]): Parser[List[A]] =
      or(succeed[List[A]](List()), map2(p, many(p))(_ :: _))

    // 9.4
    def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
      map2(p, (if (n > 0) listOfN[A](n-1, p) else succeed[List[A]](List())))(_ :: _)

    def succeed[A](a: A): Parser[A] = map(string("")) (_ => a)

    def flatMap[A,B](p: Parser[A])(f: A => Parser[B]): Parser[B]

    // 9.6
    def parser = flatMap("[0-9]+".r)(v => listOfN(v.toInt, string("a")))

    // 9.7
    def map2_flatMap[A, B, C](p: Parser[A], p2: Parser[B])(f: (A,B) => C): Parser[C] =
      flatMap(p)(x => map(p2)(y => f(x, y)))
    def product_map2[A, B](p: Parser[A], p2: Parser[B]): Parser[(A, B)] =
      map2_flatMap(p, p2)((_, _))

    //9.8
    def map_flatMap_1[A, B](p: Parser[A], f: A => B): Parser[B] =
      map2(p, p)((x, _) => f(x))
    def map_flatMap_2[A, B](p: Parser[A], f: A => B): Parser[B] =
      flatMap(p)(x => succeed(f(x)))

    def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]
    implicit def regex(r: Regex): Parser[String]
    implicit def string(s: String): Parser[String]
    implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
    implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]):
    ParserOps[String] = ParserOps(f(a))

    case class ParserOps[A](p: Parser[A]) {
      def |[B>:A](p2: Parser[B]): Parser[B] = self.or(p,p2)
      def or[B>:A](p2: => Parser[B]): Parser[B] = self.or(p,p2)
    }

    object Laws {
    }
  }

  case class Location(input: String, offset: Int = 0) {

    lazy val line = input.slice(0, offset + 1).count(_ == '\n') + 1
    lazy val col = input.slice(0, offset + 1).reverse.indexOf('\n')

    def toError(msg: String): ParseError =
      ParseError(List((this, msg)))

    def advanceBy(n: Int) = copy(offset = offset + n)

    /* Returns the line corresponding to this location */
    def currentLine: String =
      if (input.length > 1) input.lines.drop(line - 1).next
      else ""
  }

  case class ParseError(stack: List[(Location, String)] = List(),
                        otherFailures: List[ParseError] = List()) {
  }
}