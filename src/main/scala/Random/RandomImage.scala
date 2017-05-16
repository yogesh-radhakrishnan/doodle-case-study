package Random

import doodle.core._
import doodle.jvm.Java2DCanvas


sealed trait Random[ A ] {

  def map[ B ](f: A => B): Random[ B ] = Map(this, f)

  def zip[ B ](that: Random[ B ]): Random[ (A, B) ] = {
    this flatMap { x =>
      that map { y =>
        (x, y)
      }
    }

  }

  def flatMap[ B ](f: A => Random[ B ]): Random[ B ] = FlatMap(this, f)

  def run(rng: scala.util.Random): A = {
    this match {
      case Map(v, f) => f(v.run(rng))
      case FlatMap(v, f) => f(v.run(rng)).run(rng)
      case Primitive(f) => f(rng)
    }
  }

  def oneOf[ A ](elts: A*): Random[ A ] = {
    val length = elts.length
    Primitive { rng =>
      val index = rng.nextInt(length)
      elts(index)
    }
  }

  val boolean: Random[ Boolean ] = Primitive(rng => rng.nextBoolean())

  def shape(size: Double): Random[ Image ] = {
    for {
      b <- boolean
    } yield if (b) Triangle(size, size) else Circle(size / 2)
  }

}

object Random {
  val double: Random[ Double ] = Primitive(v => v.nextDouble())

  val int: Random[ Int ] = Primitive(v => v.nextInt())

  val intSeed: (Int) => Random[ Int ] = (x) => Primitive(v => v.nextInt(x))

  val normal: Random[ Double ] = Primitive(v => v.nextGaussian())

  def always[ A ](in: A): Random[ A ] = Primitive(v => in)
}

final case class Map[ A, B ](value: Random[ A ], f: (A) => B) extends Random[ B ]

final case class FlatMap[ A, B ](value: Random[ A ], f: (A) => Random[ B ]) extends Random[ B ]

final case class Primitive[ A ](f: (scala.util.Random) => A) extends Random[ A ]

object testObject {
  def main(args: Array[ String ]): Unit = {

    def rectangle(size: Double): Image = {
      println(s"Creating a rectangle")
      Rectangle(size, size)
    }

    def sierpinski(n: Int, size: Double): Image = {
      println(s"Creating a Sierpinski with n = $n")
      n match {
        case 0 =>
          rectangle(size)
        case n =>
          val smaller = sierpinski(n - 1, size / 2)
          smaller above (smaller beside smaller)
      }
    }
    implicit val canvas = Java2DCanvas.canvas

    sierpinski(10,10) draw(canvas, 10, 10)
  }
}
