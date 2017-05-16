
sealed trait Stream[ A ] {

  import Stream._

  def map[ B ](f: A => B): Stream[ B ] = {
    Map(this, f)
  }

  def zip[ B ](that: Stream[ B ]): Stream[ (A, B) ] = {
    Zip(this, that)
  }

  def flatMap[ B ](f: A => Stream[ B ]): Stream[ B ] = {
    FlatMap(this, f)
  }

  def runFold[ B ](zero: B)(f: (B, A) => B): B = {
    def next[ A ](stream: Stream[ A ]): Option[ A ] =
      stream match {
        case FromIterator(source) =>
          if (source.hasNext) Some(source.next()) else None
        case Map(source, f) =>
          next(source).map(f)
        case Zip(left, right) =>
          for {
            l <- next(left)
            r <- next(right)
          } yield (l, r)
      }

    def loop(result: B): B =
      next(this) match {
        case None => result
        case Some(a) =>
          loop(f(result, a))
      }
    loop(zero)
  }

}

object Stream {

  def fromIterator[ A ](source: Iterator[ A ]): Stream[ A ] = FromIterator(source)

  final case class Map[ A, B ](source: Stream[ A ], f: A => B) extends Stream[ B ]

  final case class Zip[ A, B ](left: Stream[ A ], right: Stream[ B ]) extends Stream[ (A, B) ]

  final case class FlatMap[ A, B ](source: Stream[ A ], f: A => Stream[ B ]) extends Stream[ B ]

  final case class FromIterator[ A ](source: Iterator[ A ]) extends Stream[ A ]

}