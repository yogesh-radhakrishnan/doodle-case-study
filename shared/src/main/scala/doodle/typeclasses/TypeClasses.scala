package doodle.typeclasses


class TypeClasses {

  trait Functor[ A, F[ A ] ] {

    def map[ B ](fa: F[ A ])(f: A => B): F[ B ]
  }

  trait Applicative[ A, F[ A ] ] extends Functor[ A, F[ A ] ] {

    def zip[ B ](fa: F[ A ], fb: F[ B ]): F[ (A, B) ]

    def point[ A ](a: A): F[ A ]

  }

  trait Monad[ A, F[ A ] ] extends Functor[ A, F[ A ] ] {

    def flatMap[ B ](fa: F[ A ])(f: A => F[ B ]): F[ B ]

    def point[ A ](a: A): F[ A ]

  }

  trait Scannable[ A, F[ A ] ] {
    def scanLeft[ B ](seed: B)(f: (B, A) => B): F[ B ]

  }

}
