package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
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

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeIntRand: Rand[Int] =
    map(int)(i => if (i < 0) i + Integer.MAX_VALUE + 1 else i)

  def nonNegativeInt(rng: RNG): (Int, RNG) =
    nonNegativeIntRand(rng)

  def doubleRand: Rand[Double] =
    map(nonNegativeInt)(_.toDouble / Integer.MAX_VALUE.toDouble + 1)

  def double(rng: RNG): (Double, RNG) =
    doubleRand(rng)

  def bothRand[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  def intDoubleRand: Rand[(Int,Double)] = 
    bothRand(int, doubleRand)

  def intDouble(rng: RNG): ((Int,Double), RNG) =
    intDoubleRand(rng)

  def doubleIntRand: Rand[(Double,Int)] =
    bothRand(doubleRand, int)

  def doubleInt(rng: RNG): ((Double,Int), RNG) =
    doubleIntRand(rng)

  def double3Rand: Rand[(Double,Double,Double)] = 
    map2(bothRand(doubleRand, doubleRand), doubleRand)((t, d) => (t._1, t._2, d))

  def double3(rng: RNG): ((Double,Double,Double), RNG) = 
    double3Rand(rng)

  def intsRand(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    intsRand(count)(rng)

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = { rng =>
    val (a, rng1) = ra(rng)
    val (b, rng2) = rb(rng1)
    (f(a, b), rng2)
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]())){ map2(_, _)(_ :: _) }

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = { rng =>
    val (a, rng1) = f(rng)
    g(a)(rng1)
  }

  def mapViaFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(f andThen unit)

  def map2ViaFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => flatMap(rb)(b => unit(f(a, b))))

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeIntRand) { i =>
      val mod = i % n
      if (i + (n - 1) - mod >= 0)
        unit(mod)
      else
        nonNegativeLessThan(n)
    }
}

case class State[S,+A](run: S => (A, S)) {

  import State._

  def map[B](f: A => B): State[S, B] =
    flatMap(f andThen unit)

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State { s =>
      val (a, ss) = run(s)
      f(a).run(ss)
    }

}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int) {
  def transition(input: Input): Machine = {
    (candies > 0, locked, input) match {
      case (false, _, _ )   => this
      case (_, true, Coin)  => this.copy(locked = false, coins = coins + 1)
      case (_, false, Turn) => this.copy(locked = true, candies = candies - 1)
      case _                => this
    }
  }
}

object State {

  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] =
    fs.foldRight(unit[S, List[A]](Nil)) {
      _.map2(_)(_ :: _)
    }

  def get[S]: State[S, S] =
    State(s => (s, s))

  def gets[S, A](f: S => A): State[S, A] =
    get.map(f)

  def put[S](s: S): State[S, Unit] =
    State(_ => ((), s))

  def set[S](s: S): State[S, Unit] = put(s)

  def modify[S](f: S => S): State[S, Unit] =
    get flatMap (f andThen put)
}

object Candy {
  import State._
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    for {
      _ <- sequence(inputs.map(input => modify((_: Machine).transition(input))))
      a <- gets((machine: Machine) => (machine.coins, machine.candies))
    } yield a
}
