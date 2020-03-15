package sandbox

object StateMonad extends App {
  case class State[S, A](run: S => (S, A)){
    def map[B](f: A => B): State[S, B] = flatMap(a => State.lift(f(a)))

    def flatMap[B](f: A => State[S, B]): State[S, B] = State{
      state => {
        val (s, a) = run(state)
        f(a).run(s)
      }
    }
  }

  object State{
    def lift[S, A](a: A): State[S, A] = State {
      state => (state, a)
    }
  }

  case class Currency(value: Int, symbol: String);

  val myMoney = Currency(50, "€");

  def addMoney(amount: Int): State[Currency, Int] = State{
    state => {
      val newAmount = amount + state.value
      (state.copy(value = newAmount), newAmount)
    }
  }

  def removeMoney(amount: Int): State[Currency, Int] = addMoney(-1 * amount)

  val myTransaction = for {
    _ <- addMoney(50)
    _ <- removeMoney(20)
    _ <- removeMoney(50)
  } yield ()

  println(myTransaction.run(myMoney)._1)
}
