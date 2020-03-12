package sandbox

import scala.io.StdIn.readLine;

object IOMonad extends App {
  case class IO[A](run: () => A) { self =>
    def map[B](f: A => B): IO[B] = {
      IO(() => f(self.run()))
    }

    def flatMap[B](f: A => IO[B]): IO[B] = {
      f(self.run())
    }
  }

  object IO {
    def pure[A](f: => A): IO[A] = IO(() => f)
  }

  def print(s: String): IO[Unit] = IO(() => println(s))
  def read(): IO[String]         = IO(() => readLine)

  val value = for {
    _ <- print("Hi")
    _ <- print("What is your name ?")
    n <- read
    _ <- print("Your name is : " + n)
  } yield ()

  println(value.run())
}
