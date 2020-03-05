package life

import life.Life.GameState
import org.scalatest.flatspec.AnyFlatSpec

class LifeTest extends AnyFlatSpec {
  "Cells" should "detect cells 1 around" in {
    val state = GameState(
      Array(
        Array(0, 0, 0),
        Array(0, 0, 1),
        Array(1, 1, 1)
      ), (3, 3)
    )

    assert(Life.countAliveAround(state, (0, 0)) == 0)
    assert(Life.countAliveAround(state, (1, 0)) == 2)
    assert(Life.countAliveAround(state, (1, 1)) == 4)
    assert(Life.countAliveAround(state, (0, 1)) == 1)
    assert(Life.countAliveAround(state, (2, 0)) == 1)
    assert(Life.countAliveAround(state, (2, 1)) == 3)
    assert(Life.countAliveAround(state, (2, 2)) == 2)
    assert(Life.countAliveAround(state, (1, 2)) == 2)
    assert(Life.countAliveAround(state, (0, 2)) == 1)
  }

  "Next state" should "be coherent with previous one (1)" in {
    val from = GameState(
      Array(
        Array(0, 0, 0),
        Array(1, 0, 0),
        Array(1, 1, 0)
      ), (3, 3)
    )

    val to = GameState(
      Array(
        Array(0, 0, 0),
        Array(1, 1, 0),
        Array(1, 1, 0)
      ), (3, 3)
    )

    assert(Life.next(from).board.zipWithIndex.forall{case (elements, index) => elements sameElements to.board(index)})
  }

  "Next state" should "be coherent with previous one (2)" in {
    val from = GameState(
      Array(
        Array(0, 0, 0),
        Array(1, 1, 1),
        Array(0, 0, 0)
      ), (3, 3)
    )

    val to = GameState(
      Array(
        Array(0, 1, 0),
        Array(0, 1, 0),
        Array(0, 1, 0)
      ), (3, 3)
    )

    assert(Life.next(from).board.zipWithIndex.forall{case (elements, index) => elements sameElements to.board(index)})
  }
}
