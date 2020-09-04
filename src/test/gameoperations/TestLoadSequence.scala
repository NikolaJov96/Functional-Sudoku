package test.gameoperations

import org.junit.Assert._
import org.junit.Test
import sudoku.GameOperations

class TestLoadSequence {

  @Test
  def testLoadSequence(): Unit = {

    val maybeSequence = GameOperations.loadSequence("test_files/seq1.txt")

    assert(maybeSequence.isDefined, "Sequence loading failed")

    assertArrayEquals(
      "Sequence not loaded correctly",
      Array('u', 'u', 'd', 'd', 'l', 'l', 'r', 'r', '1', '2', '3', '4', '5', '6', '7', '8', '9'),
      maybeSequence.get.toArray
    )
  }

  @Test
  def testSaveSequence(): Unit = {

    val seq: List[Char] = List('u', 'u', 'd', 'd', 'l', 'l', 'r', 'r', '1', '2', '3', '4', '5', '6', '7', '8', '9')

    GameOperations.saveSequence("test_files/seq2.txt", seq)

    val maybeSequence = GameOperations.loadSequence("test_files/seq2.txt")

    assert(maybeSequence.isDefined, "Sequence loading failed")

    assertArrayEquals(
      "Sequence not loaded correctly",
      seq.toArray,
      maybeSequence.get.toArray
    )
  }

}
