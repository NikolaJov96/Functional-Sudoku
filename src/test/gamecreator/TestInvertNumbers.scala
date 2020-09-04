package test.gamecreator

import org.junit.Assert._
import org.junit.Test
import sudoku.{GameCreator, SudokuTable}

class TestInvertNumbers {

  @Test
  def testInvertNumbers(): Unit = {
    val table = Array.fill(9)((1 to 9).toVector).toVector
    val invTable = Array.fill(9)((9 to 1 by -1).toVector).toVector
    assertEquals(
      "Incorrect number inversion",
      new SudokuTable(invTable, invTable, (0, 0)),
      GameCreator.invertNumbers(new SudokuTable(table, table, (0, 0)))
    )
  }

}
