package test.gameoperations

import org.junit.Assert._
import org.junit.Test
import sudoku.{GameOperations, SudokuTable}

class TestValueChange {

  @Test
  def testValidValueChange(): Unit = {

    val table = Vector(
      Vector(4, 0, 1, 2, 9, 0, 0, 7, 5),
      Vector(2, 0, 0, 3, 0, 0, 8, 0, 0),
      Vector(0, 7, 0, 0, 8, 0, 0, 0, 6),
      Vector(0, 0, 0, 1, 0, 3, 0, 6, 2),
      Vector(1, 0, 5, 0, 0, 0, 4, 0, 3),
      Vector(7, 3, 0, 6, 0, 8, 0, 0, 0),
      Vector(6, 0, 0, 0, 2, 0, 0, 3, 0),
      Vector(0, 0, 7, 0, 0, 1, 0, 0, 4),
      Vector(8, 9, 0, 0, 6, 5, 1, 0, 7),
    )

    val sudoku = SudokuTable(table, table, (0, 1))
    val maybeUpdatedSudoku = GameOperations.writeTheValue(sudoku, Option(1))

    assert(maybeUpdatedSudoku.isRight, "Failed to execute valid value change")
    val updatedSudoku = maybeUpdatedSudoku.right.get

    val resTable = Vector(
      Vector(4, 1, 1, 2, 9, 0, 0, 7, 5),
      Vector(2, 0, 0, 3, 0, 0, 8, 0, 0),
      Vector(0, 7, 0, 0, 8, 0, 0, 0, 6),
      Vector(0, 0, 0, 1, 0, 3, 0, 6, 2),
      Vector(1, 0, 5, 0, 0, 0, 4, 0, 3),
      Vector(7, 3, 0, 6, 0, 8, 0, 0, 0),
      Vector(6, 0, 0, 0, 2, 0, 0, 3, 0),
      Vector(0, 0, 7, 0, 0, 1, 0, 0, 4),
      Vector(8, 9, 0, 0, 6, 5, 1, 0, 7),
    )
    val resSudoku = SudokuTable(resTable, table, (0, 1))

    assertEquals("Value write did not produce expected sudoku object", resSudoku, updatedSudoku)

  }

  @Test
  def testInvalidValueChange(): Unit = {

    val table = Vector(
      Vector(4, 0, 1, 2, 9, 0, 0, 7, 5),
      Vector(2, 0, 0, 3, 0, 0, 8, 0, 0),
      Vector(0, 7, 0, 0, 8, 0, 0, 0, 6),
      Vector(0, 0, 0, 1, 0, 3, 0, 6, 2),
      Vector(1, 0, 5, 0, 0, 0, 4, 0, 3),
      Vector(7, 3, 0, 6, 0, 8, 0, 0, 0),
      Vector(6, 0, 0, 0, 2, 0, 0, 3, 0),
      Vector(0, 0, 7, 0, 0, 1, 0, 0, 4),
      Vector(8, 9, 0, 0, 6, 5, 1, 0, 7),
    )

    val sudoku = SudokuTable(table, table, (0, 0))
    val maybeUpdatedSudoku = GameOperations.writeTheValue(sudoku, Option(1))

    assert(maybeUpdatedSudoku.isLeft, "Invalid value change not reported")

    assertEquals("Wrong message reported", GameOperations.OriginOverwriteError, maybeUpdatedSudoku.left.get)

  }

}
