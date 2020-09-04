package test.gameoperations

import org.junit.Assert._
import org.junit.Test
import sudoku.GameOperations.GameFinishedState
import sudoku.{GameOperations, SudokuTable}

class TestCheckGameEnd {

  def getDefTable: Array[Array[Int]] = {
      (for (i <- 0 to 8) yield {
        val shift = (i % 3) * 3 + i / 3
        ((10 - shift to 9) ++ (1 to 9 - shift)).toArray
      }).toArray
  }

  @Test
  def testFinishedCorrect(): Unit = {

    val table: Array[Array[Int]] = getDefTable
    val vecTable = table.map({ row => row.toVector }).toVector

    val origin = Array.fill(9)(Array.fill(9)(0).toVector).toVector

    val sudoku = SudokuTable(vecTable, origin, (0, 0))

    assertEquals(
      "Wrong game and state returned",
      GameFinishedState(isFilled = true, anyErrors = false),
      GameOperations.checkGameEnd(sudoku)
    )

  }

  @Test
  def testFinishedIncorrect(): Unit = {

    val table: Array[Array[Int]] = getDefTable
    table(4)(4) = table(4)(3)
    val vecTable = table.map({ row => row.toVector }).toVector

    val origin = Array.fill(9)(Array.fill(9)(0).toVector).toVector

    val sudoku = SudokuTable(vecTable, origin, (0, 0))

    assertEquals(
      "Wrong game and state returned",
      GameFinishedState(isFilled = true, anyErrors = true),
      GameOperations.checkGameEnd(sudoku)
    )

  }

  @Test
  def testUnFinishedCorrect(): Unit = {

    val table: Array[Array[Int]] = getDefTable
    table(4)(4) = 0
    val vecTable = table.map({ row => row.toVector }).toVector

    val origin = Array.fill(9)(Array.fill(9)(0).toVector).toVector

    val sudoku = SudokuTable(vecTable, origin, (0, 0))

    assertEquals(
      "Wrong game and state returned",
      GameFinishedState(isFilled = false, anyErrors = false),
      GameOperations.checkGameEnd(sudoku)
    )

  }

  @Test
  def testUnFinishedIncorrect(): Unit = {

    val table: Array[Array[Int]] = getDefTable
    table(3)(3) = 0
    table(4)(4) = table(3)(4)
    val vecTable = table.map({ row => row.toVector }).toVector

    val origin = Array.fill(9)(Array.fill(9)(0).toVector).toVector

    val sudoku = SudokuTable(vecTable, origin, (0, 0))

    assertEquals(
      "Wrong game and state returned",
      GameFinishedState(isFilled = false, anyErrors = true),
      GameOperations.checkGameEnd(sudoku)
    )

  }

}
