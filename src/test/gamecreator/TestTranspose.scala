package test.gamecreator

import org.junit.Assert._
import org.junit.Test
import sudoku.{GameCreator, SudokuTable}

class TestTranspose {

  @Test
  def testTranspose(): Unit = {
    assertEquals(
      "Incorrectly transposed",
      GameCreator.setField(new SudokuTable((1, 3)), 3, 7, Some(5)).right.get,
      GameCreator.transpose(GameCreator.setField(new SudokuTable(3, 1), 7, 3, Some(5)).right.get)
    )
  }

}
