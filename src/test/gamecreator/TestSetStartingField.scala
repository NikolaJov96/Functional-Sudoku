package test.gamecreator

import org.junit.Assert._
import org.junit.Test
import sudoku.{GameCreator, SudokuTable}

class TestSetStartingField {

  @Test
  def testSetStartingField(): Unit = {
    GameCreator.setStartingField(GameCreator.setField(new SudokuTable(), 0, 0, Some(1)).right.get, 1, 1) match {
      case Right(sudoku) => assertEquals("Starting field movement incorrect", new SudokuTable((1, 1)), sudoku)
      case Left(_) => assert(assertion = false, "Valid staring field change failed")
    }
  }

}
