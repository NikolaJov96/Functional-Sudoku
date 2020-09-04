package test.gamecreator

import org.junit.Assert._
import org.junit.Test
import sudoku.{GameCreator, SudokuTable}

class TestSetField {

  @Test
  def testValidRemove(): Unit = {
    val maybeSudoku = GameCreator.setField(
      new SudokuTable().setTableValue((1, 1), Some(1)).setOriginValue((1, 1), Some(1)), 1, 1, None
    )
    assert(maybeSudoku.isRight, "Unsuccessful valid removal")
    assertEquals("Valid removal incorrect", new SudokuTable(), maybeSudoku.right.get)
  }

  @Test
  def testOutOfBounds(): Unit = {
    val maybeSudoku = GameCreator.setField(new SudokuTable(), 9, 9, None)
    assert(maybeSudoku.isLeft, "Out of bounds not caught")
    assertEquals("Incorrect error message", GameCreator.OutOfBoundsError, maybeSudoku.left.get)
  }

  @Test
  def testValidSetNumber(): Unit = {
    val maybeSudoku = GameCreator.setField(
      new SudokuTable(), 1, 1, Some(1)
    )
    assert(maybeSudoku.isRight, "Unsuccessful valid removal")
    assertEquals(
      "Valid field write incorrect",
      new SudokuTable().setTableValue((1, 1), Some(1)).setOriginValue((1, 1), Some(1)),
      maybeSudoku.right.get
    )
  }

  @Test
  def testInvalidSetNumber(): Unit = {
    val maybeSudoku = GameCreator.setField(
      new SudokuTable(), 1, 1, Some(0)
    )
    assert(maybeSudoku.isLeft, "Invalid field value not caught")
    assertEquals("Incorrect error message", GameCreator.InvalidValueError, maybeSudoku.left.get)
  }

}
