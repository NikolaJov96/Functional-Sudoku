package test.gameoperations

import org.junit.Assert._
import org.junit.Test
import sudoku.GameOperations._
import sudoku.SudokuTable

class TestSelectionMovement {

  @Test
  def testValidUp(): Unit = {
    assertEquals("Incorrect up movement", new SudokuTable((3, 4)), moveSelectionUp(new SudokuTable((4, 4))))
  }

  @Test
  def testInvalidUp(): Unit = {
    assertEquals("Incorrect up movement", new SudokuTable((0, 4)), moveSelectionUp(new SudokuTable((0, 4))))
  }

  @Test
  def testValidDown(): Unit = {
    assertEquals("Incorrect down movement", new SudokuTable((5, 4)), moveSelectionDown(new SudokuTable((4, 4))))
  }

  @Test
  def testInvalidDown(): Unit = {
    assertEquals("Incorrect down movement", new SudokuTable((8, 4)), moveSelectionDown(new SudokuTable((8, 4))))
  }

  @Test
  def testValidLeft(): Unit = {
    assertEquals("Incorrect left movement", new SudokuTable((4, 3)), moveSelectionLeft(new SudokuTable((4, 4))))
  }

  @Test
  def testInvalidLeft(): Unit = {
    assertEquals("Incorrect left movement", new SudokuTable((4, 0)), moveSelectionLeft(new SudokuTable((4, 0))))
  }

  @Test
  def testValidRight(): Unit = {
    assertEquals("Incorrect right movement", new SudokuTable((4, 5)), moveSelectionRight(new SudokuTable((4, 4))))
  }

  @Test
  def testInvalidRight(): Unit = {
    assertEquals("Incorrect right movement", new SudokuTable((4, 8)), moveSelectionRight(new SudokuTable((4, 8))))
  }

}
