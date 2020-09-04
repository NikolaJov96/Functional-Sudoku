package test.gameoperations

import org.junit.Assert._
import org.junit.Test
import sudoku.{GameOperations, SudokuTable}

class TestExecuteOperation {
  
  def getDummySudoku: SudokuTable = {
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
    SudokuTable(table, table, (0, 0))
  }

  @Test
  def testValidOperationU(): Unit = {
    val sudoku = getDummySudoku.setSelectedField(4, 4)
    val maybeSudoku = GameOperations.executeOperation(sudoku, 'u')
    assert(maybeSudoku.isRight, "Valid operation filed")
    assertEquals("Invalid operation result", GameOperations.moveSelectionUp(sudoku), maybeSudoku.right.get)
  }

  @Test
  def testInvalidOperationU(): Unit = {
    val sudoku = getDummySudoku.setSelectedField(0, 4)
    val maybeSudoku = GameOperations.executeOperation(sudoku, 'u')
    assert(maybeSudoku.isRight, "Valid operation filed")
    assertEquals("Invalid operation result", GameOperations.moveSelectionUp(sudoku), maybeSudoku.right.get)
  }

  @Test
  def testValidOperationD(): Unit = {
    val sudoku = getDummySudoku.setSelectedField(4, 4)
    val maybeSudoku = GameOperations.executeOperation(sudoku, 'd')
    assert(maybeSudoku.isRight, "Valid operation filed")
    assertEquals("Invalid operation result", GameOperations.moveSelectionDown(sudoku), maybeSudoku.right.get)
  }

  @Test
  def testInvalidOperationD(): Unit = {
    val sudoku = getDummySudoku.setSelectedField(8, 4)
    val maybeSudoku = GameOperations.executeOperation(sudoku, 'd')
    assert(maybeSudoku.isRight, "Valid operation filed")
    assertEquals("Invalid operation result", GameOperations.moveSelectionDown(sudoku), maybeSudoku.right.get)
  }

  @Test
  def testValidOperationL(): Unit = {
    val sudoku = getDummySudoku.setSelectedField(4, 4)
    val maybeSudoku = GameOperations.executeOperation(sudoku, 'l')
    assert(maybeSudoku.isRight, "Valid operation filed")
    assertEquals("Invalid operation result", GameOperations.moveSelectionLeft(sudoku), maybeSudoku.right.get)
  }

  @Test
  def testInvalidOperationL(): Unit = {
    val sudoku = getDummySudoku.setSelectedField(4, 0)
    val maybeSudoku = GameOperations.executeOperation(sudoku, 'l')
    assert(maybeSudoku.isRight, "Valid operation filed")
    assertEquals("Invalid operation result", GameOperations.moveSelectionLeft(sudoku), maybeSudoku.right.get)
  }

  @Test
  def testValidOperationR(): Unit = {
    val sudoku = getDummySudoku.setSelectedField(4, 4)
    val maybeSudoku = GameOperations.executeOperation(sudoku, 'r')
    assert(maybeSudoku.isRight, "Valid operation filed")
    assertEquals("Invalid operation result", GameOperations.moveSelectionRight(sudoku), maybeSudoku.right.get)
  }

  @Test
  def testInvalidOperationR(): Unit = {
    val sudoku = getDummySudoku.setSelectedField(4, 8)
    val maybeSudoku = GameOperations.executeOperation(sudoku, 'r')
    assert(maybeSudoku.isRight, "Valid operation filed")
    assertEquals("Invalid operation result", GameOperations.moveSelectionRight(sudoku), maybeSudoku.right.get)
  }
  
  @Test
  def testValidOperationNumber(): Unit = {
    val sudoku = getDummySudoku.setSelectedField(0, 1)
    val maybeSudoku = GameOperations.executeOperation(sudoku, '1')
    assert(maybeSudoku.isRight, "Valid operation filed")
    assertEquals("Invalid operation result", sudoku.setTableValue(sudoku.selectedField, Option(1)), maybeSudoku.right.get)
  }

  @Test
  def testInvalidOperationNumber(): Unit = {
    val sudoku = getDummySudoku.setSelectedField(0, 1)
    assert(GameOperations.executeOperation(sudoku, '0').isLeft, "Invalid operation passed")
  }

}
