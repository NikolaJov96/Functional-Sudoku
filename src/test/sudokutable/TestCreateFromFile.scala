package test.sudokutable

import org.junit.Test
import org.junit.Assert._
import sudoku.SudokuTable

class TestCreateFromFile {

  @Test
  def testFile1(): Unit = {

    val maybeSudoku = SudokuTable.createFromFile("test_files/file1.txt")

    assert(maybeSudoku.isRight, "Correct file loading failed")

    val sudoku = maybeSudoku.right.get
    
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

    for (i <- 0 until SudokuTable.Dim) assertEquals("Wrong table content", sudoku.table(i), table(i))

    val row = Vector(5, 0, 1, 2, 9, 0, 0, 7, 5)
    assertNotEquals("Different tables found equal", row, table(0))

    assertEquals("Wrong field selected", (1, 8), sudoku.selectedField)

  }

  @Test
  def testFile2(): Unit = {

    val maybeSudoku = SudokuTable.createFromFile("test_files/file2.txt")

    assert(maybeSudoku.isLeft, "Corrupt file loaded")

    assertEquals("Wrong error message", SudokuTable.UnexpectedCharError, maybeSudoku.left.get)

  }


}
