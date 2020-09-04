package test.sudokutable

import org.junit.Assert._
import org.junit.Test
import sudoku.SudokuTable

class TestSaveToFile {

  @Test
  def testLoadSaveLoad(): Unit = {
    val maybeSudoku = SudokuTable.createFromFile("test_files/file1.txt")

    assert(maybeSudoku.isRight, "Valid file load failed")
    val sudoku = maybeSudoku.right.get

    sudoku.saveToFile("test_files/out_file1.txt")

    val maybeSudoku1 = SudokuTable.createFromFile("test_files/out_file1.txt")

    assert(maybeSudoku1.isRight, "Failed to load saved game file")
    val sudoku1 = maybeSudoku1.right.get

    assertEquals(
      "Reloaded file different to saved one",
      sudoku,
      sudoku1
    )
  }

}
