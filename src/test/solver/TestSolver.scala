package test.solver

import org.junit.Assert._
import org.junit.Test
import sudoku.{GameOperations, Solver, SudokuTable}

class TestSolver {

  def getDefTable: Array[Array[Int]] = {
    (for (i <- 0 to 8) yield {
      val shift = (i % 3) * 3 + i / 3
      ((10 - shift to 9) ++ (1 to 9 - shift)).toArray
    }).toArray
  }

  @Test
  def testCompletedBoard(): Unit = {

    val vecTable = getDefTable.map({row => row.toVector}).toVector
    val sudoku = new SudokuTable(vecTable, vecTable, (0, 0))

    val maybeSolution = Solver.solve(sudoku)

    assert(maybeSolution.isRight, "Unable to solve solved game")

    assertEquals(
      "Wrong output for completed sudoku game",
      (sudoku, List[Char]()),
      maybeSolution.right.get
    )

  }

  @Test
  def testOneMissing(): Unit = {

    val modTable = getDefTable
    val remVal = modTable(4)(4)
    modTable(4)(4) = 0
    val vecTable = modTable.map({row => row.toVector}).toVector
    val sudoku = new SudokuTable(vecTable, vecTable, (0, 0))
    val resTable = getDefTable.map({row => row.toVector}).toVector
    val resSudoku = new SudokuTable(resTable, vecTable, (4, 4))

    val maybeSolution = Solver.solve(sudoku)

    assert(maybeSolution.isRight, "Unable to solve solved game")

    assertEquals(
      "Wrong output for completed sudoku game",
      (resSudoku, List[Char]('d', 'd', 'd', 'd', 'r', 'r', 'r', 'r', remVal.toString.charAt(0))),
      maybeSolution.right.get
    )

  }

  @Test
  def testOneMissingOneIncorrect(): Unit = {

    val modTable = getDefTable
    modTable(4)(4) = 0
    modTable(4)(5) = modTable(4)(6)
    val vecTable = modTable.map({row => row.toVector}).toVector

    val maybeSolution = Solver.solve(new SudokuTable(vecTable, vecTable, (0, 0)))

    assert(maybeSolution.isLeft, "Solved invalid table")

    assertEquals(
      "Wrong error message",
      Solver.TableContainsErrors,
      maybeSolution.left.get
    )

  }

  @Test
  def testFile1(): Unit = {

    val maybeSolved = Solver.solve(SudokuTable.createFromFile("test_files/file1.txt").right.get)

    assert(maybeSolved.isRight, "Valid table not solved")

    assert(!GameOperations.checkGameEnd(maybeSolved.right.get._1).anyErrors, "Solved table not valid")

    val maybeSolvedBySeq = GameOperations.executeSequence(
      SudokuTable.createFromFile("test_files/file1.txt").right.get,
      maybeSolved.right.get._2)

    assert(maybeSolved.isRight, "Could not solve using produced operation sequence")

    assert(
      !GameOperations.checkGameEnd(maybeSolvedBySeq.right.get).anyErrors,
      "Solving by produced operation sequence produced invalid table"
    )
  }

  @Test
  def testFile1Box11Removed(): Unit = {

    val sudoku = SudokuTable.createFromFile("test_files/file1.txt").right.get
    val newTable = sudoku.origin.map({ row => row.toArray })
    for (i <- 0 to 2; j <- 0 to 2) newTable(i)(j) = 0
    val vecNewTable = newTable.map({ row => row.toVector })

    val maybeSolved = Solver.solve(new SudokuTable(vecNewTable, vecNewTable, sudoku.selectedField))

    assert(maybeSolved.isLeft, "Impossible table solved")

    assertEquals("Wrong output message", Solver.NoUniqueStepFound, maybeSolved.left.get)

  }

  @Test
  def testFile1Box13Removed(): Unit = {

    val sudoku = SudokuTable.createFromFile("test_files/file1.txt").right.get
    val newTable = sudoku.origin.map({ row => row.toArray })
    for (i <- 0 to 2; j <- 6 to 8) newTable(i)(j) = 0
    val vecNewTable = newTable.map({ row => row.toVector })

    val maybeSolved = Solver.solve(new SudokuTable(vecNewTable, vecNewTable, sudoku.selectedField))

    assert(maybeSolved.isLeft, "Impossible table solved")

    assertEquals("Wrong output message", Solver.NoUniqueStepFound, maybeSolved.left.get)

  }

}
