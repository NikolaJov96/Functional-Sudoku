package test.gamecreator

import org.junit.Assert._
import org.junit.Test
import sudoku.{GameCreator, SudokuTable}

class TestFilters {

  @Test
  def testFilterLines(): Unit = {

    val resTable = Array.fill(9)(Array.fill(9)(0)).toVector
    resTable(1)(4) = 5
    resTable(4)(3) = 7
    val vecResTable = resTable.map({ row => row.toVector })

    val table = Array.fill(9)(Array.fill(9)(0)).toVector
    table(1)(4) = 5
    table(4)(3) = 7
    table(4)(4) = 1
    table(2)(4) = 1
    table(7)(4) = 1
    table(4)(1) = 1
    table(4)(5) = 1
    val vecTable = table.map({ row => row.toVector })

    assertEquals(
      "Wrong line filtering",
      new SudokuTable(vecResTable, vecResTable, (0, 0)),
      GameCreator.filterLines(SudokuTable(vecTable, vecTable, (0, 0)), 4, 4).right.get
    )

  }

  @Test
  def testFilterBox(): Unit = {

    val resTable = Array.fill(9)(Array.fill(9)(0)).toVector
    resTable(1)(4) = 1
    resTable(4)(1) = 1
    resTable(0)(0) = 5
    val vecResTable = resTable.map({ row => row.toVector })

    val table = Array.fill(9)(Array.fill(9)(0)).toVector
    table(1)(4) = 1
    table(4)(1) = 1
    table(0)(0) = 5
    table(1)(1) = 1
    table(1)(0) = 1
    table(2)(2) = 1
    val vecTable = table.map({ row => row.toVector })

    assertEquals(
      "Wrong box filtering",
      new SudokuTable(vecResTable, vecResTable, (0, 0)),
      GameCreator.filterBox(SudokuTable(vecTable, vecTable, (0, 0)), 1, 1).right.get
    )

  }

}
