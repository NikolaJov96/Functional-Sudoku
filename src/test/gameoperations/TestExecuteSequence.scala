package test.gameoperations

import org.junit.Assert._
import org.junit.Test
import sudoku.{GameOperations, SudokuTable}

class TestExecuteSequence {

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
  def testValidSequence(): Unit = {

    assertEquals(

      "Expected objects not matching",

      GameOperations.executeOperation(
        GameOperations.executeOperation(
          GameOperations.executeOperation(
            GameOperations.executeOperation(
              GameOperations.executeOperation(
                GameOperations.executeOperation(
                  GameOperations.executeOperation(
                    GameOperations.executeOperation(
                      GameOperations.executeOperation(getDummySudoku, 'd').right.get
                      , 'd').right.get
                    , 'r').right.get
                  , 'l').right.get
                , 'l').right.get
              , 'l').right.get
            , '1').right.get
          , 'l').right.get
        , '2').right.get,

      GameOperations.executeSequence(
        getDummySudoku,
        List('d', 'd', 'r', 'l', 'l', 'l', '1', 'l', '2')
      ).right.get
    )
  }

}
