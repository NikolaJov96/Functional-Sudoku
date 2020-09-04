package sudoku

import java.io.{File, PrintWriter}

import scala.annotation.tailrec
import scala.io.Source

object GameOperations {

  case class GameFinishedState(isFilled: Boolean, anyErrors: Boolean)

  val OriginOverwriteError = "Cannot write over the original value"
  val UnrecognizedOperationError = "Operation not recognized"

  def moveSelectionUp(sudoku: SudokuTable): SudokuTable = {
    SudokuTable(
      sudoku.table,
      sudoku.origin,
      (
        sudoku.selectedField._1 match {
          case x if x <= 0 => 0
          case x => x - 1
        },
        sudoku.selectedField._2
      )
    )
  }

  def moveSelectionDown(sudoku: SudokuTable): SudokuTable = {
    SudokuTable(
      sudoku.table,
      sudoku.origin,
      (
        sudoku.selectedField._1 match {
          case x if x >= SudokuTable.Dim - 1 => SudokuTable.Dim - 1
          case x => x + 1
        },
        sudoku.selectedField._2
      )
    )
  }

  def moveSelectionLeft(sudoku: SudokuTable): SudokuTable = {
    SudokuTable(
      sudoku.table,
      sudoku.origin,
      (
        sudoku.selectedField._1,
        sudoku.selectedField._2 match {
          case x if x <= 0 => 0
          case x => x - 1
        }
      )
    )
  }

  def moveSelectionRight(sudoku: SudokuTable): SudokuTable = {
    SudokuTable(
      sudoku.table,
      sudoku.origin,
      (
        sudoku.selectedField._1,
        sudoku.selectedField._2 match {
          case x if x >= SudokuTable.Dim - 1 => SudokuTable.Dim - 1
          case x => x + 1
        }
      )
    )
  }

  def writeTheValue(sudoku: SudokuTable, value: Option[Int]): Either[String, SudokuTable] = {
    val x = sudoku.selectedField._1
    val y = sudoku.selectedField._2
    if (sudoku.origin(x)(y) != 0) Left(OriginOverwriteError)
    else Right(sudoku.setTableValue(sudoku.selectedField, value))
  }

  def executeOperation(sudoku: SudokuTable, op: Char): Either[String, SudokuTable] = op match {
    case 'd' => Right(moveSelectionDown(sudoku))
    case 'u' => Right(moveSelectionUp(sudoku))
    case 'l' => Right(moveSelectionLeft(sudoku))
    case 'r' => Right(moveSelectionRight(sudoku))
    case c: Char if c.isDigit && c.asDigit != 0 =>
      Right(sudoku.setTableValue(sudoku.selectedField, Option(c.asDigit)))
    case _ => Left(UnrecognizedOperationError)
  }

  @tailrec
  def executeSequence(sudoku: SudokuTable, seq: List[Char]): Either[String, SudokuTable] = {
    if (seq.isEmpty) Right(sudoku)
    else executeOperation(sudoku, seq.head) match {
      case Right(newSudoku) => executeSequence(newSudoku, seq.tail)
      case Left(error) => Left(error)
    }
  }

  def loadSequence(filePath: String): Option[List[Char]] = {
    val source = Source.fromFile(filePath)
    val seq: Option[List[Char]] =
      try { Some(source.getLines().toList.map({ row => row.charAt(0) })) }
      catch { case _: Exception => None }
    source.close()
    seq
  }

  def saveSequence(filePath: String, seq: List[Char]): Boolean =
    try {
      val pw = new PrintWriter(new File(filePath))
      seq.foreach({ op => pw.println(op) })
      pw.close()
      true
    } catch { case e: Exception => false }

  def checkGameEnd(sudoku: SudokuTable): GameFinishedState = {

    @tailrec
    def checkBox(sudoku: SudokuTable, x: Int, y: Int): Boolean = {
      val res = (for (row1 <- x to x + 2;
                      col1 <- y to y + 2;
                      row2 <- x to x + 2;
                      col2 <- y to y + 2
                      if (!(row1 == row2 && col1 == col2)
                        && sudoku.table(row1)(col1) != 0
                        && sudoku.table(row2)(col2) != 0)
                      ) yield (row1, col1, row2, col2)) match {
        case IndexedSeq() => false
        case seq: IndexedSeq[(Int, Int, Int, Int)] =>
          seq.exists({ cords => sudoku.table(cords._1)(cords._2) == sudoku.table(cords._3)(cords._4) })
      }
      if (!res) {
        if (x < 6) checkBox(sudoku, x + 3, y)
        else if (y < 6) checkBox(sudoku, 0, y + 3)
        else res
      }
      else res
    }

    @tailrec
    def checkHorizontal(sudoku: SudokuTable, x: Int): Boolean = {
      val res = (for (col1 <- 0 to 7;
                      col2 <- col1 + 1 to 8
                      if (sudoku.table(x)(col1) != 0
                        && sudoku.table(x)(col2) != 0)
                      ) yield (col1, col2)) match {
        case IndexedSeq() => false
        case seq: IndexedSeq[(Int, Int)] =>
          seq.exists({ cols => sudoku.table(x)(cols._1) == sudoku.table(x)(cols._2) })
      }
      if (!res) {
        if (x < 8) checkHorizontal(sudoku, x + 1)
        else checkBox(sudoku, 0, 0)
      }
      else res
    }

    @tailrec
    def checkVertical(sudoku: SudokuTable, y: Int): Boolean = {
      val res = (for (row1 <- 0 to 7;
                      row2 <- row1 + 1 to 8
                      if (sudoku.table(row1)(y) != 0
                        && sudoku.table(row2)(y) != 0)
                      ) yield (row1, row2)) match {
        case IndexedSeq() => false
        case seq: IndexedSeq[(Int, Int)] =>
          seq.exists({ rows => sudoku.table(rows._1)(y) == sudoku.table(rows._2)(y) })
      }
      if (!res) {
        if (y < 8) checkVertical(sudoku, y + 1)
        else checkHorizontal(sudoku, 0)
      }
      else res
    }

    GameFinishedState(
      sudoku.table.forall({ row => row.forall({ num => num > 0 }) }),
      checkVertical(sudoku, 0)
    )

  }

}
