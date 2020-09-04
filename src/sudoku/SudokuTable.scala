package sudoku

import java.io.File
import java.io.PrintWriter

import scala.annotation.tailrec
import scala.io.Source

case class SudokuTable(table: Vector[Vector[Int]], origin: Vector[Vector[Int]], selectedField: (Int, Int)) {

  def this(selectedField: (Int, Int)) {
    this(SudokuTable.defTable(), SudokuTable.defTable(), selectedField)
  }

  def this() {
    this(SudokuTable.DefSelection)
  }

  def setTableValue(pos: (Int, Int), value: Option[Int]): SudokuTable = {
    val newTable: Vector[Array[Int]] = table.map(row => row.toArray)
    newTable(pos._1)(pos._2) = value match {
      case Some(x) => x
      case None => 0
    }
    new SudokuTable(newTable.map(row => row.toVector), origin, selectedField)
  }

  def setOriginValue(pos: (Int, Int), value: Option[Int]): SudokuTable = {
    val newOrigin: Vector[Array[Int]] = origin.map(row => row.toArray)
    newOrigin(pos._1)(pos._2) = value match {
      case Some(x) => x
      case None => 0
    }
    val vedNewOrigin = newOrigin.map(row => row.toVector)
    new SudokuTable(vedNewOrigin, vedNewOrigin, selectedField)
  }

  def setSelectedField(x: Int, y: Int): SudokuTable = SudokuTable(table, origin, (x, y))

  def saveToFile(filePath: String): Boolean = {
    try {
      val pw = new PrintWriter(new File(filePath))
      for (i <- 0 to 8) {
        for (j <- 0 to 8) {
          if (selectedField == (i, j)) pw.print('P')
          else if (origin(i)(j) == 0) pw.print('-')
          else pw.print(origin(i)(j))
        }
        pw.println()
      }
      pw.close()
      true
    } catch { case _: Exception => false }
  }

}

object SudokuTable {

  val Dim: Int = 9
  val DefSelection: (Int, Int) = (0, 0)
  val UnexpectedCharError = "Unexpected field value"
  val InvalidFileError = "Invalid input file"

  def defTable(): Vector[Vector[Int]] = Array.fill(Dim)(Array.fill(Dim)(0).toVector).toVector

  def createFromFile(filePath: String): Either[String, SudokuTable] = {

    @tailrec
    def fieldRec(
                  line: String,
                  table: Array[Array[Int]],
                  selectedField: (Int, Int),
                  i: Int,
                  j: Int
                ): Either[String, (Int, Int)] =
      j match {
        case Dim => Right(selectedField)
        case _ if line(j) == '-' || line(j) == 'P' || (line(j).isDigit && line(j).asDigit != 0) =>
          table(i)(j) = line(j) match {
            case '-' | 'P' => 0
            case x: Char => x.asDigit
          }
          fieldRec(
            line,
            table,
            line(j) match {
              case 'P' => (i, j)
              case _ => selectedField
            },
            i,
            j + 1)
        case _ => Left(UnexpectedCharError)
      }

    def rowRec(
                source: Iterator[String],
                table: Array[Array[Int]],
                selectedField: (Int, Int),
                i: Int
              ): Either[String, (Int, Int)] =
      i match {
        case Dim => Right(selectedField)
        case _ =>
          try {
            fieldRec(source.next(), table, selectedField, i, 0) match {
              case Left(error) => Left(error)
              case Right(selectedField) => rowRec(source, table, selectedField, i + 1)
            }
          } catch { case _: Exception => Left(InvalidFileError) }
      }

    val table: Array[Array[Int]] = Array.ofDim[Int](Dim, Dim)
    val source = Source.fromFile(filePath)

    val res: Either[String, SudokuTable] = rowRec(source.getLines, table, DefSelection, 0) match {
      case Left(error) => Left(error)
      case Right(selectedField) =>
        val vecTable = table.map(row => row.toVector).toVector
        Right(SudokuTable(vecTable, vecTable, selectedField))
    }
    source.close()
    res
  }

}
