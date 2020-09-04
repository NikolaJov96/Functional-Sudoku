package sudoku

object GameCreator {

  val OutOfBoundsError = "Index out of bounds"
  val InvalidValueError = "Field value out of scope"

  def setField(sudoku: SudokuTable, x: Int, y: Int, value: Option[Int]): Either[String, SudokuTable] = {
    if (x < 0 || x >= SudokuTable.Dim || y < 0 || y >= SudokuTable.Dim) Left(OutOfBoundsError)
    else if (value.isDefined && (value.get < 1 || value.get > 9)) Left(InvalidValueError)
    else Right(sudoku.setTableValue((x, y), value).setOriginValue((x, y), value))
  }

  def setStartingField(sudoku: SudokuTable, x: Int, y: Int): Either[String, SudokuTable] = {
    if (sudoku.selectedField == (x, y)) Right(sudoku)
    else if (x < 0 || x >= SudokuTable.Dim || y < 0 || y >= SudokuTable.Dim) Left(OutOfBoundsError)
    else {
      val table: Vector[Array[Int]] = sudoku.table.map({ row => row.toArray })
      val oldX = sudoku.selectedField._1
      val oldY = sudoku.selectedField._2
      table(oldX)(oldY) = 0
      val vecTable = table.map({ row => row.toVector })
      Right(SudokuTable(vecTable, vecTable, (x, y)))
    }
  }

  def transpose(sudoku: SudokuTable): SudokuTable = {
    val table: Vector[Array[Int]] = sudoku.origin.map({ row => row.toArray })
    for (i <- 1 to 8; j <- 0 until i) {
      val temp = table(i)(j)
      table(i)(j) = table(j)(i)
      table(j)(i) = temp
    }
    val vecTable = table.map({ row => row.toVector })
    val newSelected = (sudoku.selectedField._2, sudoku.selectedField._1)
    SudokuTable(vecTable, vecTable, newSelected)
  }

  def invertNumbers(sudoku: SudokuTable): SudokuTable = {
    val table: Vector[Array[Int]] = sudoku.origin.map({ row => row.toArray })
    for (i <- 0 to 8; j <- 0 to 8) if (table(i)(j) != 0) table(i)(j) = 10 - table(i)(j)
    val vecTable = table.map({ row => row.toVector })
    SudokuTable(vecTable, vecTable, sudoku.selectedField)
  }

  def filterLines(sudoku: SudokuTable, x: Int, y: Int): Either[String, SudokuTable] = {
    if (x < 0 || x >= SudokuTable.Dim || y < 0 || y >= SudokuTable.Dim) Left(OutOfBoundsError)
    else {
      val selNum = sudoku.origin(x)(y)
      if (selNum == 0) Right(sudoku)
      else {
        val table: Vector[Array[Int]] = sudoku.origin.map({ row => row.toArray })
        for (i <- 0 to 8) {
          if (table(x)(i) == selNum) table(x)(i) = 0
          if (table(i)(y) == selNum) table(i)(y) = 0
        }
        val vecTable = table.map({ row => row.toVector })
        Right(SudokuTable(vecTable, vecTable, sudoku.selectedField))
      }
    }
  }

  def filterBox(sudoku: SudokuTable, x: Int, y: Int): Either[String, SudokuTable] = {
    if (x < 0 || x >= SudokuTable.Dim || y < 0 || y >= SudokuTable.Dim) Left(OutOfBoundsError)
    else {
      val selNum = sudoku.origin(x)(y)
      if (selNum == 0) Right(sudoku)
      else {
        val stX = x / 3 * 3
        val stY = y / 3 * 3
        val table: Vector[Array[Int]] = sudoku.origin.map({ row => row.toArray })
        for (i <- stX to stX + 2; j <- stY to stY + 2 if table(i)(j) == selNum) table(i)(j) = 0
        val vecTable = table.map({ row => row.toVector })
        Right(SudokuTable(vecTable, vecTable, sudoku.selectedField))
      }
    }
  }

}
