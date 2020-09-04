package sudoku

trait CreatorOperation extends Seq[CreatorOperation] {
  def execute(sudoku: SudokuTable): Either[String, SudokuTable]
  def insert(creatorOperation: CreatorOperation): CreatorOperation
}

class COIterator(initialCO: CreatorOperation) extends Iterator[CreatorOperation] {
  var currCO: CreatorOperation = initialCO
  override def hasNext: Boolean = 
    currCO.isInstanceOf[NoNilOperation] && currCO.asInstanceOf[NoNilOperation].nextCO.isInstanceOf[NoNilOperation]
  override def next(): CreatorOperation = {
    val res = currCO
    currCO = currCO.asInstanceOf[NoNilOperation].nextCO
    res
  }
}

object NilOperation extends CreatorOperation {
  override def execute(sudoku: SudokuTable): Either[String, SudokuTable] = Right(sudoku)
  override def insert(creatorOperation: CreatorOperation): CreatorOperation = creatorOperation
  override def toString: String = "Nil"

  override def length: Int = 0
  override def apply(idx: Int): CreatorOperation = throw new IndexOutOfBoundsException
  override def iterator: Iterator[CreatorOperation] = new COIterator(this)
}

abstract class NoNilOperation(val nextCO: CreatorOperation) extends CreatorOperation {
  def execute(sudoku: SudokuTable): Either[String, SudokuTable]
  def propagate(either: Either[String, SudokuTable]): Either[String, SudokuTable] = either match {
    case Left(error) => Left(error)
    case Right(sudoku) => nextCO.execute(sudoku)
  }

  override def length: Int = 1 + nextCO.length
  override def apply(idx: Int): CreatorOperation = idx match {
    case 0 => this
    case _ => nextCO.apply(idx - 1)
  }
  override def iterator: Iterator[CreatorOperation] = new COIterator(this)
}

class SetFieldOp(val x: Int, val y: Int, val value: Option[Int], nextCO: CreatorOperation) extends NoNilOperation(nextCO) {
  override def execute(sudoku: SudokuTable): Either[String, SudokuTable] =
    propagate(GameCreator.setField(sudoku, x, y, value))
  override def insert(creatorOperation: CreatorOperation): CreatorOperation =
    new SetFieldOp(x, y, value, nextCO.insert(creatorOperation))
  override def toString: String = "Set field: x = " + x.toString + ", y = " + y.toString + ", val = " + value.toString
}

class SetStartingFieldOp(val x: Int, val y: Int, nextCO: CreatorOperation) extends NoNilOperation(nextCO) {
  override def execute(sudoku: SudokuTable): Either[String, SudokuTable] =
    propagate(GameCreator.setStartingField(sudoku, x, y))
  override def insert(creatorOperation: CreatorOperation): CreatorOperation =
    new SetStartingFieldOp(x, y, nextCO.insert(creatorOperation))
  override def toString: String = "Set starting pos: x = " + x.toString + ", y = " + y.toString
}

class TransposeOp(nextCO: CreatorOperation) extends NoNilOperation(nextCO) {
  override def execute(sudoku: SudokuTable): Either[String, SudokuTable] =
    propagate(Right(GameCreator.transpose(sudoku)))
  override def insert(creatorOperation: CreatorOperation): CreatorOperation =
    new TransposeOp(nextCO.insert(creatorOperation))
  override def toString: String = "Transpose"
}

class InvertNumbersOp(nextCO: CreatorOperation) extends NoNilOperation(nextCO) {
  override def execute(sudoku: SudokuTable): Either[String, SudokuTable] =
    propagate(Right(GameCreator.invertNumbers(sudoku)))
  override def insert(creatorOperation: CreatorOperation): CreatorOperation =
    new InvertNumbersOp(nextCO.insert(creatorOperation))
  override def toString: String = "Invert numbers"
}

class FilterLinesOp(val x: Int, val y: Int, nextCO: CreatorOperation) extends NoNilOperation(nextCO) {
  override def execute(sudoku: SudokuTable): Either[String, SudokuTable] =
    propagate(GameCreator.filterLines(sudoku, x, y))
  override def insert(creatorOperation: CreatorOperation): CreatorOperation =
    new FilterLinesOp(x, y, nextCO.insert(creatorOperation))
  override def toString: String = "Filter lines: x = " + x.toString + ", y = " + y.toString
}

class FilterBoxOp(val x: Int, val y: Int, nextCO: CreatorOperation) extends NoNilOperation(nextCO) {
  override def execute(sudoku: SudokuTable): Either[String, SudokuTable] =
    propagate(GameCreator.filterBox(sudoku, x, y))
  override def insert(creatorOperation: CreatorOperation): CreatorOperation =
    new FilterBoxOp(x, y, nextCO.insert(creatorOperation))
  override def toString: String = "Filter box: x = " + x.toString + ", y = " + y.toString
}

class CompositeOperationOp(val branch: CreatorOperation, val name: String, nextCO: CreatorOperation) extends NoNilOperation(nextCO) {
  override def execute(sudoku: SudokuTable): Either[String, SudokuTable] = propagate(branch.execute(sudoku))
  override def insert(creatorOperation: CreatorOperation): CreatorOperation =
    new CompositeOperationOp(branch, name, nextCO.insert(creatorOperation))
  override def toString: String = "Composite: " + name
}
