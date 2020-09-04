package sudoku

import collection.mutable
import scala.annotation.tailrec

object Solver {

  val TableContainsErrors = "Table contains errors"
  val NoUniqueStepFound = "No unique step found"

  case class StepResult(succeeded: Boolean, sudoku: SudokuTable, steps: List[Char])
  case class SolverState(availableValues: Array[Array[mutable.Set[Int]]],
                         missingHor: Array[mutable.Set[Int]],
                         missingVert: Array[mutable.Set[Int]],
                         missingBox: Array[Array[mutable.Set[Int]]]) {

    def update(x: Int, y: Int, fVal: Int): Unit = {

      // Remove field value from available values of its "neighbors"
      for (i <- 0 to 8) {
        availableValues(x)(i).remove(fVal)
        availableValues(i)(y).remove(fVal)
        availableValues(x / 3 * 3 + i / 3)(y / 3 * 3 + i % 3).remove(fVal)
      }

      // Only available value for the specific field
      availableValues(x)(y).clear()
      availableValues(x)(y).add(fVal)

      // Remove missing value for field groups current field belongs to
      missingHor(x).remove(fVal)
      missingVert(y).remove(fVal)
      missingBox(x / 3)(y / 3).remove(fVal)
    }

  }

  val DefPrevFound: Int = -1

  def fillInTheField(sudoku: SudokuTable, steps: List[Char], x: Int, y: Int, state: SolverState): StepResult = {

    case class SteppingResult(sudoku: SudokuTable, steps: List[Char])

    @tailrec
    def stepAlongY(sudoku: SudokuTable, steps: List[Char], x: Int, y: Int): SteppingResult = {
      sudoku.selectedField._2 match {
        case s if s < y => stepAlongY(GameOperations.moveSelectionRight(sudoku), steps :+ 'r', x, y)
        case s if s > y => stepAlongY(GameOperations.moveSelectionLeft(sudoku), steps :+ 'l', x, y)
        case s if s == y => SteppingResult(sudoku, steps)
      }
    }

    @tailrec
    def stepAlongX(sudoku: SudokuTable, steps: List[Char], x: Int, y: Int): SteppingResult =
      sudoku.selectedField._1 match {
        case s if s < x => stepAlongX(GameOperations.moveSelectionDown(sudoku), steps :+ 'd', x , y)
        case s if s > x => stepAlongX(GameOperations.moveSelectionUp(sudoku), steps :+ 'u', x, y)
        case s if s == x => stepAlongY(sudoku, steps, x, y)
      }

    val newVal: Int = state.availableValues(x)(y).toVector(0)

    val steppingResult = stepAlongX(sudoku, steps, x, y)

    val newSudoku = steppingResult.sudoku.setTableValue((x, y), Some(newVal))

    // update solver state
    state.update(x, y, newVal)

    StepResult(succeeded = true, newSudoku, steppingResult.steps :+ newVal.toString.charAt(0))
  }

  def checkOnlyPossibilitiesBox(sudoku: SudokuTable, steps: List[Char], x: Int, y: Int, state: SolverState): StepResult = {

    @tailrec
    def checkMissing(sudoku: SudokuTable, steps: List[Char], x: Int, y: Int, it: Iterator[Int]): StepResult = {

      @tailrec
      def checkSingleOption(sudoku: SudokuTable, x: Int, y: Int, ind: Int, fVal: Int, prevFound: Int): Int = {

        if (ind == 9) prevFound
        else {
          if (sudoku.table(x + ind / 3)(y + ind % 3) != fVal) checkSingleOption(sudoku, x, y, ind + 1, fVal, prevFound)
          else {
            if (prevFound == DefPrevFound) checkSingleOption(sudoku, x, y, ind + 1, fVal, ind)
            else DefPrevFound
          }
        }
      }

      if (it.isEmpty) {
        if (y < 6) checkOnlyPossibilitiesBox(sudoku, steps, x, y + 3, state)
        else if (x < 6) checkOnlyPossibilitiesBox(sudoku, steps, x + 3, 0, state)
        else StepResult(succeeded = false, sudoku, steps)
      } else checkSingleOption(sudoku, x, y, 0, it.next(), DefPrevFound) match {
        case DefPrevFound => checkMissing(sudoku, steps, x, y, it)
        case ind: Int => fillInTheField(sudoku, steps, x + ind / 3, y + ind % 3, state)
      }

    }

    checkMissing(sudoku, steps, x, y, state.missingBox(x / 3)(y / 3).iterator)
  }

  def checkOnlyPossibilitiesVert(sudoku: SudokuTable, steps: List[Char], y: Int, state: SolverState): StepResult = {

    @tailrec
    def checkMissing(sudoku: SudokuTable, steps: List[Char], y: Int, it: Iterator[Int]): StepResult = {

      @tailrec
      def checkSingleOption(sudoku: SudokuTable, x: Int, y: Int, fVal: Int, prevFound: Int): Int = {

        if (x == 9) prevFound
        else {
          if (sudoku.table(x)(y) != fVal) checkSingleOption(sudoku, x + 1, y, fVal, prevFound)
          else {
            if (prevFound == DefPrevFound) checkSingleOption(sudoku, x + 1, y, fVal, y)
            else DefPrevFound
          }
        }
      }

      if (it.isEmpty) {
        if (y < 8) checkOnlyPossibilitiesVert(sudoku, steps, y + 1, state)
        else checkOnlyPossibilitiesBox(sudoku, steps, 0, 0, state)
      } else checkSingleOption(sudoku, 0, y, it.next(), DefPrevFound) match {
        case DefPrevFound => checkMissing(sudoku, steps, y, it)
        case x: Int => fillInTheField(sudoku, steps, x, y, state)
      }

    }

    checkMissing(sudoku, steps, y, state.missingVert(y).iterator)
  }

  def checkOnlyPossibilitiesHor(sudoku: SudokuTable, steps: List[Char], x: Int, state: SolverState): StepResult = {

    @tailrec
    def checkMissing(sudoku: SudokuTable, steps: List[Char], x: Int, it: Iterator[Int]): StepResult = {

      @tailrec
      def checkSingleOption(sudoku: SudokuTable, x: Int, y: Int, fVal: Int, prevFound: Int): Int = {

        if (y == 9) prevFound
        else {
          if (sudoku.table(x)(y) != fVal) checkSingleOption(sudoku, x, y + 1, fVal, prevFound)
          else {
            if (prevFound == DefPrevFound) checkSingleOption(sudoku, x, y + 1, fVal, y)
            else DefPrevFound
          }
        }
      }

      if (it.isEmpty) {
        if (x < 8) checkOnlyPossibilitiesHor(sudoku, steps, x + 1, state)
        else checkOnlyPossibilitiesVert(sudoku, steps, 0, state)
      } else checkSingleOption(sudoku, x, 0, it.next(), DefPrevFound) match {
        case DefPrevFound => checkMissing(sudoku, steps, x, it)
        case y: Int => fillInTheField(sudoku, steps, x, y, state)
      }

    }

    checkMissing(sudoku, steps, x, state.missingHor(x).iterator)
  }

  def checkIndividualFields(sudoku: SudokuTable, steps: List[Char], x: Int, y: Int, state: SolverState): StepResult = {

    def next(): StepResult = {
      if (y < 8) checkIndividualFields(sudoku, steps, x, y + 1, state)
      else if (x < 8) checkIndividualFields(sudoku, steps, x + 1, 0, state)
      else checkOnlyPossibilitiesHor(sudoku, steps, 0, state)
    }

    if ((x == 8 && y == 8) || state.availableValues(x)(y).size > 1 || sudoku.table(x)(y) != 0) next()
    else fillInTheField(sudoku, steps, x, y, state)
  }

  @tailrec
  def executeStep(sudoku: SudokuTable, steps: List[Char], state: SolverState
                 ): Either[String, (SudokuTable, List[Char])] = {
    val status = GameOperations.checkGameEnd(sudoku)
    if (status.anyErrors) Left(TableContainsErrors)
    else if (status.isFilled) Right((sudoku, steps))
    else {
      val stepResult: StepResult = checkIndividualFields(sudoku, steps, 0, 0, state)
      if (stepResult.succeeded) executeStep(stepResult.sudoku, stepResult.steps, state)
      else Left(NoUniqueStepFound)
    }
  }

  def solve(sudoku: SudokuTable): Either[String, (SudokuTable, List[Char])] = {

    val solverState = SolverState(
      Array.fill(9)(Array.fill(9)(mutable.Set(1 to 9 :_*))),
      Array.fill(9)(mutable.Set(1 to 9 :_*)),
      Array.fill(9)(mutable.Set(1 to 9 :_*)),
      Array.fill(3)(Array.fill(3)(mutable.Set(1 to 9 :_*)))
    )

    for (i <- 0 to 8; j <- 0 to 8; fVal = sudoku.origin(i)(j); if fVal != 0) solverState.update(i, j, fVal)

    executeStep(sudoku, List(), solverState)

  }

}
